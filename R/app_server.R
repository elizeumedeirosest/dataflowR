#' @title Servidor Reativo do dataflowR
#' @description Lógica do servidor, gerenciamento da pilha de operações,
#'   sincronização da planilha e tratamento dos modais de engenharia de dados.
#' @name app_server
#' @keywords internal
NULL

#' Criar Servidor da Aplicação dataflow
#'
#' @param dados_iniciais Data frame passado pelo usuário.
#' @param nome_objeto_original Nome da variável passada no console.
#' @return Função server do Shiny.
#' @keywords internal
criar_server_dataflow <- function(dados_iniciais, nome_objeto_original = "dados") {
  function(input, output, session) {

    # Estado reativo
    valores <- shiny::reactiveValues(
      dados = as.data.frame(dados_iniciais),
      historico_dados = list(as.data.frame(dados_iniciais)),
      operacoes = list(),
      dim_original = list(linhas = nrow(dados_iniciais), colunas = ncol(dados_iniciais))
    )

    # 1. Renderização das Métricas no Cabeçalho
    output$metricas_topo <- shiny::renderUI({
      d <- valores$dados
      n_na <- sum(is.na(d))
      n_ops <- length(valores$operacoes)

      htmltools::tagList(
        htmltools::tags$span(class = "metric-badge", "Linhas: ", htmltools::tags$span(class = "metric-val", format(nrow(d), big.mark = "."))),
        htmltools::tags$span(class = "metric-badge", "Colunas: ", htmltools::tags$span(class = "metric-val", ncol(d))),
        htmltools::tags$span(class = "metric-badge", "Ausentes (NA): ", htmltools::tags$span(class = if (n_na > 0) "text-danger fw-bold" else "text-success fw-bold", n_na)),
        htmltools::tags$span(class = "metric-badge", "Etapas: ", htmltools::tags$span(class = "metric-val", n_ops))
      )
    })

    # 2. Renderização da Planilha rhandsontable
    output$tabela_dados <- rhandsontable::renderRHandsontable({
      criar_tabela_interativa(valores$dados, altura = 550)
    })

    # Captura edições diretas na tabela se o usuário clicar em Concluir ou alterar células
    shiny::observe({
      if (!is.null(input$tabela_dados)) {
        df_editado <- rhandsontable::hot_to_r(input$tabela_dados)
        if (!is.null(df_editado) && nrow(df_editado) > 0) {
          # Compara se houve edição pontual de células
          dif <- detectar_edicoes_celulas(valores$dados, df_editado)
          if (!is.null(dif) && dif$total_modificacoes > 0) {
            # Registra no histórico como edição manual
            op_manual <- nova_operacao(
              tipo = "edicao",
              titulo = paste0("Edição Manual de ", dif$total_modificacoes, " célula(s)"),
              comentario = paste0("Alterações diretas na planilha: ", dif$resumo),
              codigo_dplyr = "# [Edição direta de células na planilha interativa]",
              detalhes = list(
                modificacoes = dif$total_modificacoes,
                linhas_antes = nrow(valores$dados),
                linhas_depois = nrow(df_editado)
              )
            )
            valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
            valores$operacoes <- c(valores$operacoes, list(op_manual))
            valores$dados <- df_editado
          }
        }
      }
    })

    # 3. Desfazer Última Operação
    shiny::observeEvent(input$btn_desfazer, {
      if (length(valores$historico_dados) > 1) {
        valores$dados <- valores$historico_dados[[length(valores$historico_dados)]]
        valores$historico_dados <- utils::head(valores$historico_dados, -1)
        if (length(valores$operacoes) > 0) {
          valores$operacoes <- utils::head(valores$operacoes, -1)
        }
        shiny::showNotification("Última operação desfeita com sucesso.", type = "warning", duration = 3)
      } else {
        shiny::showNotification("Nenhuma operação anterior para desfazer.", type = "default", duration = 2)
      }
    })

    # =========================================================================
    # MODAL 1: TRATAR DADOS AUSENTES (NA)
    # =========================================================================
    shiny::observeEvent(input$btn_modal_na, {
      cols <- names(valores$dados)
      cols_num <- names(valores$dados)[vapply(valores$dados, is.numeric, logical(1))]
      cols_cat <- names(valores$dados)[vapply(valores$dados, function(x) is.factor(x) || is.character(x), logical(1))]

      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("bandage", class = "text-primary me-2"), "Tratamento de Dados Ausentes (NA)"),
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("aplicar_na", "Aplicar Tratamento", class = "btn btn-primary")
        ),
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Remoção",
            htmltools::tags$br(),
            shiny::radioButtons(
              "na_tipo_remocao",
              "Selecione a regra de remoção:",
              choices = c(
                "Remover linhas com NA em qualquer coluna" = "linhas_todas",
                "Remover linhas com NA em colunas específicas" = "linhas_especificas",
                "Remover colunas vazias ou com alto % de NA" = "colunas_limiar"
              ),
              selected = "linhas_todas"
            ),
            shiny::conditionalPanel(
              condition = "input.na_tipo_remocao == 'linhas_especificas'",
              shiny::selectizeInput("na_cols_remover", "Escolha a(s) coluna(s):", choices = cols, multiple = TRUE)
            ),
            shiny::conditionalPanel(
              condition = "input.na_tipo_remocao == 'colunas_limiar'",
              shiny::sliderInput("na_limiar_pct", "Remover colunas com % de NA igual ou superior a:", min = 10, max = 100, value = 100, step = 5, post = "%")
            )
          ),
          shiny::tabPanel(
            "Imputação Simples",
            htmltools::tags$br(),
            shiny::selectInput("na_col_imputar", "Coluna a ser imputada:", choices = cols),
            shiny::selectInput(
              "na_metodo_imputar",
              "Método de imputação:",
              choices = c(
                "Média (para numéricas)" = "media",
                "Mediana (robusta a outliers)" = "mediana",
                "Moda (valor mais frequente)" = "moda",
                "Preencher com Zero (0)" = "zero",
                "Rotular como 'Não Informado'" = "nao_informado",
                "Valor Fixo Constante" = "constante"
              ),
              selected = "mediana"
            ),
            shiny::conditionalPanel(
              condition = "input.na_metodo_imputar == 'constante'",
              shiny::textInput("na_valor_constante", "Valor constante:", value = "")
            )
          ),
          shiny::tabPanel(
            "Imputação por Grupo",
            htmltools::tags$br(),
            htmltools::tags$p(class = "text-muted small", "Preenche os valores ausentes de uma variável numérica calculando a média ou mediana dentro de cada categoria (ex.: salário por cargo)."),
            shiny::selectInput("na_col_alvo_grupo", "Coluna numérica com NAs:", choices = cols_num),
            shiny::selectInput("na_col_agrupamento", "Coluna categórica de agrupamento:", choices = c(cols_cat, cols)),
            shiny::radioButtons("na_metodo_grupo", "Métrica de imputação do grupo:", choices = c("Mediana" = "mediana", "Média" = "media"), inline = TRUE)
          )
        )
      ))
    })

    shiny::observeEvent(input$aplicar_na, {
      shiny::removeModal()
      # Verifica qual aba/opção foi escolhida
      res <- NULL
      # 1. Remoção
      if (!is.null(input$na_tipo_remocao)) {
        if (input$na_tipo_remocao == "linhas_todas") {
          res <- remover_linhas_na(valores$dados, NULL)
        } else if (input$na_tipo_remocao == "linhas_especificas" && length(input$na_cols_remover) > 0) {
          res <- remover_linhas_na(valores$dados, input$na_cols_remover)
        } else if (input$na_tipo_remocao == "colunas_limiar") {
          res <- remover_colunas_na(valores$dados, input$na_limiar_pct)
        }
      }

      # Se foi imputação simples
      if (is.null(res) && !is.null(input$na_col_imputar)) {
        res <- imputar_univariado(valores$dados, input$na_col_imputar, input$na_metodo_imputar, input$na_valor_constante)
      }

      # Se foi imputação por grupo
      if (is.null(res) && !is.null(input$na_col_alvo_grupo) && !is.null(input$na_col_agrupamento)) {
        res <- imputar_por_grupo(valores$dados, input$na_col_alvo_grupo, input$na_col_agrupamento, input$na_metodo_grupo)
      }

      if (!is.null(res) && !is.null(res$operacao)) {
        valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
        valores$dados <- res$dados
        valores$operacoes <- c(valores$operacoes, list(res$operacao))
        shiny::showNotification(paste0("Sucesso: ", res$operacao$titulo), type = "message")
      }
    })

    # =========================================================================
    # MODAL 2: FRAGMENTAR DATAS
    # =========================================================================
    shiny::observeEvent(input$btn_modal_datas, {
      cols <- names(valores$dados)
      # Tenta sugerir colunas que pareçam data
      col_sugerida <- NULL
      for (c_nome in cols) {
        if (detectar_tipo_data(valores$dados[[c_nome]])$eh_data) {
          col_sugerida <- c_nome
          break
        }
      }

      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("calendar-days", class = "text-primary me-2"), "Fragmentador Inteligente de Datas"),
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("aplicar_datas", "Extrair Componentes", class = "btn btn-primary")
        ),
        shiny::selectInput("datas_coluna", "Selecione a coluna de data:", choices = cols, selected = col_sugerida),
        shiny::checkboxGroupInput(
          "datas_componentes",
          "Selecione os atributos temporais a extrair:",
          choices = c(
            "Ano (ex.: 2026)" = "ano",
            "Mês numérico (1 a 12)" = "mes",
            "Nome do Mês (ex.: 'Outubro')" = "mes_nome",
            "Dia do mês (1 a 31)" = "dia",
            "Dia da Semana (ex.: 'Segunda')" = "dia_semana",
            "Trimestre (T1, T2, T3, T4)" = "trimestre",
            "Semestre (1 ou 2)" = "semestre",
            "Flag Fim de Semana (TRUE/FALSE)" = "fim_de_semana",
            "Hora (0 a 23)" = "hora",
            "Minuto (0 a 59)" = "minuto"
          ),
          selected = c("ano", "mes", "dia", "dia_semana", "fim_de_semana")
        ),
        shiny::textInput("datas_prefixo", "Prefixo opcional para as novas colunas:", placeholder = "Padrão: nome_da_coluna_")
      ))
    })

    shiny::observeEvent(input$aplicar_datas, {
      shiny::removeModal()
      if (!is.null(input$datas_coluna) && length(input$datas_componentes) > 0) {
        res <- fragmentar_data(
          valores$dados,
          input$datas_coluna,
          input$datas_componentes,
          input$datas_prefixo
        )
        if (!is.null(res) && !is.null(res$operacao)) {
          valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
          valores$dados <- res$dados
          valores$operacoes <- c(valores$operacoes, list(res$operacao))
          shiny::showNotification(paste0("Sucesso: ", res$operacao$titulo), type = "message")
        }
      }
    })

    # =========================================================================
    # MODAL 3: FILTRAR LINHAS
    # =========================================================================
    shiny::observeEvent(input$btn_modal_filtro, {
      cols <- names(valores$dados)
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("filter", class = "text-primary me-2"), "Filtrar Linhas por Condição"),
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("aplicar_filtro", "Aplicar Filtro", class = "btn btn-primary")
        ),
        shiny::selectInput("filtro_coluna", "Coluna a ser avaliada:", choices = cols),
        shiny::selectInput(
          "filtro_operador",
          "Operador lógico:",
          choices = c(
            "Igual a (==)" = "==",
            "Diferente de (!=)" = "!=",
            "Maior que (>)" = ">",
            "Maior ou igual (>=)" = ">=",
            "Menor que (<)" = "<",
            "Menor ou igual (<=)" = "<=",
            "Entre faixa (valor 1 e 2)" = "entre",
            "Remover NAs nesta coluna" = "!is.na",
            "Manter somente NAs" = "is.na"
          ),
          selected = ">"
        ),
        shiny::conditionalPanel(
          condition = "input.filtro_operador != 'is.na' && input.filtro_operador != '!is.na'",
          shiny::textInput("filtro_valor", "Valor de corte / referência:", value = "")
        ),
        shiny::conditionalPanel(
          condition = "input.filtro_operador == 'entre'",
          shiny::textInput("filtro_valor2", "Segundo valor da faixa:", value = "")
        )
      ))
    })

    shiny::observeEvent(input$aplicar_filtro, {
      shiny::removeModal()
      if (!is.null(input$filtro_coluna) && !is.null(input$filtro_operador)) {
        res <- filtrar_por_condicao(
          valores$dados,
          input$filtro_coluna,
          input$filtro_operador,
          input$filtro_valor,
          input$filtro_valor2
        )
        if (!is.null(res) && !is.null(res$operacao)) {
          valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
          valores$dados <- res$dados
          valores$operacoes <- c(valores$operacoes, list(res$operacao))
          shiny::showNotification(paste0("Sucesso: ", res$operacao$titulo), type = "message")
        }
      }
    })

    # =========================================================================
    # MODAL 4: SUBSTITUIR VALORES SENTINELA
    # =========================================================================
    shiny::observeEvent(input$btn_modal_sentinela, {
      cols <- names(valores$dados)
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("arrows-rotate", class = "text-primary me-2"), "Substituir Valores Sentinela"),
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("aplicar_sentinela", "Substituir Valores", class = "btn btn-primary")
        ),
        htmltools::tags$p(class = "text-muted small", "Troque códigos de preenchimento inválidos (ex.: -999, 9999, '?') por NA ou por um valor padrão."),
        shiny::selectInput("sentinela_coluna", "Coluna:", choices = cols),
        shiny::textInput("sentinela_alvo", "Valor atual a substituir (ex.: -999 ou ?):", value = "-999"),
        shiny::radioButtons(
          "sentinela_destino_tipo",
          "Substituir por:",
          choices = c("Valor Ausente (NA)" = "na", "Outro valor customizado" = "custom"),
          selected = "na"
        ),
        shiny::conditionalPanel(
          condition = "input.sentinela_destino_tipo == 'custom'",
          shiny::textInput("sentinela_novo_valor", "Novo valor:", value = "0")
        )
      ))
    })

    shiny::observeEvent(input$aplicar_sentinela, {
      shiny::removeModal()
      if (!is.null(input$sentinela_coluna)) {
        alvo <- input$sentinela_alvo
        novo_val <- if (input$sentinela_destino_tipo == "na") NA else input$sentinela_novo_valor
        # Converte para número se aplicável
        if (is.numeric(valores$dados[[input$sentinela_coluna]])) {
          alvo <- suppressWarnings(as.numeric(alvo))
          if (!is.na(novo_val)) novo_val <- suppressWarnings(as.numeric(novo_val))
        }

        res <- substituir_por_condicao(valores$dados, input$sentinela_coluna, "==", alvo, novo_val)
        if (!is.null(res) && !is.null(res$operacao)) {
          valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
          valores$dados <- res$dados
          valores$operacoes <- c(valores$operacoes, list(res$operacao))
          shiny::showNotification(paste0("Sucesso: ", res$operacao$titulo), type = "message")
        }
      }
    })

    # =========================================================================
    # MODAL 5: CRIAR NOVA COLUNA
    # =========================================================================
    shiny::observeEvent(input$btn_modal_nova_col, {
      cols <- names(valores$dados)
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("plus", class = "text-primary me-2"), "Criar Nova Coluna (Mutate)"),
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("aplicar_nova_col", "Criar Coluna", class = "btn btn-primary")
        ),
        shiny::textInput("nova_col_nome", "Nome da nova coluna:", placeholder = "ex.: imc, razao, log_renda"),
        shiny::textInput("nova_col_expr", "Expressão de cálculo R:", placeholder = "ex.: peso / (altura^2) ou log(mpg)"),
        htmltools::tags$div(
          class = "text-muted small mt-2",
          "Colunas disponíveis: ",
          htmltools::tags$span(class = "fw-bold", paste(cols, collapse = ", "))
        )
      ))
    })

    shiny::observeEvent(input$aplicar_nova_col, {
      shiny::removeModal()
      if (nzchar(input$nova_col_nome) && nzchar(input$nova_col_expr)) {
        tryCatch({
          res <- criar_coluna_expressao(valores$dados, input$nova_col_nome, input$nova_col_expr)
          if (!is.null(res) && !is.null(res$operacao)) {
            valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
            valores$dados <- res$dados
            valores$operacoes <- c(valores$operacoes, list(res$operacao))
            shiny::showNotification(paste0("Sucesso: ", res$operacao$titulo), type = "message")
          }
        }, error = function(e) {
          shiny::showNotification(paste0("Erro ao avaliar expressão: ", e$message), type = "error")
        })
      }
    })

    # =========================================================================
    # MODAL 6: RENOMEAR E TIPAGEM
    # =========================================================================
    shiny::observeEvent(input$btn_modal_renomear, {
      cols <- names(valores$dados)
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("tag", class = "text-primary me-2"), "Renomear ou Alterar Tipo de Coluna"),
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("aplicar_renomear_tipo", "Salvar Alteração", class = "btn btn-primary")
        ),
        shiny::selectInput("renomear_col_alvo", "Selecione a coluna:", choices = cols),
        shiny::textInput("renomear_novo_nome", "Novo nome (deixe igual para manter):", value = cols[1]),
        shiny::selectInput(
          "renomear_novo_tipo",
          "Converter tipo de dado:",
          choices = c(
            "Manter tipo atual" = "manter",
            "Numérico (numeric)" = "numeric",
            "Texto (character)" = "character",
            "Fator / Categórica (factor)" = "factor",
            "Inteiro (integer)" = "integer",
            "Data (Date)" = "Date"
          ),
          selected = "manter"
        )
      ))
    })

    shiny::observeEvent(input$renomear_col_alvo, {
      shiny::updateTextInput(session, "renomear_novo_nome", value = input$renomear_col_alvo)
    })

    shiny::observeEvent(input$aplicar_renomear_tipo, {
      shiny::removeModal()
      col_alvo <- input$renomear_col_alvo
      novo_nome <- input$renomear_novo_nome
      novo_tipo <- input$renomear_novo_tipo

      d_temp <- valores$dados

      # 1. Altera tipo se requisitado
      if (novo_tipo != "manter") {
        res_tipo <- alterar_tipo_coluna(d_temp, col_alvo, novo_tipo)
        if (!is.null(res_tipo$operacao)) {
          valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
          d_temp <- res_tipo$dados
          valores$operacoes <- c(valores$operacoes, list(res_tipo$operacao))
        }
      }

      # 2. Renomeia se nome for diferente
      if (nzchar(novo_nome) && novo_nome != col_alvo) {
        res_renomear <- renomear_coluna(d_temp, col_alvo, novo_nome)
        if (!is.null(res_renomear$operacao)) {
          valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
          d_temp <- res_renomear$dados
          valores$operacoes <- c(valores$operacoes, list(res_renomear$operacao))
        }
      }

      valores$dados <- d_temp
      shiny::showNotification("Coluna atualizada com sucesso.", type = "message")
    })

    # =========================================================================
    # MODAL 7: EXCLUIR COLUNAS
    # =========================================================================
    shiny::observeEvent(input$btn_modal_excluir_col, {
      cols <- names(valores$dados)
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("trash-can", class = "text-danger me-2"), "Excluir Colunas"),
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("aplicar_excluir_col", "Excluir Selecionadas", class = "btn btn-danger")
        ),
        shiny::selectizeInput("excluir_cols_selecionadas", "Selecione a(s) coluna(s) a remover:", choices = cols, multiple = TRUE)
      ))
    })

    shiny::observeEvent(input$aplicar_excluir_col, {
      shiny::removeModal()
      if (length(input$excluir_cols_selecionadas) > 0) {
        res <- remover_colunas(valores$dados, input$excluir_cols_selecionadas)
        if (!is.null(res) && !is.null(res$operacao)) {
          valores$historico_dados <- c(valores$historico_dados, list(valores$dados))
          valores$dados <- res$dados
          valores$operacoes <- c(valores$operacoes, list(res$operacao))
          shiny::showNotification(paste0("Sucesso: ", res$operacao$titulo), type = "message")
        }
      }
    })

    # =========================================================================
    # MODAL 8: VISUALIZAR CÓDIGO DPLYR EM TEMPO REAL
    # =========================================================================
    shiny::observeEvent(input$btn_ver_codigo, {
      script_gerado <- gerar_script_dplyr(
        nome_dados = nome_objeto_original,
        operacoes = valores$operacoes,
        atribuir_a = paste0(nome_objeto_original, "_tratado")
      )

      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(shiny::icon("code", class = "text-primary me-2"), "Código R Reproduzível (dplyr)"),
        size = "l",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Fechar")
        ),
        htmltools::tags$p(class = "text-muted small", "Este script reproduz com precisão matemática cada alteração realizada visualmente nesta sessão."),
        htmltools::tags$pre(class = "code-output", script_gerado)
      ))
    })

    # =========================================================================
    # CONCLUIR / CANCELAR
    # =========================================================================
    shiny::observeEvent(input$btn_cancelar, {
      shiny::stopApp(NULL)
    })

    shiny::observeEvent(input$btn_concluir, {
      # Retorna o data.frame com classe dataflow e metadados de histórico
      dados_finais <- criar_objeto_dataflow(
        dados = valores$dados,
        operacoes = valores$operacoes,
        dados_originais = valores$dim_original
      )
      shiny::stopApp(dados_finais)
    })
  }
}
