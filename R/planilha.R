#' @title Configuração e Integração com Handsontable
#' @description Renderização e captura de alterações na planilha interativa.
#' @name planilha
#' @keywords internal
NULL

#' Renderizar Tabela Interativa rhandsontable Customizada
#'
#' @param dados Data frame a ser exibido.
#' @param altura Altura em pixels da tabela (padrão: 550).
#' @return Objeto rhandsontable configurado.
#' @keywords internal
criar_tabela_interativa <- function(dados, altura = 550) {
  if (is.null(dados) || nrow(dados) == 0) {
    return(suppressWarnings(
      rhandsontable::rhandsontable(data.frame(Aviso = "Tabela vazia"))
    ))
  }

  # useTypes = FALSE: evita warning de hot_context_menu e permite operações de colunas
  hot <- suppressWarnings(
    rhandsontable::rhandsontable(
      dados,
      height  = altura,
      useTypes = FALSE,
      stretchH = "all",
      manualColumnResize = TRUE,
      manualRowResize    = TRUE,
      highlightRow       = TRUE,
      highlightCol       = TRUE,
      colHeaders         = names(dados)
    )
  )

  # Ordenação por coluna com seta indicadora (Handsontable.js nativo)
  hot <- suppressWarnings(
    rhandsontable::hot_table(hot, columnSorting = TRUE, sortIndicator = TRUE)
  )

  # Menu de contexto em português (sem add/remove coluna — temos botões próprios)
  menu_pt <- list(
    items = list(
      row_above  = list(name = "Inserir linha acima"),
      row_below  = list(name = "Inserir linha abaixo"),
      hsep1      = "---------",
      remove_row = list(name = "Remover linha(s) selecionada(s)"),
      hsep2      = "---------",
      undo       = list(name = "Desfazer"),
      redo       = list(name = "Refazer")
    )
  )

  hot <- suppressWarnings(
    rhandsontable::hot_context_menu(hot, customOpts = menu_pt)
  )

  hot
}

#' Detectar Edições Manuais em Células entre Duas Versões do Data Frame
#'
#' @param antigo Data frame antes da edição.
#' @param novo Data frame após a edição.
#' @return Lista com contagem de células modificadas e descrição.
#' @keywords internal
detectar_edicoes_celulas <- function(antigo, novo) {
  if (is.null(antigo) || is.null(novo)) return(NULL)
  if (!identical(dim(antigo), dim(novo))) return(NULL)

  cols_comuns <- intersect(names(antigo), names(novo))
  if (length(cols_comuns) == 0) return(NULL)

  modificacoes <- 0
  detalhes_edicoes <- character(0)

  for (col in cols_comuns) {
    v_antigo <- antigo[[col]]
    v_novo   <- novo[[col]]
    diferencas <- which(
      (is.na(v_antigo) != is.na(v_novo)) |
      (!is.na(v_antigo) & !is.na(v_novo) & v_antigo != v_novo)
    )
    if (length(diferencas) > 0) {
      modificacoes <- modificacoes + length(diferencas)
      detalhes_edicoes <- c(detalhes_edicoes, paste0(col, " (", length(diferencas), " cél.)"))
    }
  }

  if (modificacoes == 0) return(NULL)
  list(total_modificacoes = modificacoes, resumo = paste(detalhes_edicoes, collapse = ", "))
}
