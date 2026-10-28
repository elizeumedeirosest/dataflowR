#' @title Resumo e Visualização da Linhagem de Dados
#' @description Apresenta a linhagem completa de transformações no console em formato
#'   ANSI elegante ou em diagrama visual interativo.
#' @name resumo
NULL

#' Resumo da Linhagem do dataflow
#'
#' Exibe no console uma visão detalhada e auditável de cada etapa realizada
#' no pipeline de preparação de dados.
#'
#' @param dados Objeto da classe \code{dataflow} retornado pela função \code{dataflow()}.
#' @return Retorna o histórico de operações invisivelmente.
#' @export
#' @examples
#' \dontrun{
#' dados_limpos <- dataflow(mtcars)
#' resumo_dataflow(dados_limpos)
#' }
resumo_dataflow <- function(dados) {
  if (!inherits(dados, "dataflow")) {
    cli::cli_abort("O objeto fornecido nao e da classe {.cls dataflow}. Utilize a funcao {.fn dataflow} para gerar dados com rastreamento.")
  }

  historico <- attr(dados, "historico")
  origem <- attr(dados, "origem")

  cli::cli_h1(cli::col_blue(paste0(cli::style_bold("dataflowR"), " - Linhagem & Auditoria de Transformações")))

  if (!is.null(origem)) {
    cli::cli_alert_info("Dados Originais: {.val {origem$linhas}} linhas x {.val {origem$colunas}} colunas")
  }

  if (is.null(historico) || length(historico) == 0) {
    cli::cli_alert_warning("Nenhuma transformacao foi registrada. Os dados permanecem inalterados.")
    return(invisible(historico))
  }

  cli::cli_h2("Etapas Executadas ({length(historico)})")

  for (i in seq_along(historico)) {
    op <- historico[[i]]
    detalhes <- op$detalhes

    # Ícone e cor baseado no tipo
    tag_tipo <- switch(
      op$tipo,
      "filtro" = cli::col_yellow("[FILTRO]"),
      "imputacao" = cli::col_green("[AUSENTES]"),
      "datas" = cli::col_cyan("[DATAS]"),
      "remover_coluna" = cli::col_red("[EXCLUSÃO]"),
      "renomear" = cli::col_magenta("[RENOMEAR]"),
      "mutacao" = cli::col_blue("[NOVA COLUNA]"),
      "substituicao" = cli::col_yellow("[SUBSTITUIÇÃO]"),
      "edicao" = cli::col_grey("[EDIÇÃO MANUAL]"),
      cli::col_grey("[OPERAÇÃO]")
    )

    cli::cli_li("{cli::style_bold(paste0(i, '. '))} {tag_tipo} {cli::style_bold(op$titulo)}")
    if (nzchar(op$comentario)) {
      cli::cli_text("    {cli::col_grey(op$comentario)}")
    }

    # Métricas de antes/depois se disponíveis
    if (!is.null(detalhes$linhas_antes) && !is.null(detalhes$linhas_depois)) {
      dif_linhas <- detalhes$linhas_depois - detalhes$linhas_antes
      dif_str <- if (dif_linhas != 0) paste0(" (", ifelse(dif_linhas > 0, "+", ""), dif_linhas, ")") else ""
      cli::cli_text("    {cli::col_silver('Linhas:')} {detalhes$linhas_antes} ──> {detalhes$linhas_depois}{dif_str}")
    }
  }

  cli::cli_h2("Situação Final do Banco")
  n_na_total <- sum(is.na(dados))
  na_info <- if (n_na_total == 0) cli::col_green("0 ausentes") else cli::col_red(paste0(n_na_total, " ausentes"))

  cli::cli_bullets(c(
    "v" = "Dimensoes: {.strong {nrow(dados)}} linhas x {.strong {ncol(dados)}} colunas",
    "v" = "Integridade: {na_info}",
    "*" = "Codigo reprodutivel gerado com sucesso. Acesse com: {.code attr(dados, 'codigo_dplyr')}"
  ))
  cli::cli_rule()

  invisible(historico)
}

#' Gerar Fluxograma Visual da Linhagem (Diagrama)
#'
#' Renderiza um diagrama interativo em HTML/Mermaid representando o fluxo
#' completo desde o dado bruto até a base final tratada.
#'
#' @param dados Objeto da classe \code{dataflow}.
#' @return Objeto HTML ou texto do diagrama Mermaid.
#' @export
grafico_dataflow <- function(dados) {
  if (!inherits(dados, "dataflow")) {
    cli::cli_abort("O objeto fornecido nao e da classe {.cls dataflow}.")
  }

  historico <- attr(dados, "historico")
  origem <- attr(dados, "origem")

  linhas_mermaid <- c("graph TD", "  classDef box fill:#f8fafc,stroke:#0284c7,stroke-width:2px,color:#0f172a,rx:8,ry:8;")

  node_origem <- if (!is.null(origem)) {
    paste0('  N0["<b>Dados Brutos</b><br>', origem$linhas, ' linhas | ', origem$colunas, ' cols"]')
  } else {
    '  N0["<b>Dados Brutos</b>"]'
  }
  linhas_mermaid <- c(linhas_mermaid, node_origem)

  if (is.null(historico) || length(historico) == 0) {
    linhas_mermaid <- c(linhas_mermaid, '  N0 --> N_FIM["<b>Base Final</b><br>Sem alterações"]')
    mermaid_str <- paste(linhas_mermaid, collapse = "\n")
    return(mermaid_str)
  }

  ultimo_node <- "N0"

  for (i in seq_along(historico)) {
    op <- historico[[i]]
    node_id <- paste0("N", i)
    node_label <- paste0('  ', node_id, '["<b>', i, '. ', op$titulo, '</b><br><small>', op$tipo, '</small>"]')
    linhas_mermaid <- c(linhas_mermaid, node_label)
    linhas_mermaid <- c(linhas_mermaid, paste0("  ", ultimo_node, " --> ", node_id))
    ultimo_node <- node_id
  }

  node_final <- paste0('  N_FINAL["<b>Base Final Tratada</b><br>', nrow(dados), ' linhas | ', ncol(dados), ' cols"]')
  linhas_mermaid <- c(linhas_mermaid, node_final)
  linhas_mermaid <- c(linhas_mermaid, paste0("  ", ultimo_node, " --> N_FINAL"))

  mermaid_str <- paste(linhas_mermaid, collapse = "\n")

  # Se estiver em sessão interativa, exibe via HTML widget
  html_widget <- htmltools::tagList(
    htmltools::tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"),
    htmltools::tags$div(
      class = "mermaid",
      style = "text-align: center; margin: 20px auto; max-width: 800px;",
      htmltools::HTML(mermaid_str)
    ),
    htmltools::tags$script("mermaid.initialize({startOnLoad:true, theme:'neutral'});")
  )

  if (interactive()) {
    htmltools::html_print(html_widget)
  }

  invisible(mermaid_str)
}
