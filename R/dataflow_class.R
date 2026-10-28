#' @title Classe S3 dataflow
#' @description Construtores e métodos S3 para o objeto com linhagem rastreada.
#' @name dataflow_class
#' @keywords internal
NULL

#' Criar Objeto da Classe dataflow
#'
#' @param dados Data frame tratado.
#' @param operacoes Lista de operações da classe \code{dataflow_op}.
#' @param dados_originais Dimensões ou objeto original.
#' @return Objeto com classe \code{c("dataflow", class(dados))}.
#' @keywords internal
criar_objeto_dataflow <- function(dados, operacoes = list(), dados_originais = NULL) {
  structure(
    dados,
    historico = operacoes,
    origem = dados_originais,
    codigo_dplyr = gerar_script_dplyr("dados", operacoes, "dados_tratados"),
    class = unique(c("dataflow", class(dados)))
  )
}

#' Método Print para Objetos da Classe dataflow
#'
#' @param x Objeto da classe \code{dataflow}.
#' @param ... Argumentos adicionais repassados ao método print padrão.
#' @export
print.dataflow <- function(x, ...) {
  ops <- attr(x, "historico")
  n_ops <- if (is.null(ops)) 0 else length(ops)

  cli::cli_div(theme = list(span.bold = list("font-weight" = "bold")))
  cli::cli_alert_info("{.bold dataflow}: {nrow(x)} linhas x {ncol(x)} colunas | {n_ops} etapa{?s} no historico")
  cli::cli_end()

  # Chama o print normal da classe base sem o prefixo dataflow
  class_limpa <- setdiff(class(x), "dataflow")
  x_base <- x
  class(x_base) <- class_limpa
  print(utils::head(x_base, 10), ...)

  if (nrow(x) > 10) {
    cli::cli_text(cli::col_grey("... com mais {nrow(x) - 10} linhas."))
  }

  cli::cli_alert_success("Para ver a linhagem completa: {.code resumo_dataflow(dados)}")
  invisible(x)
}
