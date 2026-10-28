#' @title Motor de Rastreamento de Operações e Linhagem
#' @description Estruturas internas para registrar o histórico de transformações,
#'   auditoria e compilação do script dplyr reproduzível.
#' @name operacoes
#' @keywords internal
NULL

#' Criar uma nova operação no pipeline
#'
#' @param tipo Caractere indicando o tipo da operação: "edicao", "filtro", "imputacao",
#'   "datas", "remover_coluna", "renomear", "mutacao", "substituicao".
#' @param titulo Resumo curto da operação (ex.: "Filtro: idade > 18").
#' @param comentario Comentário explicativo em português para o script.
#' @param codigo_dplyr Código dplyr/tidyverse que executa a transformação.
#' @param detalhes Lista com métricas antes/depois (ex.: linhas_antes, linhas_depois, cols_antes, cols_depois).
#' @return Objeto da classe \code{dataflow_op}.
#' @keywords internal
nova_operacao <- function(tipo,
                          titulo,
                          comentario = "",
                          codigo_dplyr = "",
                          detalhes = list()) {
  structure(
    list(
      id = paste0("op_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1)),
      tipo = tipo,
      titulo = titulo,
      comentario = comentario,
      codigo = codigo_dplyr,
      detalhes = detalhes,
      timestamp = Sys.time()
    ),
    class = "dataflow_op"
  )
}

#' Gerar Script dplyr Reproduzível a partir do Histórico
#'
#' Converte a pilha de operações registradas em um script R limpo,
#' comentado em português e pronto para publicação ou execução.
#'
#' @param nome_dados Nome do objeto original (padrão: \code{"dados"}).
#' @param operacoes Lista de operações da classe \code{dataflow_op}.
#' @param atribuir_a Nome da variável de destino (se NULL, retorna a expressão do pipeline).
#' @return String contendo o script R formatado.
#' @export
gerar_script_dplyr <- function(nome_dados = "dados",
                               operacoes = list(),
                               atribuir_a = "dados_tratados") {
  cabecalho <- c(
    "# ==============================================================================",
    "# PIPELINE DE TRATAMENTO DE DADOS - dataflowR",
    paste0("# Gerado em: ", format(Sys.time(), "%d/%m/%Y as %H:%M")),
    paste0("# Total de operacoes realizadas: ", length(operacoes)),
    "# ==============================================================================",
    "",
    "library(dplyr)",
    "library(tidyr)",
    "library(lubridate)",
    ""
  )

  if (length(operacoes) == 0) {
    corpo <- paste0("# Nenhuma modificacao foi realizada sobre '", nome_dados, "'.\n",
                    if (!is.null(atribuir_a)) paste0(atribuir_a, " <- ", nome_dados) else nome_dados)
    return(paste(c(cabecalho, corpo), collapse = "\n"))
  }

  blocos <- character(0)

  for (i in seq_along(operacoes)) {
    op <- operacoes[[i]]
    bloco <- c(
      paste0("  # ", i, ". ", op$titulo),
      if (nzchar(op$comentario)) paste0("  # ", op$comentario),
      paste0("  ", op$codigo)
    )
    blocos <- c(blocos, paste(bloco, collapse = "\n"))
  }

  pipeline <- paste(blocos, collapse = " |>\n\n")

  if (!is.null(atribuir_a)) {
    corpo_completo <- paste0(atribuir_a, " <- ", nome_dados, " |>\n", pipeline)
  } else {
    corpo_completo <- paste0(nome_dados, " |>\n", pipeline)
  }

  paste(c(cabecalho, corpo_completo, ""), collapse = "\n")
}
