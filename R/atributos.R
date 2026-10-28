#' @title Engenharia de Atributos, Tipagem e Renomeação
#' @description Funções para criar novas colunas, discretizar faixas, alterar tipos
#'   de variáveis e renomear colunas com geração de código reproduzível.
#' @name atributos
#' @keywords internal
NULL

#' Renomear Coluna
#'
#' @param dados Data frame.
#' @param coluna_antiga Nome atual da coluna.
#' @param coluna_nova Novo nome para a coluna.
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
renomear_coluna <- function(dados, coluna_antiga, coluna_nova) {
  stopifnot(coluna_antiga %in% names(dados))
  if (identical(coluna_antiga, coluna_nova)) return(list(dados = dados, operacao = NULL))

  dados_novos <- dados
  names(dados_novos)[names(dados_novos) == coluna_antiga] <- coluna_nova

  codigo <- paste0("dplyr::rename(", coluna_nova, " = ", coluna_antiga, ")")
  titulo <- paste0("Renomear Coluna: '", coluna_antiga, "' -> '", coluna_nova, "'")
  comentario <- paste0("Coluna '", coluna_antiga, "' renomeada para '", coluna_nova, "'")

  op <- nova_operacao(
    tipo = "renomear",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      coluna_antiga = coluna_antiga,
      coluna_nova = coluna_nova,
      linhas_antes = nrow(dados),
      linhas_depois = nrow(dados_novos),
      cols_antes = ncol(dados),
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}

#' Remover Colunas Selecionadas
#'
#' @param dados Data frame.
#' @param colunas Vetor de nomes de colunas a serem excluídas.
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
remover_colunas <- function(dados, colunas) {
  colunas_validas <- intersect(colunas, names(dados))
  if (length(colunas_validas) == 0) return(list(dados = dados, operacao = NULL))

  dados_novos <- dados[, !(names(dados) %in% colunas_validas), drop = FALSE]

  codigo <- paste0("dplyr::select(-tidyselect::all_of(c(",
                   paste0('"', colunas_validas, '"', collapse = ", "), ")))")
  titulo <- paste0("Excluir Colunas: ", paste(colunas_validas, collapse = ", "))
  comentario <- paste0("Removidas as colunas: ", paste(colunas_validas, collapse = ", "))

  op <- nova_operacao(
    tipo = "remover_coluna",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      colunas_removidas = colunas_validas,
      linhas_antes = nrow(dados),
      linhas_depois = nrow(dados_novos),
      cols_antes = ncol(dados),
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}

#' Alterar Tipo de Dados de uma Coluna
#'
#' @param dados Data frame.
#' @param coluna Nome da coluna.
#' @param novo_tipo "numeric", "character", "factor", "integer", "Date".
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
alterar_tipo_coluna <- function(dados, coluna, novo_tipo = c("numeric", "character", "factor", "integer", "Date")) {
  stopifnot(coluna %in% names(dados))
  novo_tipo <- match.arg(novo_tipo)

  dados_novos <- dados
  val <- dados_novos[[coluna]]

  if (novo_tipo == "numeric") {
    dados_novos[[coluna]] <- as.numeric(as.character(val))
    fun_str <- "as.numeric"
  } else if (novo_tipo == "integer") {
    dados_novos[[coluna]] <- as.integer(as.character(val))
    fun_str <- "as.integer"
  } else if (novo_tipo == "character") {
    dados_novos[[coluna]] <- as.character(val)
    fun_str <- "as.character"
  } else if (novo_tipo == "factor") {
    dados_novos[[coluna]] <- as.factor(val)
    fun_str <- "as.factor"
  } else if (novo_tipo == "Date") {
    dados_novos[[coluna]] <- as.Date(padronizar_data(val))
    fun_str <- "as.Date"
  }

  codigo <- paste0("dplyr::mutate(", coluna, " = ", fun_str, "(", coluna, "))")
  titulo <- paste0("Conversão de Tipo: '", coluna, "' -> ", novo_tipo)
  comentario <- paste0("Tipo da coluna '", coluna, "' convertido para ", novo_tipo)

  op <- nova_operacao(
    tipo = "tipagem",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      coluna = coluna,
      novo_tipo = novo_tipo,
      linhas_antes = nrow(dados),
      linhas_depois = nrow(dados_novos),
      cols_antes = ncol(dados),
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}

#' Criar Nova Coluna a partir de Expressão R
#'
#' @param dados Data frame.
#' @param novo_nome Nome da nova coluna a ser criada.
#' @param expressao_str String contendo a expressão (ex.: "colA / colB" ou "log(mpg)").
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
criar_coluna_expressao <- function(dados, novo_nome, expressao_str) {
  stopifnot(nzchar(novo_nome), nzchar(expressao_str))

  expr <- parse(text = expressao_str)
  res <- with(dados, eval(expr))

  dados_novos <- dados
  dados_novos[[novo_nome]] <- res

  codigo <- paste0("dplyr::mutate(", novo_nome, " = ", expressao_str, ")")
  titulo <- paste0("Nova Coluna: '", novo_nome, "' = ", expressao_str)
  comentario <- paste0("Coluna '", novo_nome, "' criada via expressao: ", expressao_str)

  op <- nova_operacao(
    tipo = "mutacao",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      nova_coluna = novo_nome,
      expressao = expressao_str,
      linhas_antes = nrow(dados),
      linhas_depois = nrow(dados_novos),
      cols_antes = ncol(dados),
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}
