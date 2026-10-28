#' @title Filtros e Substituição Condicional de Valores
#' @description Filtragem de registros e substituição de valores sentinela
#'   ou extremos sob condições lógicas.
#' @name filtros
#' @keywords internal
NULL

#' Filtrar Linhas por Condição Lógica
#'
#' @param dados Data frame.
#' @param coluna Nome da coluna a ser avaliada.
#' @param operador Operador: "==", "!=", ">", ">=", "<", "<=", "entre", "%in%", "is.na", "!is.na".
#' @param valor Valor ou vetor de valores para comparação.
#' @param valor2 Segundo valor para o operador "entre".
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
filtrar_por_condicao <- function(dados, coluna, operador, valor = NULL, valor2 = NULL) {
  stopifnot(coluna %in% names(dados))
  n_antes <- nrow(dados)
  cols_antes <- ncol(dados)

  vetor <- dados[[coluna]]
  mascara <- rep(TRUE, nrow(dados))

  if (operador == "==") {
    mascara <- !is.na(vetor) & (vetor == valor)
    val_repr <- if (is.character(valor)) paste0('"', valor, '"') else as.character(valor)
    codigo <- paste0("dplyr::filter(", coluna, " == ", val_repr, ")")
    titulo <- paste0("Filtro: '", coluna, "' == ", valor)

  } else if (operador == "!=") {
    mascara <- !is.na(vetor) & (vetor != valor)
    val_repr <- if (is.character(valor)) paste0('"', valor, '"') else as.character(valor)
    codigo <- paste0("dplyr::filter(", coluna, " != ", val_repr, ")")
    titulo <- paste0("Filtro: '", coluna, "' != ", valor)

  } else if (operador == ">") {
    mascara <- !is.na(vetor) & (vetor > as.numeric(valor))
    codigo <- paste0("dplyr::filter(", coluna, " > ", valor, ")")
    titulo <- paste0("Filtro: '", coluna, "' > ", valor)

  } else if (operador == ">=") {
    mascara <- !is.na(vetor) & (vetor >= as.numeric(valor))
    codigo <- paste0("dplyr::filter(", coluna, " >= ", valor, ")")
    titulo <- paste0("Filtro: '", coluna, "' >= ", valor)

  } else if (operador == "<") {
    mascara <- !is.na(vetor) & (vetor < as.numeric(valor))
    codigo <- paste0("dplyr::filter(", coluna, " < ", valor, ")")
    titulo <- paste0("Filtro: '", coluna, "' < ", valor)

  } else if (operador == "<=") {
    mascara <- !is.na(vetor) & (vetor <= as.numeric(valor))
    codigo <- paste0("dplyr::filter(", coluna, " <= ", valor, ")")
    titulo <- paste0("Filtro: '", coluna, "' <= ", valor)

  } else if (operador == "entre") {
    v1 <- as.numeric(valor)
    v2 <- as.numeric(valor2)
    mascara <- !is.na(vetor) & (vetor >= v1 & vetor <= v2)
    codigo <- paste0("dplyr::filter(", coluna, " >= ", v1, " & ", coluna, " <= ", v2, ")")
    titulo <- paste0("Filtro: '", coluna, "' entre ", v1, " e ", v2)

  } else if (operador == "%in%") {
    mascara <- !is.na(vetor) & (vetor %in% valor)
    vals_str <- paste0('c(', paste0('"', valor, '"', collapse = ", "), ')')
    codigo <- paste0("dplyr::filter(", coluna, " %in% ", vals_str, ")")
    titulo <- paste0("Filtro: '", coluna, "' pertence a lista de valores")

  } else if (operador == "is.na") {
    mascara <- is.na(vetor)
    codigo <- paste0("dplyr::filter(is.na(", coluna, "))")
    titulo <- paste0("Filtro: manter apenas NA em '", coluna, "'")

  } else if (operador == "!is.na") {
    mascara <- !is.na(vetor)
    codigo <- paste0("dplyr::filter(!is.na(", coluna, "))")
    titulo <- paste0("Filtro: remover NA em '", coluna, "'")
  }

  dados_novos <- dados[mascara, , drop = FALSE]
  row.names(dados_novos) <- NULL
  n_depois <- nrow(dados_novos)

  comentario <- paste0("Linhas antes: ", n_antes, " -> Linhas restantes: ", n_depois,
                       " (removidas: ", n_antes - n_depois, ")")

  op <- nova_operacao(
    tipo = "filtro",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      coluna = coluna,
      operador = operador,
      linhas_antes = n_antes,
      linhas_depois = n_depois,
      linhas_removidas = n_antes - n_depois,
      cols_antes = cols_antes,
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}

#' Substituir Valores sob Condição (Limpeza de Sentinelas)
#'
#' @param dados Data frame.
#' @param coluna Nome da coluna a ser modificada.
#' @param operador "==", ">", "<", ">=", "<=".
#' @param valor_alvo Valor a ser comparado (ex.: -999, "?", 9999).
#' @param novo_valor Novo valor (pode ser NA ou outro valor).
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
substituir_por_condicao <- function(dados, coluna, operador = "==", valor_alvo = -999, novo_valor = NA) {
  stopifnot(coluna %in% names(dados))
  n_antes <- nrow(dados)
  cols_antes <- ncol(dados)

  dados_novos <- dados
  vetor <- dados_novos[[coluna]]

  # Determina máscara
  if (operador == "==") {
    if (is.na(valor_alvo)) {
      alvo_idx <- is.na(vetor)
    } else {
      alvo_idx <- !is.na(vetor) & (vetor == valor_alvo)
    }
  } else if (operador == ">") {
    alvo_idx <- !is.na(vetor) & (vetor > as.numeric(valor_alvo))
  } else if (operador == "<") {
    alvo_idx <- !is.na(vetor) & (vetor < as.numeric(valor_alvo))
  } else if (operador == ">=") {
    alvo_idx <- !is.na(vetor) & (vetor >= as.numeric(valor_alvo))
  } else if (operador == "<=") {
    alvo_idx <- !is.na(vetor) & (vetor <= as.numeric(valor_alvo))
  }

  n_substituidos <- sum(alvo_idx)
  dados_novos[[coluna]][alvo_idx] <- novo_valor

  novo_val_str <- if (is.na(novo_valor)) "NA" else if (is.character(novo_valor)) paste0('"', novo_valor, '"') else as.character(novo_valor)
  alvo_val_str <- if (is.na(valor_alvo)) "NA" else if (is.character(valor_alvo)) paste0('"', valor_alvo, '"') else as.character(valor_alvo)

  if (operador == "==" && is.na(novo_valor)) {
    codigo <- paste0("dplyr::mutate(", coluna, " = dplyr::na_if(", coluna, ", ", alvo_val_str, "))")
    titulo <- paste0("Substituição: '", coluna, "' (", alvo_val_str, " -> NA)")
  } else {
    condicao_str <- paste0(coluna, " ", operador, " ", alvo_val_str)
    codigo <- paste0("dplyr::mutate(", coluna, " = ifelse(", condicao_str, ", ", novo_val_str, ", ", coluna, "))")
    titulo <- paste0("Substituição: '", coluna, "' onde ", operador, " ", alvo_val_str, " por ", novo_val_str)
  }

  comentario <- paste0("Substituidos ", n_substituidos, " valores na coluna '", coluna, "'")

  op <- nova_operacao(
    tipo = "substituicao",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      coluna = coluna,
      n_substituidos = n_substituidos,
      linhas_antes = n_antes,
      linhas_depois = nrow(dados_novos),
      cols_antes = cols_antes,
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}
