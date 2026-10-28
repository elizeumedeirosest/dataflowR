#' @title Tratamento e Imputação de Dados Ausentes (NA)
#' @description Funções para remoção e imputação univariada e por grupo,
#'   gerando código dplyr auditável.
#' @name ausentes
#' @keywords internal
NULL

#' Calcular a Moda Estatística de um Vetor
#' @param x Vetor qualquer.
#' @param na.rm Lógico. Se TRUE, desconsidera NA.
#' @return Valor mais frequente de \code{x}.
#' @keywords internal
calcular_moda <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  tab <- table(x)
  moda_val <- names(tab)[which.max(tab)]
  # Preserva tipo se numérico
  if (is.numeric(x)) {
    return(as.numeric(moda_val))
  }
  moda_val
}

#' Remover Linhas com Valores Ausentes (NA)
#'
#' @param dados Data frame a ser tratado.
#' @param colunas Vetor de colunas a verificar (se NULL, verifica todas as colunas).
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
remover_linhas_na <- function(dados, colunas = NULL) {
  n_antes <- nrow(dados)
  cols_antes <- ncol(dados)

  if (is.null(colunas) || length(colunas) == 0) {
    dados_novos <- stats::na.omit(dados)
    row.names(dados_novos) <- NULL
    codigo <- "tidyr::drop_na()"
    titulo <- "Remover Linhas com NA (Global)"
    comentario <- "Remover registros contendo qualquer valor ausente no banco"
  } else {
    cols_str <- paste(colunas, collapse = ", ")
    dados_novos <- dados
    for (col in colunas) {
      if (col %in% names(dados_novos)) {
        dados_novos <- dados_novos[!is.na(dados_novos[[col]]), , drop = FALSE]
      }
    }
    row.names(dados_novos) <- NULL
    codigo <- paste0("tidyr::drop_na(tidyselect::all_of(c(", paste0('"', colunas, '"', collapse = ", "), ")))")
    titulo <- paste0("Remover Linhas com NA em: ", cols_str)
    comentario <- paste0("Remover linhas onde ha NA nas colunas: ", cols_str)
  }

  n_depois <- nrow(dados_novos)
  removidas <- n_antes - n_depois

  op <- nova_operacao(
    tipo = "imputacao",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      linhas_antes = n_antes,
      linhas_depois = n_depois,
      linhas_removidas = removidas,
      cols_antes = cols_antes,
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}

#' Remover Colunas com Alto Percentual de NA
#'
#' @param dados Data frame.
#' @param limiar_pct Percentual de corte (0 a 100). Colunas com % de NA >= limiar serão removidas.
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
remover_colunas_na <- function(dados, limiar_pct = 100) {
  n_antes <- nrow(dados)
  cols_antes <- ncol(dados)

  pct_na <- vapply(dados, function(col) mean(is.na(col)) * 100, numeric(1))
  cols_para_remover <- names(pct_na)[pct_na >= limiar_pct]

  if (length(cols_para_remover) == 0) {
    return(list(dados = dados, operacao = NULL))
  }

  dados_novos <- dados[, !(names(dados) %in% cols_para_remover), drop = FALSE]

  codigo <- paste0("dplyr::select(-tidyselect::all_of(c(",
                   paste0('"', cols_para_remover, '"', collapse = ", "), ")))")
  titulo <- paste0("Remover Colunas com >= ", limiar_pct, "% de NA")
  comentario <- paste0("Removidas ", length(cols_para_remover), " colunas: ",
                       paste(cols_para_remover, collapse = ", "))

  op <- nova_operacao(
    tipo = "remover_coluna",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      linhas_antes = n_antes,
      linhas_depois = nrow(dados_novos),
      cols_antes = cols_antes,
      cols_depois = ncol(dados_novos),
      colunas_removidas = cols_para_remover
    )
  )

  list(dados = dados_novos, operacao = op)
}

#' Imputar Valores Ausentes (Univariado)
#'
#' @param dados Data frame.
#' @param coluna Nome da coluna a ser imputada.
#' @param metodo Método de imputação: "media", "mediana", "moda", "minimo", "maximo", "zero", "constante", "nao_informado".
#' @param valor_constante Valor fixo quando metodo = "constante".
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
imputar_univariado <- function(dados, coluna, metodo = "media", valor_constante = NULL) {
  stopifnot(coluna %in% names(dados))
  n_antes <- nrow(dados)
  cols_antes <- ncol(dados)
  vetor <- dados[[coluna]]
  n_na <- sum(is.na(vetor))

  if (n_na == 0) {
    return(list(dados = dados, operacao = NULL))
  }

  dados_novos <- dados

  if (metodo == "media") {
    val <- mean(vetor, na.rm = TRUE)
    dados_novos[[coluna]][is.na(vetor)] <- val
    codigo <- paste0("dplyr::mutate(", coluna, " = tidyr::replace_na(", coluna, ", mean(", coluna, ", na.rm = TRUE)))")
    titulo <- paste0("Imputação NA: '", coluna, "' pela Média")
    comentario <- paste0("Preencher ", n_na, " valores ausentes em '", coluna, "' pela media (", round(val, 2), ")")

  } else if (metodo == "mediana") {
    val <- stats::median(vetor, na.rm = TRUE)
    dados_novos[[coluna]][is.na(vetor)] <- val
    codigo <- paste0("dplyr::mutate(", coluna, " = tidyr::replace_na(", coluna, ", stats::median(", coluna, ", na.rm = TRUE)))")
    titulo <- paste0("Imputação NA: '", coluna, "' pela Mediana")
    comentario <- paste0("Preencher ", n_na, " valores ausentes em '", coluna, "' pela mediana (", round(val, 2), ")")

  } else if (metodo == "moda") {
    val <- calcular_moda(vetor, na.rm = TRUE)
    dados_novos[[coluna]][is.na(vetor)] <- val
    val_str <- if (is.character(val)) paste0('"', val, '"') else as.character(val)
    codigo <- paste0("dplyr::mutate(", coluna, " = tidyr::replace_na(", coluna, ", ", val_str, "))")
    titulo <- paste0("Imputação NA: '", coluna, "' pela Moda")
    comentario <- paste0("Preencher ", n_na, " valores ausentes em '", coluna, "' pela moda (", val, ")")

  } else if (metodo == "zero") {
    dados_novos[[coluna]][is.na(vetor)] <- 0
    codigo <- paste0("dplyr::mutate(", coluna, " = tidyr::replace_na(", coluna, ", 0))")
    titulo <- paste0("Imputação NA: '", coluna, "' por Zero")
    comentario <- paste0("Preencher ", n_na, " valores ausentes em '", coluna, "' por 0")

  } else if (metodo == "nao_informado") {
    if (is.factor(dados_novos[[coluna]])) {
      levels(dados_novos[[coluna]]) <- c(levels(dados_novos[[coluna]]), "Não Informado")
    }
    dados_novos[[coluna]][is.na(vetor)] <- "Não Informado"
    codigo <- paste0('dplyr::mutate(', coluna, ' = tidyr::replace_na(as.character(', coluna, '), "Não Informado"))')
    titulo <- paste0("Imputação NA: '", coluna, "' por 'Não Informado'")
    comentario <- paste0("Rotular ", n_na, " registros ausentes como 'Não Informado'")

  } else if (metodo == "constante") {
    dados_novos[[coluna]][is.na(vetor)] <- valor_constante
    val_repr <- if (is.character(valor_constante)) paste0('"', valor_constante, '"') else as.character(valor_constante)
    codigo <- paste0("dplyr::mutate(", coluna, " = tidyr::replace_na(", coluna, ", ", val_repr, "))")
    titulo <- paste0("Imputação NA: '", coluna, "' por Constante")
    comentario <- paste0("Preencher ", n_na, " ausentes com valor fixo: ", valor_constante)
  }

  op <- nova_operacao(
    tipo = "imputacao",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      coluna = coluna,
      n_imputados = n_na,
      metodo = metodo,
      linhas_antes = n_antes,
      linhas_depois = nrow(dados_novos),
      cols_antes = cols_antes,
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}

#' Imputar Valores Ausentes Agrupados por Categoria
#'
#' @param dados Data frame.
#' @param coluna Nome da coluna a ser imputada (numérica).
#' @param coluna_grupo Nome da coluna categórica de agrupamento.
#' @param metodo "media" ou "mediana".
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
imputar_por_grupo <- function(dados, coluna, coluna_grupo, metodo = "mediana") {
  stopifnot(coluna %in% names(dados), coluna_grupo %in% names(dados))
  n_antes <- nrow(dados)
  cols_antes <- ncol(dados)
  vetor <- dados[[coluna]]
  n_na <- sum(is.na(vetor))

  if (n_na == 0) return(list(dados = dados, operacao = NULL))

  dados_novos <- dados

  fun_str <- if (metodo == "media") "mean" else "stats::median"

  # Executa a imputação por grupo no R
  grupos <- split(seq_len(nrow(dados_novos)), dados_novos[[coluna_grupo]])
  for (idx in grupos) {
    vals <- dados_novos[[coluna]][idx]
    if (any(is.na(vals))) {
      rep_val <- if (metodo == "media") mean(vals, na.rm = TRUE) else stats::median(vals, na.rm = TRUE)
      if (is.nan(rep_val) || is.na(rep_val)) {
        # Fallback para média global se o grupo todo for NA
        rep_val <- if (metodo == "media") mean(vetor, na.rm = TRUE) else stats::median(vetor, na.rm = TRUE)
      }
      dados_novos[[coluna]][idx[is.na(vals)]] <- rep_val
    }
  }

  codigo <- paste0(
    "dplyr::group_by(", coluna_grupo, ") |>\n",
    "  dplyr::mutate(", coluna, " = tidyr::replace_na(", coluna, ", ", fun_str, "(", coluna, ", na.rm = TRUE))) |>\n",
    "  dplyr::ungroup()"
  )

  titulo <- paste0("Imputação NA por Grupo: '", coluna, "' por '", coluna_grupo, "' (", metodo, ")")
  comentario <- paste0("Preencher ", n_na, " NAs em '", coluna, "' pela ", metodo, " de cada '", coluna_grupo, "'")

  op <- nova_operacao(
    tipo = "imputacao",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      coluna = coluna,
      grupo = coluna_grupo,
      n_imputados = n_na,
      metodo = metodo,
      linhas_antes = n_antes,
      linhas_depois = nrow(dados_novos),
      cols_antes = cols_antes,
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}
