#' @title Fragmentação e Engenharia de Datas
#' @description Detecção inteligente de formatos temporais e extração otimizada
#'   de componentes para modelagem e análise exploratória.
#' @name datas
#' @keywords internal
NULL

#' Identificar se um Vetor é ou Pode Ser Convertido para Data
#'
#' @param x Vetor a ser analisado.
#' @return Lista com \code{eh_data = TRUE/FALSE}, \code{formato_sugerido} e \code{possui_hora}.
#' @keywords internal
detectar_tipo_data <- function(x) {
  if (inherits(x, "Date")) {
    return(list(eh_data = TRUE, formato_sugerido = "Date", possui_hora = FALSE))
  }
  if (inherits(x, "POSIXt")) {
    return(list(eh_data = TRUE, formato_sugerido = "POSIXct", possui_hora = TRUE))
  }

  amostra <- stats::na.omit(as.character(utils::head(x, 100)))
  if (length(amostra) == 0) {
    return(list(eh_data = FALSE, formato_sugerido = NULL, possui_hora = FALSE))
  }

  # Teste ISO (YYYY-MM-DD)
  if (all(grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}", amostra))) {
    tem_hora <- any(grepl("\\d{1,2}:\\d{2}", amostra))
    return(list(eh_data = TRUE, formato_sugerido = if (tem_hora) "ymd_hms" else "ymd", possui_hora = tem_hora))
  }

  # Teste Brasileiro / Europeu (DD/MM/YYYY)
  if (all(grepl("^\\d{1,2}[-/]\\d{1,2}[-/]\\d{4}", amostra))) {
    tem_hora <- any(grepl("\\d{1,2}:\\d{2}", amostra))
    return(list(eh_data = TRUE, formato_sugerido = if (tem_hora) "dmy_hms" else "dmy", possui_hora = tem_hora))
  }

  # Teste Numérico Compacto (YYYYMMDD)
  if (is.numeric(x) && all(grepl("^\\d{8}$", amostra))) {
    return(list(eh_data = TRUE, formato_sugerido = "ymd_num", possui_hora = FALSE))
  }

  # Teste Serial do Excel (números entre 30000 e 65000)
  if (is.numeric(x) && all(x >= 30000 & x <= 65000, na.rm = TRUE)) {
    return(list(eh_data = TRUE, formato_sugerido = "excel_serial", possui_hora = FALSE))
  }

  list(eh_data = FALSE, formato_sugerido = NULL, possui_hora = FALSE)
}

#' Converter Vetor para Objeto de Data/Hora Padronizado
#'
#' @param x Vetor a ser convertido.
#' @param formato Formato sugerido ou NULL para auto-detecção.
#' @return Vetor da classe Date ou POSIXct.
#' @keywords internal
padronizar_data <- function(x, formato = NULL) {
  if (inherits(x, c("Date", "POSIXt"))) return(x)

  if (is.null(formato)) {
    det <- detectar_tipo_data(x)
    formato <- det$formato_sugerido
  }

  if (identical(formato, "dmy")) {
    return(as.Date(lubridate::dmy(x)))
  } else if (identical(formato, "dmy_hms")) {
    return(lubridate::dmy_hms(x))
  } else if (identical(formato, "ymd") || identical(formato, "ymd_num")) {
    return(as.Date(lubridate::ymd(x)))
  } else if (identical(formato, "ymd_hms")) {
    return(lubridate::ymd_hms(x))
  } else if (identical(formato, "excel_serial")) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  # Fallback seguro com lubridate
  parsed <- lubridate::parse_date_time(x, orders = c("dmy", "ymd", "mdy", "dmy HMS", "ymd HMS"), quiet = TRUE)
  if (!all(is.na(parsed[!is.na(x)]))) {
    return(parsed)
  }
  x
}

#' Fragmentar Coluna de Data em Componentes
#'
#' Extrai atributos temporais como ano, mês, dia, dia da semana, trimestre e flag
#' de fim de semana, gerando código dplyr/lubridate limpo e comentado.
#'
#' @param dados Data frame.
#' @param coluna Nome da coluna contendo datas.
#' @param componentes Vetor de caracteres com os componentes desejados:
#'   \code{"ano"}, \code{"mes"}, \code{"mes_nome"}, \code{"dia"}, \code{"dia_semana"},
#'   \code{"dia_semana_num"}, \code{"trimestre"}, \code{"semestre"}, \code{"fim_de_semana"},
#'   \code{"hora"}, \code{"minuto"}.
#' @param prefixo Prefixo opcional para os novos nomes de coluna (se NULL, usa o nome da coluna de origem).
#' @return Lista contendo \code{dados} tratados e o objeto \code{operacao}.
#' @keywords internal
fragmentar_data <- function(dados,
                            coluna,
                            componentes = c("ano", "mes", "dia", "dia_semana", "fim_de_semana"),
                            prefixo = NULL) {
  stopifnot(coluna %in% names(dados))
  n_antes <- nrow(dados)
  cols_antes <- ncol(dados)

  pref <- if (is.null(prefixo) || !nzchar(prefixo)) paste0(coluna, "_") else paste0(prefixo, "_")

  vetor_data <- padronizar_data(dados[[coluna]])
  dados_novos <- dados

  mutacoes_codigo <- character(0)
  colunas_criadas <- character(0)

  # Aplica cada componente selecionado
  if ("ano" %in% componentes) {
    nome_col <- paste0(pref, "ano")
    dados_novos[[nome_col]] <- lubridate::year(vetor_data)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::year(", coluna, ")"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("mes" %in% componentes) {
    nome_col <- paste0(pref, "mes")
    dados_novos[[nome_col]] <- lubridate::month(vetor_data)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::month(", coluna, ")"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("mes_nome" %in% componentes) {
    nome_col <- paste0(pref, "mes_nome")
    meses_pt <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                  "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
    m_num <- lubridate::month(vetor_data)
    dados_novos[[nome_col]] <- factor(meses_pt[m_num], levels = meses_pt)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::month(", coluna, ", label = TRUE, abbr = FALSE)"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("dia" %in% componentes) {
    nome_col <- paste0(pref, "dia")
    dados_novos[[nome_col]] <- lubridate::day(vetor_data)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::day(", coluna, ")"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("dia_semana" %in% componentes) {
    nome_col <- paste0(pref, "dia_semana")
    dias_pt <- c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado")
    d_num <- lubridate::wday(vetor_data)
    dados_novos[[nome_col]] <- factor(dias_pt[d_num], levels = dias_pt)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::wday(", coluna, ", label = TRUE, abbr = FALSE)"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("dia_semana_num" %in% componentes) {
    nome_col <- paste0(pref, "dia_semana_num")
    dados_novos[[nome_col]] <- lubridate::wday(vetor_data)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::wday(", coluna, ")"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("trimestre" %in% componentes) {
    nome_col <- paste0(pref, "trimestre")
    dados_novos[[nome_col]] <- paste0("T", lubridate::quarter(vetor_data))
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = paste0('T', lubridate::quarter(", coluna, "))"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("semestre" %in% componentes) {
    nome_col <- paste0(pref, "semestre")
    dados_novos[[nome_col]] <- ifelse(lubridate::quarter(vetor_data) <= 2, 1, 2)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = ifelse(lubridate::quarter(", coluna, ") <= 2, 1, 2)"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("fim_de_semana" %in% componentes) {
    nome_col <- paste0(pref, "fim_de_semana")
    dados_novos[[nome_col]] <- lubridate::wday(vetor_data) %in% c(1, 7)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::wday(", coluna, ") %in% c(1, 7)"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("hora" %in% componentes) {
    nome_col <- paste0(pref, "hora")
    dados_novos[[nome_col]] <- lubridate::hour(vetor_data)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::hour(", coluna, ")"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  if ("minuto" %in% componentes) {
    nome_col <- paste0(pref, "minuto")
    dados_novos[[nome_col]] <- lubridate::minute(vetor_data)
    mutacoes_codigo <- c(mutacoes_codigo, paste0(nome_col, " = lubridate::minute(", coluna, ")"))
    colunas_criadas <- c(colunas_criadas, nome_col)
  }

  codigo <- paste0(
    "dplyr::mutate(\n",
    paste0("  ", mutacoes_codigo, collapse = ",\n"), "\n",
    ")"
  )

  titulo <- paste0("Fragmentação de Data: '", coluna, "'")
  comentario <- paste0("Extraidos ", length(colunas_criadas), " atributos temporais: ",
                       paste(colunas_criadas, collapse = ", "))

  op <- nova_operacao(
    tipo = "datas",
    titulo = titulo,
    comentario = comentario,
    codigo_dplyr = codigo,
    detalhes = list(
      coluna_origem = coluna,
      colunas_criadas = colunas_criadas,
      linhas_antes = n_antes,
      linhas_depois = nrow(dados_novos),
      cols_antes = cols_antes,
      cols_depois = ncol(dados_novos)
    )
  )

  list(dados = dados_novos, operacao = op)
}
