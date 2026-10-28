test_that("Tratamento de NAs funciona corretamente", {
  df <- data.frame(
    id = 1:5,
    valor = c(10, NA, 30, NA, 50),
    grupo = c("A", "A", "B", "B", "B")
  )

  # Remoção de linhas
  res_rem <- remover_linhas_na(df, "valor")
  expect_equal(nrow(res_rem$dados), 3)
  expect_equal(res_rem$operacao$detalhes$linhas_removidas, 2)

  # Imputação univariada pela média (média de 10, 30, 50 = 30)
  res_media <- imputar_univariado(df, "valor", metodo = "media")
  expect_equal(sum(is.na(res_media$dados$valor)), 0)
  expect_equal(res_media$dados$valor[2], 30)

  # Imputação por grupo (grupo A tem 10; grupo B tem 30 e 50 -> mediana 40)
  res_grupo <- imputar_por_grupo(df, "valor", "grupo", metodo = "mediana")
  expect_equal(sum(is.na(res_grupo$dados$valor)), 0)
  expect_equal(res_grupo$dados$valor[2], 10) # grupo A
  expect_equal(res_grupo$dados$valor[4], 40) # grupo B
})

test_that("Fragmentação de datas funciona perfeitamente", {
  df_datas <- data.frame(
    registro = c("2026-10-28", "2026-05-15", "2025-12-01"),
    stringsAsFactors = FALSE
  )

  res_datas <- fragmentar_data(df_datas, "registro", componentes = c("ano", "mes", "dia", "dia_semana", "fim_de_semana"))
  expect_true("registro_ano" %in% names(res_datas$dados))
  expect_true("registro_mes" %in% names(res_datas$dados))
  expect_true("registro_dia" %in% names(res_datas$dados))
  expect_true("registro_dia_semana" %in% names(res_datas$dados))
  expect_true("registro_fim_de_semana" %in% names(res_datas$dados))
  expect_equal(res_datas$dados$registro_ano[1], 2026)
  expect_equal(res_datas$dados$registro_mes[1], 10)
  expect_equal(res_datas$dados$registro_dia[1], 28)
})

test_that("Filtros e substituição de sentinelas funcionam", {
  df_filtros <- data.frame(
    idade = c(15, 25, 35, 45, -999)
  )

  # Substituição de sentinela -999 por NA
  res_sub <- substituir_por_condicao(df_filtros, "idade", "==", -999, NA)
  expect_true(is.na(res_sub$dados$idade[5]))

  # Filtro idade >= 25
  res_filtro <- filtrar_por_condicao(df_filtros, "idade", ">=", 25)
  expect_equal(nrow(res_filtro$dados), 3)
})

test_that("Objeto dataflow e resumo_dataflow funcionam", {
  df <- data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"))
  op1 <- nova_operacao("filtro", "Filtro teste", "manter x > 2", "dplyr::filter(x > 2)")

  obj <- criar_objeto_dataflow(df[3:5, ], list(op1), list(linhas = 5, colunas = 2))
  expect_s3_class(obj, "dataflow")
  expect_equal(length(attr(obj, "historico")), 1)

  # Teste resumo_dataflow
  res <- resumo_dataflow(obj)
  expect_equal(length(res), 1)
  expect_equal(res[[1]]$titulo, "Filtro teste")
})
