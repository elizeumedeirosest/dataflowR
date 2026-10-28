#' @title Editor Visual e Engenharia de Atributos Interativa
#' @description Inicia a interface gráfica do dataflowR para exploração, edição direta
#'   em planilha, filtragem, imputação de dados ausentes e fragmentação de datas,
#'   gerando código dplyr reproduzível com auditoria completa de transformações.
#'
#' @param dados Um \code{data.frame}, \code{tibble} ou matriz a ser editada.
#' @param viewer Tipo de visualizador do Shiny Gadget:
#'   \itemize{
#'     \item \code{"dialog"}: Janela modal independente no RStudio (padrão).
#'     \item \code{"browser"}: Abre em aba do navegador web.
#'     \item \code{"pane"}: Painel Viewer do RStudio.
#'   }
#' @param largura Largura da janela modal em pixels (quando viewer = "dialog"). Padrão: 1200.
#' @param altura Altura da janela modal em pixels (quando viewer = "dialog"). Padrão: 750.
#'
#' @return Retorna um objeto da classe \code{dataflow} (herdando de \code{data.frame})
#'   contendo os dados processados e os atributos de linhagem e código reproduzível.
#'   Caso o usuário clique em Cancelar, retorna o objeto original sem alterações.
#'
#' @export
#' @examples
#' \dontrun{
#' library(dataflowR)
#'
#' # Abrir interface com dados de exemplo
#' dados_limpos <- dataflow(mtcars)
#'
#' # Inspecionar linhagem das transformações realizadas
#' resumo_dataflow(dados_limpos)
#' }
dataflow <- function(dados,
                     viewer = c("dialog", "browser", "pane"),
                     largura = 1200,
                     altura = 750) {
  if (missing(dados) || is.null(dados)) {
    cli::cli_abort("Voce precisa fornecer um {.cls data.frame} ou {.cls tibble} para o {.fn dataflow}.")
  }

  nome_obj <- deparse(substitute(dados))
  if (length(nome_obj) > 1 || !nzchar(nome_obj)) {
    nome_obj <- "dados"
  }

  viewer <- match.arg(viewer)

  # Configura o tipo de visualizador do gadget
  gadget_viewer <- switch(
    viewer,
    "dialog" = shiny::dialogViewer("dataflowR - Editor Visual de Dados", width = largura, height = altura),
    "browser" = shiny::browserViewer(),
    "pane" = shiny::paneViewer()
  )

  ui     <- criar_ui_dataflow()
  server <- criar_server_dataflow(dados_iniciais = dados, nome_objeto_original = nome_obj)

  # Executa como Shiny Gadget
  resultado <- suppressWarnings(
    shiny::runGadget(ui, server, viewer = gadget_viewer, stopOnCancel = FALSE)
  )

  # Se o usuário clicou em "Expandir" → reabre em tela cheia no navegador
  if (identical(resultado, "__expandir_navegador__")) {
    cli::cli_alert_info("Reabrindo em tela cheia no navegador...")
    resultado <- suppressWarnings(
      shiny::runGadget(ui, server, viewer = shiny::browserViewer(), stopOnCancel = FALSE)
    )
  }

  if (is.null(resultado)) {
    cli::cli_alert_info("Edição cancelada pelo usuário. Os dados não foram modificados.")
    return(invisible(dados))
  }

  cli::cli_alert_success("Dados processados com sucesso!")
  resultado
}

#' @title Addin do RStudio para o dataflowR
#' @description Permite invocar a interface do dataflowR diretamente pelo menu Addins do RStudio.
#' @keywords internal
#' @export
dataflow_addin <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    cli::cli_abort("O RStudio nao esta ativo para executar o Addin.")
  }

  # Tenta capturar o texto selecionado no editor de código
  contexto <- rstudioapi::getActiveDocumentContext()
  texto_sel <- contexto$selection[[1]]$text

  dados_alvo <- NULL
  nome_alvo <- "dados"

  if (nzchar(texto_sel)) {
    tryCatch({
      obj <- get(texto_sel, envir = .GlobalEnv)
      if (is.data.frame(obj)) {
        dados_alvo <- obj
        nome_alvo <- texto_sel
      }
    }, error = function(e) NULL)
  }

  if (is.null(dados_alvo)) {
    # Busca o primeiro data.frame disponível no GlobalEnv
    objs <- ls(envir = .GlobalEnv)
    for (o in objs) {
      val <- get(o, envir = .GlobalEnv)
      if (is.data.frame(val)) {
        dados_alvo <- val
        nome_alvo <- o
        break
      }
    }
  }

  if (is.null(dados_alvo)) {
    cli::cli_alert_warning("Nenhum data.frame encontrado no ambiente. Abrindo com dados de exemplo (mtcars).")
    dados_alvo <- datasets::mtcars
    nome_alvo <- "mtcars"
  }

  dados_tratados <- dataflow(dados_alvo, viewer = "dialog")

  if (!is.null(dados_tratados) && inherits(dados_tratados, "dataflow")) {
    nome_saida <- paste0(nome_alvo, "_tratado")
    assign(nome_saida, dados_tratados, envir = .GlobalEnv)
    cli::cli_alert_success("Objeto {.val {nome_saida}} salvo no ambiente global (.GlobalEnv)!")
  }
}
