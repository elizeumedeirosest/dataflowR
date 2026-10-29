#' @title Editor Visual e Engenharia de Atributos Interativa
#' @description Inicia a interface gráfica do dataflowR para exploração, edição direta
#'   em planilha, filtragem, imputação de dados ausentes e fragmentação de datas,
#'   gerando código dplyr reproduzível com auditoria completa de transformações.
#'
#' @param dados Um \code{data.frame}, \code{tibble} ou matriz a ser editada.
#' @param viewer Tipo de visualizador do Shiny Gadget:
#'   \itemize{
#'     \item \code{"window"}: Janela desktop nativa do sistema operacional com botões nativos de minimizar, maximizar e fechar (padrão).
#'     \item \code{"browser"}: Abre em aba do navegador web padrão.
#'     \item \code{"dialog"}: Janela modal interna do RStudio.
#'     \item \code{"pane"}: Painel Viewer do RStudio.
#'   }
#' @param largura Largura da janela modal em pixels (quando viewer = "dialog" ou "window"). Padrão: 1200.
#' @param altura Altura da janela modal em pixels (quando viewer = "dialog" ou "window"). Padrão: 750.
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
                     viewer = c("window", "browser", "dialog", "pane"),
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
    "window"  = criar_viewer_janela(largura = largura, altura = altura),
    "browser" = shiny::browserViewer(),
    "dialog"  = shiny::dialogViewer("dataflowR - Editor Visual de Dados", width = largura, height = altura),
    "pane"    = shiny::paneViewer()
  )

  ui     <- criar_ui_dataflow()
  server <- criar_server_dataflow(dados_iniciais = dados, nome_objeto_original = nome_obj)

  # Executa como Shiny Gadget
  resultado <- suppressWarnings(
    shiny::runGadget(ui, server, viewer = gadget_viewer, stopOnCancel = FALSE)
  )

  if (is.null(resultado)) {
    cli::cli_alert_info("Edição cancelada pelo usuário. Os dados não foram modificados.")
    return(invisible(dados))
  }

  cli::cli_alert_success("Dados processados com sucesso!")
  resultado
}

#' Criar Viewer de Janela Nativa Desktop
#'
#' Lança a aplicação como uma janela independente do sistema operacional (Chromium App Mode),
#' fornecendo moldura nativa com botões de minimizar, maximizar e fechar.
#'
#' @param largura Largura inicial da janela.
#' @param altura Altura inicial da janela.
#' @return Função compatível com viewer do Shiny.
#' @keywords internal
criar_viewer_janela <- function(largura = 1200, altura = 750) {
  # Localiza navegadores baseados em Chromium para modo App Window nativo
  candidatos <- c(
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe")
  )

  app_bin <- NULL
  for (cand in candidatos) {
    if (file.exists(cand)) {
      app_bin <- cand
      break
    }
  }

  if (!is.null(app_bin)) {
    shiny::browserViewer(browser = function(url) {
      args <- c(
        paste0("--app=", url),
        paste0("--window-size=", largura, ",", altura)
      )
      system2(app_bin, args = args, wait = FALSE)
    })
  } else {
    shiny::browserViewer()
  }
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

  dados_tratados <- dataflow(dados_alvo, viewer = "window")

  if (!is.null(dados_tratados) && inherits(dados_tratados, "dataflow")) {
    nome_saida <- paste0(nome_alvo, "_tratado")
    assign(nome_saida, dados_tratados, envir = .GlobalEnv)
    cli::cli_alert_success("Objeto {.val {nome_saida}} salvo no ambiente global (.GlobalEnv)!")
  }
}
