#' @title Interface Gráfica Shiny do dataflowR
#' @description Constrói a interface moderna e intuitiva em Bootstrap 5 / bslib.
#' @name app_ui
#' @keywords internal
NULL

#' Criar UI da Aplicação dataflow
#'
#' @return Objeto UI do Shiny.
#' @keywords internal
criar_ui_dataflow <- function() {
  bslib::page_fillable(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "cosmo",
      primary = "#0071e3",
      base_font = bslib::font_google("Inter")
    ),
    title = "dataflowR - Editor Visual de Dados",

    # Estilos customizados modernos (Apple-style / Clean)
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
        body { background-color: #f8fafc; font-family: 'Inter', sans-serif; }
        .brand-text { font-size: 1.5rem; font-weight: 800; letter-spacing: -0.5px; }
        .brand-dark { color: #0f172a; }
        .brand-blue { color: #0071e3; }
        .toolbar-btn {
          border-radius: 8px;
          font-weight: 500;
          font-size: 0.88rem;
          padding: 6px 14px;
          margin-right: 6px;
          margin-bottom: 6px;
          transition: all 0.2s ease;
        }
        .toolbar-btn:hover { transform: translateY(-1px); box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); }
        .metric-badge {
          background-color: #ffffff;
          border: 1px solid #e2e8f0;
          border-radius: 8px;
          padding: 6px 14px;
          font-size: 0.85rem;
          font-weight: 600;
          color: #475569;
          margin-right: 8px;
          box-shadow: 0 1px 2px 0 rgba(0,0,0,0.05);
        }
        .metric-val { color: #0071e3; font-weight: 700; }
        .grid-card {
          background: #ffffff;
          border-radius: 12px;
          border: 1px solid #e2e8f0;
          box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05);
          padding: 12px;
        }
        .handsontable td { font-size: 13px; }
        .handsontable th { font-weight: 600; background-color: #f1f5f9; color: #334155; }
        pre.code-output {
          background-color: #0f172a;
          color: #f8fafc;
          padding: 16px;
          border-radius: 8px;
          font-family: 'Fira Code', 'Consolas', monospace;
          font-size: 0.85rem;
          max-height: 400px;
          overflow-y: auto;
        }
      "))
    ),

    # 1. Cabeçalho Superior
    htmltools::tags$div(
      class = "d-flex justify-content-between align-items-center py-2 px-3 mb-2 bg-white border-bottom shadow-sm",
      htmltools::tags$div(
        class = "d-flex align-items-center",
        htmltools::tags$span(
          class = "brand-text me-4",
          htmltools::tags$span(class = "brand-dark", "data"),
          htmltools::tags$span(class = "brand-blue", "flowR")
        ),
        # Badges métricas
        shiny::uiOutput("metricas_topo", inline = TRUE)
      ),
      htmltools::tags$div(
        class = "d-flex align-items-center",
        shiny::actionButton("btn_ver_codigo", "Ver Código dplyr", class = "btn btn-outline-primary toolbar-btn me-2", icon = shiny::icon("code")),
        shiny::actionButton("btn_desfazer", "Desfazer Última", class = "btn btn-outline-secondary toolbar-btn me-2", icon = shiny::icon("rotate-left")),
        shiny::actionButton("btn_cancelar", "Cancelar", class = "btn btn-light toolbar-btn me-2 text-secondary"),
        shiny::actionButton("btn_concluir", "Concluir e Retornar", class = "btn btn-primary toolbar-btn fw-bold px-3", icon = shiny::icon("check"))
      )
    ),

    # 2. Barra de Ferramentas / Módulos de Engenharia
    htmltools::tags$div(
      class = "d-flex flex-wrap align-items-center px-3 mb-2",
      shiny::actionButton("btn_modal_na", "Tratar NAs", class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("bandage")),
      shiny::actionButton("btn_modal_datas", "Fragmentar Datas", class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("calendar-days")),
      shiny::actionButton("btn_modal_filtro", "Filtrar Linhas", class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("filter")),
      shiny::actionButton("btn_modal_sentinela", "Substituir Sentinelas", class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("arrows-rotate")),
      shiny::actionButton("btn_modal_nova_col", "Nova Coluna", class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("plus")),
      shiny::actionButton("btn_modal_renomear", "Renomear / Tipagem", class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("tag")),
      shiny::actionButton("btn_modal_excluir_col", "Excluir Colunas", class = "btn btn-outline-danger toolbar-btn bg-white", icon = shiny::icon("trash-can"))
    ),

    # 3. Planilha Handsontable Central
    htmltools::tags$div(
      class = "grid-card mx-3 mb-3 flex-grow-1",
      rhandsontable::rHandsontableOutput("tabela_dados", height = "560px")
    )
  )
}
