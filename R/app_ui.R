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
      version    = 5,
      bootswatch = "cosmo",
      primary    = "#0071e3",
      bg         = "#ffffff",
      fg         = "#0f172a",
      base_font  = bslib::font_google("Inter")
    ),
    title = "dataflowR — Editor Visual de Dados",

    # ── Cabeça: fontes, estilos e scripts ──────────────────────────────────────
    htmltools::tags$head(
      # Mermaid para o fluxograma
      htmltools::tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"),
      htmltools::tags$script(htmltools::HTML("
        mermaid.initialize({ startOnLoad: false, theme: 'neutral' });
      ")),
      # Copiar código para clipboard
      htmltools::tags$script(htmltools::HTML("
        function copiarCodigoDataflow() {
          var el = document.getElementById('texto_codigo_dplyr');
          if (!el) return;
          navigator.clipboard.writeText(el.innerText || el.textContent).then(function() {
            var btn = document.getElementById('btn_copiar_clipboard');
            if (!btn) return;
            var orig = btn.innerHTML;
            btn.innerHTML = '<i class=\"fa fa-check me-1\"></i> Copiado!';
            btn.className = btn.className.replace('btn-outline-primary','btn-success');
            setTimeout(function() {
              btn.innerHTML = orig;
              btn.className = btn.className.replace('btn-success','btn-outline-primary');
            }, 2500);
          });
        }

        // Alternar modo tela cheia nativo (Maximizar janela inteira)
        function alternarTelaCheia() {
          if (!document.fullscreenElement) {
            document.documentElement.requestFullscreen().catch(function(err) {});
          } else {
            if (document.exitFullscreen) {
              document.exitFullscreen();
            }
          }
        }
      ")),
      # Duplo clique no cabeçalho da coluna → renomear (event delegation)
      htmltools::tags$script(htmltools::HTML("
        $(document).on('dblclick', '#tabela_dados .colHeader', function() {
          var span  = $(this);
          var th    = span.closest('th');
          var idx   = th.index() - 1; // -1 pelo cabeçalho de linhas
          var nome  = span.text().trim();
          var novo  = window.prompt('Renomear coluna:', nome);
          if (novo && novo.trim() !== '' && novo.trim() !== nome) {
            Shiny.setInputValue('renomear_header_col', {
              col_idx   : idx,
              nome_atual: nome,
              novo_nome : novo.trim()
            }, { priority: 'event' });
          }
        });
      ")),
      # Estilos
      htmltools::tags$style(htmltools::HTML("
        /* ─ Tipografia / Paleta ─ */
        body { background: #f8fafc; font-family: 'Inter', sans-serif; }

        /* ─ Marca ─ */
        .df-brand { font-size: 1.45rem; font-weight: 800; letter-spacing: -0.6px; line-height: 1; white-space: nowrap; }
        .df-brand-data { color: #0f172a; }
        .df-brand-flow { color: #475569; }
        .df-brand-r    { color: #0071e3; }

        /* ─ Métricas ─ */
        .metric-badge {
          display: inline-flex; align-items: center; gap: 4px;
          background: #fff; border: 1px solid #e2e8f0; border-radius: 8px;
          padding: 4px 12px; font-size: 0.83rem; font-weight: 600;
          color: #475569; margin-right: 6px;
          box-shadow: 0 1px 2px rgba(0,0,0,.05);
        }
        .metric-val { color: #0071e3; font-weight: 700; }

        /* ─ Botões da barra ─ */
        .toolbar-btn {
          border-radius: 8px; font-weight: 500; font-size: 0.85rem;
          padding: 5px 13px; margin-right: 5px; margin-bottom: 5px;
          transition: all .18s ease;
        }
        .toolbar-btn:hover { transform: translateY(-1px); box-shadow: 0 4px 6px -1px rgba(0,0,0,.1); }

        /* ─ Grid / Tabela ─ */
        .grid-card {
          background: #fff; border-radius: 12px;
          border: 1px solid #e2e8f0;
          box-shadow: 0 4px 6px -1px rgba(0,0,0,.05); padding: 10px;
        }
        .handsontable td  { font-size: 13px; }
        .handsontable th  { font-weight: 600; background: #f1f5f9 !important; color: #334155; cursor: pointer; }
        .handsontable .colHeader { cursor: pointer; user-select: none; }

        /* ─ Modal: forçar fundo branco / borda neutra ─ */
        .modal-content  { background: #ffffff !important; border: 1px solid #e2e8f0 !important; box-shadow: 0 20px 60px rgba(0,0,0,.15) !important; }
        .modal-header   { background: #ffffff !important; border-bottom: 1px solid #e2e8f0; }
        .modal-footer   { background: #ffffff !important; border-top: 1px solid #e2e8f0; }
        .modal-body     { background: #ffffff !important; }

        /* ─ Código ─ */
        pre.code-output {
          background: #0f172a; color: #f8fafc;
          padding: 16px; border-radius: 10px;
          font-family: 'Fira Code', Consolas, monospace;
          font-size: .87rem; line-height: 1.55;
          max-height: 440px; overflow-y: auto; white-space: pre-wrap;
        }

        /* ─ Cards de histórico ─ */
        .history-card {
          background: #f8fafc; border-left: 4px solid #0071e3;
          border-radius: 6px; padding: 10px 14px; margin-bottom: 10px;
        }

        /* ─ Mermaid ─ */
        .mermaid { width: 100%; }
      "))
    ),

    # ── 1. Cabeçalho Superior ────────────────────────────────────────────────
    htmltools::tags$div(
      class = "d-flex justify-content-between align-items-center py-2 px-3 mb-2 bg-white border-bottom shadow-sm",
      # Marca + métricas
      htmltools::tags$div(
        class = "d-flex align-items-center gap-3",
        htmltools::tags$span(
          class = "df-brand",
          htmltools::tags$span(class = "df-brand-data", "data"),
          htmltools::tags$span(class = "df-brand-flow", "flow"),
          htmltools::tags$span(class = "df-brand-r", "R")
        ),
        shiny::uiOutput("metricas_topo", inline = TRUE)
      ),
      # Botões do lado direito
      htmltools::tags$div(
        class = "d-flex align-items-center flex-wrap",
        shiny::actionButton("btn_ver_codigo",  "Código & Linhagem",
          class = "btn btn-outline-primary toolbar-btn fw-semibold",
          icon  = shiny::icon("code")),
        htmltools::tags$button(
          type    = "button",
          class   = "btn btn-outline-secondary toolbar-btn",
          title   = "Maximizar / Alternar Tela Cheia",
          onclick = "alternarTelaCheia();",
          htmltools::tagList(shiny::icon("up-right-and-down-left-from-center"))
        ),
        shiny::actionButton("btn_desfazer",    "Desfazer",
          class = "btn btn-outline-secondary toolbar-btn",
          icon  = shiny::icon("rotate-left")),
        shiny::actionButton("btn_cancelar",    "Cancelar",
          class = "btn btn-light toolbar-btn text-secondary"),
        shiny::actionButton("btn_concluir",    "Concluir",
          class = "btn btn-primary toolbar-btn fw-bold px-3",
          icon  = shiny::icon("check"))
      )
    ),

    # ── 2. Barra de Ferramentas de Engenharia ───────────────────────────────
    htmltools::tags$div(
      class = "d-flex flex-wrap align-items-center px-3 mb-2",
      shiny::actionButton("btn_modal_na",          "Tratar NAs",
        class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("bandage")),
      shiny::actionButton("btn_modal_datas",        "Fragmentar Datas",
        class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("calendar-days")),
      shiny::actionButton("btn_modal_filtro",       "Filtrar Linhas",
        class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("filter")),
      shiny::actionButton("btn_modal_sentinela",    "Substituir Sentinelas",
        class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("arrows-rotate")),
      shiny::actionButton("btn_modal_nova_col",     "Nova Coluna",
        class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("plus")),
      shiny::actionButton("btn_modal_renomear",     "Renomear / Tipagem",
        class = "btn btn-outline-dark toolbar-btn bg-white", icon = shiny::icon("tag")),
      shiny::actionButton("btn_modal_excluir_col",  "Excluir Colunas",
        class = "btn btn-outline-danger toolbar-btn bg-white", icon = shiny::icon("trash-can"))
    ),

    # ── 3. Planilha Handsontable ─────────────────────────────────────────────
    htmltools::tags$div(
      class = "grid-card mx-3 mb-3",
      rhandsontable::rHandsontableOutput("tabela_dados", height = "560px")
    )
  )
}
