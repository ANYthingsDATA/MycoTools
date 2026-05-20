# app.R ─────────────────────────────────────────────────────────────────────
# MycoTools Data Platform
# Built by ANYthings for Mycoteam
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
library(writexl)
library(rlang)
library(parsedate)
library(tools)
library(MycoTools)

options(shiny.maxRequestSize = 100 * 1024^2)

# ── THEME ─────────────────────────────────────────────────────────────────────
# Mycoteam brand colors
BRAND_PRIMARY   <- "#57A19F"   # Mycoteam teal — primary buttons, navbar, active states
BRAND_SECONDARY <- "#C9CC64"   # Mycoteam yellow-green — accents, highlights

# ANYthings brand system — see brand identity docs for full spec
BRAND_BLACK      <- "#1A1A1A"  # ANYthings Black — headings, top accent stripe
BRAND_SIGNAL     <- "#E03C31"  # Signal Red — key callouts, alerts (use sparingly)
BRAND_BLUE       <- "#0B3D91"  # System Blue — links, secondary emphasis, Plotly primary series
BRAND_DARK_GREY  <- "#333333"  # Body text, secondary headings
BRAND_MID_GREY   <- "#767676"  # Captions, metadata, muted text
BRAND_LIGHT_GREY <- "#D9D9D9"  # Rules, borders, table gridlines
BRAND_NEAR_WHITE <- "#F5F5F5"  # Page background, alternate row backgrounds
BRAND_OFF_WHITE  <- "#FAFAFA"  # Card backgrounds, alternate surfaces

app_theme <- bs_theme(
  version   = 5,
  primary   = BRAND_PRIMARY,
  secondary = BRAND_SECONDARY,
  danger    = BRAND_SIGNAL,
  info      = BRAND_BLUE,
  bg        = "#FFFFFF",
  fg        = BRAND_DARK_GREY,
  "navbar-bg"    = BRAND_PRIMARY,
  "navbar-color" = "#FFFFFF",
  "border-color" = BRAND_LIGHT_GREY,
  "card-border-color" = BRAND_LIGHT_GREY,
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter", wght = "700"),
  code_font    = font_google("JetBrains Mono"),
  font_scale   = 0.9
)

# ── CONSTANTS ─────────────────────────────────────────────────────────────────
MEASURE_COLS <- c(
  "Temperature (°C)"      = "gen_temp",
  "Relative Humidity (%)" = "gen_rhum",
  "Wood Moisture (%)"     = "gen_wood",
  "Ohm"                   = "gen_ohm"
)

MIX_COLS <- c(
  "MYCOindex — Mold" = "MIx_mold",
  "MYCOindex — Temp" = "MIx_temp",
  "MYCOindex — Wood" = "MIx_wood"
)

ALL_GEN_COLS <- c(MEASURE_COLS, MIX_COLS)

# Colour scale for MYCOindex heatmap: dark green → light green → yellow → red
MIX_COLORSCALE <- list(
  list(0,    "#2E7D32"),   # 0   — dark green (no risk)
  list(0.25, "#81C784"),   # 0.25 — light green (low risk)
  list(0.5,  "#FDD835"),   # 0.5  — yellow (moderate risk)
  list(1,    "#C62828")    # 1    — red (high risk)
)

# Data visualisation palette (NYCTA-inspired) — cycles by sensor index
NYCTA_PALETTE <- c(
  "#0B3D91",  # System Blue
  "#E03C31",  # Signal Red
  "#00933C",  # Transit Green
  "#FF6319",  # Line Orange
  "#6E267B"   # Line Purple
)

# Per-index colours for the MYCOindex distribution overlay
MIX_HIST_COLORS <- c(
  MIx_mold = "#0B3D91",
  MIx_temp = "#FF6319",
  MIx_wood = "#00933C"
)

# ── HELPERS ───────────────────────────────────────────────────────────────────
empty_plotly <- function(msg = "No data to display") {
  plot_ly() %>%
    layout(
      title  = list(text = msg, x = 0.5, font = list(color = "#999", size = 13)),
      xaxis  = list(visible = FALSE),
      yaxis  = list(visible = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
}

col_label <- function(col_id, lookup = ALL_GEN_COLS) {
  nm <- names(lookup)[lookup == col_id]
  if (length(nm)) nm[[1]] else col_id
}

# Standard Plotly layout — Inter font, JetBrains Mono ticks, #D9D9D9 gridlines
apply_plotly_theme <- function(p,
                               xaxis_title = "",
                               yaxis_title = "",
                               hovermode   = "x unified",
                               show_legend = TRUE) {
  plotly::layout(p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(family = "Inter, Helvetica Neue, Arial",
                size   = 10, color = "#555555"),
    xaxis = list(
      title    = xaxis_title,
      showgrid = TRUE, gridcolor = "#D9D9D9", gridwidth = 0.5,
      zeroline = FALSE, linecolor = "#333333", linewidth = 1,
      tickfont = list(family = "JetBrains Mono", size = 9)
    ),
    yaxis = list(
      title    = yaxis_title,
      showgrid = TRUE, gridcolor = "#D9D9D9", gridwidth = 0.5,
      zeroline = FALSE, linecolor = "#333333", linewidth = 1,
      tickfont = list(family = "JetBrains Mono", size = 9)
    ),
    hovermode = hovermode,
    showlegend = show_legend,
    legend = list(orientation = "h", y = -0.18, font = list(size = 9)),
    margin = list(t = 20, r = 20, b = 70, l = 54)
  )
}

# Cycle NYCTA palette by 1-indexed sensor slot
nycta_color <- function(i) {
  NYCTA_PALETTE[((i - 1L) %% length(NYCTA_PALETTE)) + 1L]
}

# Dashboard stat tile — label / value / unit / sub
stat_tile <- function(label, value, unit = NULL, sub = NULL, alert = FALSE) {
  val_color <- if (isTRUE(alert)) BRAND_SIGNAL else BRAND_BLACK
  div(
    class = "stat-tile",
    div(class = "stat-tile-label", label),
    div(
      class = "stat-tile-value",
      style = paste0("color: ", val_color, ";"),
      value,
      if (!is.null(unit)) tags$span(class = "stat-tile-unit", unit)
    ),
    if (!is.null(sub)) div(class = "stat-tile-sub", sub)
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
# Global CSS overrides — NASA/NYCTA aesthetic: zero border-radius, section labels,
# sidebar nav, DT styling, stat tiles, upload dropzone, active-tab treatments.
app_css <- HTML('
  /* Top accent stripe (3px black bar above the navbar) */
  body::before {
    content: ""; display: block;
    position: fixed; top: 0; left: 0; right: 0; height: 3px;
    background: #1A1A1A; z-index: 9999;
  }
  body { background: #F5F5F5; padding-top: 3px; }

  /* Remove all border-radius (NASA aesthetic) */
  .card, .nav-tabs .nav-link, .nav-pills .nav-link, .btn, select, input,
  .form-control, .form-select, .input-group, .input-group > *,
  .dataTables_wrapper, .shiny-notification, .alert, .badge,
  .shiny-input-container .form-control,
  .bslib-value-box, .bslib-card { border-radius: 0 !important; }

  /* Section labels (9px uppercase, mid grey) */
  .section-label, .small-caps-label {
    font-size: 9px; font-weight: 700; color: #767676;
    text-transform: uppercase; letter-spacing: 0.08em; margin-bottom: 6px;
  }

  /* Sidebar h6 section labels */
  .bslib-sidebar-layout .sidebar h6.text-uppercase {
    font-size: 9px !important; font-weight: 700; color: #767676;
    letter-spacing: 0.08em; margin: 14px 0 6px;
  }

  /* Main navbar: active tab white bg / teal text */
  .navbar-nav .nav-link.active,
  .navbar-nav .show > .nav-link {
    background: #FFFFFF !important;
    color: #57A19F !important;
    font-weight: 700;
  }
  .navbar-nav .nav-link { color: #FFFFFF; }

  /* Sub-tabs inside cards: black active, white text */
  .nav-tabs .nav-link.active {
    background: #1A1A1A !important; color: #FFFFFF !important;
    border-color: #1A1A1A !important; border-radius: 0 !important;
    font-weight: 700;
  }
  .nav-tabs .nav-link {
    border-radius: 0 !important; font-size: 11px; color: #555555;
  }

  /* DT tables */
  table.dataTable thead th {
    background: #F5F5F5 !important; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.05em; font-size: 10px;
  }
  table.dataTable tbody tr:nth-child(even) td { background: #FAFAFA; }
  table.dataTable td, table.dataTable th {
    border-bottom: 0.5px solid #D9D9D9 !important;
  }
  table.dataTable.table-sm td, table.dataTable.table-sm th {
    padding: 6px 8px; font-size: 11px;
  }

  /* bslib card headers */
  .card-header {
    font-size: 11px; font-weight: 700; background: #FFFFFF;
    border-bottom: 1px solid #D9D9D9; text-transform: uppercase;
    letter-spacing: 0.05em; color: #1A1A1A;
  }
  .card { background: #FFFFFF; }

  /* Scrollbars */
  ::-webkit-scrollbar { width: 5px; height: 5px; }
  ::-webkit-scrollbar-track { background: #F5F5F5; }
  ::-webkit-scrollbar-thumb { background: #D9D9D9; }

  /* Stat tiles (dashboard) */
  .stat-tile {
    border: 1px solid #D9D9D9; background: #FFFFFF; padding: 14px 16px;
    height: 100%;
  }
  .stat-tile-label {
    font-size: 9px; font-weight: 700; color: #767676;
    text-transform: uppercase; letter-spacing: 0.07em; margin-bottom: 6px;
  }
  .stat-tile-value {
    font-size: 26px; font-weight: 700; letter-spacing: -0.02em;
    line-height: 1; color: #1A1A1A;
  }
  .stat-tile-unit { font-size: 11px; color: #555555; margin-left: 4px; }
  .stat-tile-sub  { font-size: 9px; color: #767676; margin-top: 4px; }

  /* Upload dropzone — wraps fileInput */
  .upload-dropzone .form-group {
    border: 1.5px dashed #D9D9D9; background: #F5F5F5;
    padding: 18px 12px; text-align: center; margin-bottom: 0;
  }
  .upload-dropzone .form-group label { color: #767676; font-size: 11px; }

  /* Outline and filled badges for upload summary */
  .badge-outline {
    display: inline-block; font-size: 9px; font-weight: 700;
    padding: 3px 8px; text-transform: uppercase; letter-spacing: 0.05em;
    background: #F5F5F5; color: #333333; border: 1px solid #D9D9D9;
    border-radius: 0; margin-right: 6px;
  }
  .badge-filled {
    display: inline-block; font-size: 9px; font-weight: 700;
    padding: 3px 8px; text-transform: uppercase; letter-spacing: 0.05em;
    background: #57A19F; color: #FFFFFF; border-radius: 0; margin-right: 6px;
  }

  /* Animated processing bar */
  .shiny-progress .progress-bar { background: #57A19F !important; }

  /* Success alert tuned to design green */
  .alert-success {
    background: #E8F5E9; border: 1px solid #A5D6A7; color: #2E7D32;
    border-radius: 0 !important;
  }

  /* Column reference table (Download tab) */
  .colref-table { width: 100%; border-collapse: collapse; font-size: 11px; }
  .colref-table tr { border-bottom: 0.5px solid #D9D9D9; }
  .colref-table tbody tr:nth-child(odd) td  { background: #FAFAFA; }
  .colref-table tbody tr:nth-child(even) td { background: #FFFFFF; }
  .colref-table td { padding: 7px 10px; vertical-align: middle; }
  .colref-table td.col-name {
    font-family: "JetBrains Mono", monospace; color: #0B3D91; font-size: 11px;
  }
  .colref-table td.col-type {
    font-family: "JetBrains Mono", monospace; color: #555555; font-size: 10px;
  }
  .colref-table thead th {
    font-size: 9px; font-weight: 700; text-transform: uppercase;
    letter-spacing: 0.08em; color: #767676; background: #F5F5F5;
    padding: 8px 10px; text-align: left; border-bottom: 1px solid #D9D9D9;
  }

  /* Footer */
  .app-footer .foot-left {
    font-size: 9px; font-weight: 700; color: #1A1A1A;
    letter-spacing: 0.05em; text-transform: uppercase;
  }
  .app-footer .foot-right { font-size: 9px; color: #767676; }
  .app-footer .foot-count {
    font-family: "JetBrains Mono", monospace; font-size: 9px; color: #767676;
    margin-left: 10px;
  }
')

ui <- page_navbar(
  id      = "main_nav",
  # TODO: replace tags$strong("MycoTools") with:
  #   tags$span(
  #     tags$img(src = "mycoteam_logo.png", height = "28px", alt = "Mycoteam",
  #              style = "vertical-align: middle; margin-right: 8px;"),
  #     tags$span("MycoTools", style = "color: #FFFFFF; font-weight: 700;")
  #   )
  # Logo files go in www/ — Shiny serves that folder as the static root.
  # Use any_logo_white_back_v3.png (horizontal) for the footer.
  # Use square_any_logo_white_back_v3.png for the favicon / app icon.
  title   = tags$strong("MycoTools", style = "color: #FFFFFF; letter-spacing: -0.3px;"),
  theme   = app_theme,
  inverse = TRUE,
  header  = tags$head(
    tags$link(rel = "icon", href = "square_any_logo_white_back_v3.png"),
    tags$style(app_css),
    tags$script(HTML("
      // Clipboard copy — triggered by sendCustomMessage('copy_to_clipboard', text)
      $(function() {
        Shiny.addCustomMessageHandler('copy_to_clipboard', function(text) {
          var done = function(ok) {
            Shiny.setInputValue(ok ? 'log_copy_ok' : 'log_copy_fail',
                                Math.random(), {priority: 'event'});
          };
          if (navigator.clipboard && navigator.clipboard.writeText) {
            navigator.clipboard.writeText(text).then(
              function() { done(true); },
              function() { done(false); }
            );
          } else {
            var ta = document.createElement('textarea');
            ta.value = text;
            ta.style.position = 'fixed';
            ta.style.left = '-9999px';
            document.body.appendChild(ta);
            ta.select();
            var ok = false;
            try { ok = document.execCommand('copy'); } catch(e) { ok = false; }
            document.body.removeChild(ta);
            done(ok);
          }
        });

        // Capture output render errors (bubble to server log)
        $(document).on('shiny:error', function(event) {
          var err = event.error || {};
          // Skip silent errors from req() / validate() — control flow, not failures.
          // err.type may be a string or an array of strings.
          var types = [].concat(err.type || []);
          if (types.indexOf('shiny.silent.error') !== -1 ||
              types.indexOf('validation') !== -1 ||
              types.indexOf('shiny.custom.error') !== -1) return;
          var msg = err.message || (typeof err === 'string' ? err : JSON.stringify(err));
          Shiny.setInputValue('log_render_error',
            { target: event.name || 'output', message: msg, at: Date.now() },
            { priority: 'event' });
        });

        // Capture uncaught JS errors
        window.addEventListener('error', function(e) {
          Shiny.setInputValue('log_js_error',
            { message: e.message || String(e),
              source:  e.filename || '',
              line:    e.lineno || 0,
              at:      Date.now() },
            { priority: 'event' });
        });
      });
    "))
  ),
  footer  = div(
    class = "app-footer d-flex align-items-center justify-content-between px-3 py-2",
    style = "border-top: 1px solid #D9D9D9; background: #FFFFFF;",
    # Primary brand — Mycoteam (left)
    # TODO: replace the text span with:
    #   tags$img(src = "mycoteam_logo.png", height = "20px", alt = "Mycoteam")
    tags$span(class = "foot-left", "MYCOTEAM"),

    # Right cluster: ANYthings credit + reactive row/col counter
    div(
      class = "d-flex align-items-center",
      tags$span(class = "foot-right", "Built by ANYthings · MycoTools v1.0"),
      # TODO: add ANYthings logo beside the credit line:
      #   tags$img(src = "any_logo_white_back_v3.png", height = "20px",
      #            alt = "ANYthings", style = "margin-left: 8px;")
      tags$span(class = "foot-count", textOutput("footer_count", inline = TRUE))
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # TAB 1 — UPLOAD
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(icon("upload"), " Upload"),
    value = "tab_upload",

    layout_columns(
      col_widths = c(4, 8),

      # ── Import controls ──
      card(
        card_header(icon("file-import", class = "me-1"), "Import File"),
        card_body(
          div(class = "section-label", "Source File"),
          div(class = "upload-dropzone",
              fileInput("file_upload", "Drop CSV / Excel / TXT here",
                        accept      = c(".csv", ".txt", ".xlsx", ".xls"),
                        buttonLabel = "Browse…",
                        placeholder = "No file selected")
          ),

          hr(),
          div(class = "section-label", "Parse Options"),
          selectInput("import_delim", "Delimiter",
                      choices  = c("Auto-detect" = "auto",
                                   "Comma  (,)"  = ",",
                                   "Semicolon (;)" = ";",
                                   "Tab  (\\t)"  = "\t"),
                      selected = "auto", width = "100%"),
          numericInput("import_skip",    "Skip lines at top", value = 0, min = 0),
          textInput("import_comment",    "Comment prefix",    value = "",
                    placeholder = "e.g.  #  — leave empty to disable"),
          numericInput("import_sheet",   "Excel sheet",       value = 1, min = 1),
          hr(),
          actionButton("btn_import", "Import",
                       icon  = icon("file-import"),
                       class = "btn-primary w-100"),
          uiOutput("import_status_ui")
        )
      ),

      # ── Preview ──
      navset_card_tab(
        nav_panel(
          title = tagList(icon("file-lines", class = "me-1"), " Raw file"),
          card_body(
            uiOutput("raw_hint_ui"),
            verbatimTextOutput("raw_preview")
          )
        ),
        nav_panel(
          title = tagList(
            icon("table", class = "me-1"), " Imported data",
            span(class = "ms-2", uiOutput("upload_badge_ui", inline = TRUE))
          ),
          card_body(
            uiOutput("upload_hint_ui"),
            DTOutput("tbl_preview")
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # TAB 2 — CONFIGURE & PROCESS
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(icon("sliders"), " Configure"),
    value = "tab_configure",

    layout_columns(
      col_widths = c(4, 8),

      # ── Left: configuration panels ──
      navset_card_tab(

        # ── Columns ──
        nav_panel(
          title = tagList(icon("columns"), " Columns"),
          p(class = "text-muted small mt-1 mb-3",
            "Map raw column names to the standard variables used downstream."),

          h6(class = "text-uppercase text-muted small mb-1", "Date / Time"),
          selectInput("map_date", "Date column *",
                      choices = c("— select after import —" = ""), width = "100%"),
          selectInput("map_time", "Time column (if separate from date)",
                      choices = c("— None —" = ""), width = "100%"),
          selectInput("map_timezone", "Timezone",
                      choices  = c("UTC", "Europe/Oslo", "Europe/Stockholm",
                                   "Europe/Copenhagen", "Europe/Berlin", "Europe/London"),
                      selected = "Europe/Oslo", width = "100%"),

          h6(class = "text-uppercase text-muted small mb-1 mt-3", "Sensor"),
          selectInput("map_sensor", "Sensor ID column",
                      choices = c("— None —" = ""), width = "100%"),
          selectInput("map_port", "Port column (appended to sensor ID)",
                      choices = c("— None —" = ""), width = "100%"),
          conditionalPanel(
            condition = "input.map_sensor == ''",
            textInput("manual_sensor_id", "Manual sensor label",
                      placeholder = "e.g. Sensor_01 — used when no column is selected",
                      width = "100%")
          ),

          h6(class = "text-uppercase text-muted small mb-1 mt-3", "Measurements"),
          selectInput("map_temp", "Temperature",       choices = c("— None —" = ""), width = "100%"),
          selectInput("map_rhum", "Relative Humidity", choices = c("— None —" = ""), width = "100%"),
          selectInput("map_wood", "Wood Moisture",     choices = c("— None —" = ""), width = "100%"),
          selectInput("map_ohm",  "Ohm",               choices = c("— None —" = ""), width = "100%")
        ),

        # ── Date completion ──
        nav_panel(
          title = tagList(icon("calendar-check"), " Dates"),
          p(class = "text-muted small mt-1 mb-3",
            "Fill missing timestamps to create a regular time series."),

          checkboxInput("do_complete", "Complete missing dates", value = FALSE),

          conditionalPanel(
            "input.do_complete == true",
            selectInput("map_site_id", "Site ID column (optional grouping)",
                        choices = c("— None —" = ""), width = "100%"),
            selectInput("complete_timeframe", "Resolution",
                        choices  = c("Hourly" = "hour", "Daily" = "day",
                                     "Weekly" = "week", "Monthly" = "month"),
                        selected = "day", width = "100%"),
            div(class = "alert alert-warning small py-2 mt-2",
                icon("triangle-exclamation", class = "me-1"),
                "Resolution must match your data's actual sampling interval.")
          )
        ),

        # ── MYCOindex ──
        nav_panel(
          title = tagList(icon("virus"), " MYCOindex"),
          p(class = "text-muted small mt-1 mb-2",
            "Select which indexes to compute and adjust thresholds if needed."),

          checkboxGroupInput("do_indexes", NULL,
                             choices  = c("Mold (from RH)"  = "mold",
                                          "Temperature"     = "temp",
                                          "Wood Moisture"   = "wood"),
                             selected = c("mold", "temp", "wood")),

          hr(),
          tags$b("Mold — Relative Humidity (%)"),
          fluidRow(
            column(4, numericInput("mold_low",  "Low",  75, width = "100%")),
            column(4, numericInput("mold_mid",  "Mid",  85, width = "100%")),
            column(4, numericInput("mold_high", "High", 95, width = "100%"))
          ),
          hr(),
          tags$b("Temperature (°C)"),
          fluidRow(
            column(3, numericInput("temp_low",  "Low",   4, width = "100%")),
            column(3, numericInput("temp_mid",  "Mid",   8, width = "100%")),
            column(3, numericInput("temp_high", "High", 14, width = "100%")),
            column(3, numericInput("temp_max",  "Max",  35, width = "100%"))
          ),
          hr(),
          tags$b("Wood Moisture (%)"),
          fluidRow(
            column(3, numericInput("wood_low",  "Low",   20, width = "100%")),
            column(3, numericInput("wood_mid",  "Mid",   25, width = "100%")),
            column(3, numericInput("wood_high", "High",  30, width = "100%")),
            column(3, numericInput("wood_max",  "Max",  100, width = "100%"))
          )
        ),

        # ── Extras ──
        nav_panel(
          title = tagList(icon("plus-circle"), " Extras"),
          checkboxInput("do_seasons", "Add season & calendar variables", value = TRUE),
          tags$ul(class = "text-muted small mt-2",
            tags$li(code("gen_season"), " — Winter / Spring / Summer / Fall"),
            tags$li(code("gen_year_season"), " — e.g. '2024, Winter'"),
            tags$li(code("gen_isoweek"), " / ", code("gen_isoyear"),
                    " — ISO 8601 week"),
            tags$li(code("gen_date_month_num"), " / ",
                    code("gen_date_month_lab"))
          )
        )
      ),

      # ── Right: run + output ──
      card(
        card_header(icon("gears", class = "me-1"), "Run Processing"),
        card_body(
          uiOutput("process_prereq_ui"),
          actionButton("btn_process", "Run Processing",
                       icon  = icon("gears"),
                       class = "btn-primary w-100"),
          br(), br(),
          uiOutput("process_status_ui"),
          hr(),
          h6(class = "fw-bold", "Output Preview (first 200 rows)"),
          DTOutput("tbl_processed")
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # TAB 3 — DASHBOARD
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(icon("chart-line"), " Dashboard"),
    value = "tab_dashboard",

    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        title = "Filters",
        open  = TRUE,

        # Sensor checkboxes — populated dynamically
        uiOutput("dash_sensor_ui"),

        # Sensor rename inputs — populated dynamically
        uiOutput("sensor_rename_ui"),

        hr(),

        # Date range — populated dynamically
        uiOutput("dash_daterange_ui"),

        hr(),

        # Variable selectors — updated via observe() when data is ready
        selectInput("dash_primary_var", "Primary variable",
                    choices = character(0), width = "100%"),

        checkboxGroupInput("dash_mix_overlay", "Overlay MYCOindex",
                           choices  = MIX_COLS,
                           selected = character(0)),

        hr(),

        selectInput("heatmap_index", "Heatmap: MYCOindex",
                    choices = MIX_COLS, selected = "MIx_mold", width = "100%")
      ),

      uiOutput("dashboard_main_ui")
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # TAB 4 — DOWNLOAD
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(icon("download"), " Download"),
    value = "tab_download",

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header(icon("file-export", class = "me-1"), "Export"),
        card_body(
          uiOutput("dl_prereq_ui"),
          selectInput("dl_format", "Format",
                      choices = c("CSV (comma)"       = "csv",
                                  "CSV2 (semicolon)"  = "csv2",
                                  "Excel (.xlsx)"     = "xlsx"),
                      width = "100%"),
          textInput("dl_filename", "Filename (without extension)",
                    value = paste0("mycotools_", format(Sys.time(), "%Y-%m-%d-%H-%M")),
                    width = "100%"),
          downloadButton("btn_download", "Download Processed Data",
                         class = "btn-primary w-100 mt-2")
        )
      ),

      card(
        card_header(icon("book", class = "me-1"), "Generated Column Reference"),
        card_body(
          local({
            rows <- list(
              c("gen_datetime",    "POSIXct", "Standardised datetime"),
              c("gen_date",        "Date",    "Date component"),
              c("gen_time",        "chr",     "Time HH:MM:SS"),
              c("gen_sensorID",    "chr",     "Sensor identifier"),
              c("gen_temp",        "dbl",     "Temperature (°C)"),
              c("gen_rhum",        "dbl",     "Relative humidity (%)"),
              c("gen_wood",        "dbl",     "Wood moisture (%)"),
              c("gen_ohm",         "dbl",     "Ohm value"),
              c("MIx_mold",        "dbl",     "Mold index (0 / 0.25 / 0.5 / 1)"),
              c("MIx_temp",        "dbl",     "Temperature index (0 / 0.2 / 0.4 / 1)"),
              c("MIx_wood",        "dbl",     "Wood index (0 / 0.25 / 0.5 / 1)"),
              c("gen_season",      "chr",     "Winter / Spring / Summer / Fall"),
              c("gen_year_season", "chr",     "e.g. '2024, Winter'"),
              c("gen_isoweek",     "int",     "ISO 8601 week number"),
              c("gen_isoyear",     "int",     "ISO year")
            )
            tags$table(
              class = "colref-table",
              tags$thead(tags$tr(
                tags$th("Column"), tags$th("Type"), tags$th("Description")
              )),
              tags$tbody(
                lapply(rows, function(r) {
                  tags$tr(
                    tags$td(class = "col-name", r[[1]]),
                    tags$td(class = "col-type", r[[2]]),
                    tags$td(r[[3]])
                  )
                })
              )
            )
          })
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # TAB 5 — LOG
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(icon("bug"), " Log"),
    value = "tab_log",

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header(icon("circle-info", class = "me-1"), "Report an Issue"),
        card_body(
          p(class = "small text-muted mt-1",
            "Errors and warnings from this session are recorded here. ",
            "If something goes wrong, use ", tags$b("Copy"), " or ",
            tags$b("Download"), " to capture the log and send it to ",
            "support so the issue can be diagnosed."),
          hr(),
          div(class = "section-label", "Filter"),
          radioButtons("log_filter", NULL,
                       choices  = c("All events"   = "all",
                                    "Warnings +"   = "warn",
                                    "Errors only"  = "error"),
                       selected = "all"),
          hr(),
          div(class = "section-label", "Actions"),
          actionButton("log_copy", "Copy to clipboard",
                       icon  = icon("copy"),
                       class = "btn-primary w-100 mb-2"),
          downloadButton("log_download", "Download as text",
                         class = "btn-outline-primary w-100 mb-2"),
          actionButton("log_clear", "Clear log",
                       icon  = icon("trash"),
                       class = "btn-outline-secondary w-100")
        )
      ),

      card(
        card_header(
          icon("list", class = "me-1"), "Event Log",
          span(class = "ms-2", uiOutput("log_badge_ui", inline = TRUE))
        ),
        card_body(
          tags$style(HTML("
            #log_view {
              height: 560px; overflow: auto;
              font-family: 'JetBrains Mono', monospace;
              font-size: 11px; line-height: 1.45;
              background: #FAFAFA; border: 1px solid #D9D9D9;
              padding: 10px 12px; white-space: pre;
            }
            .log-badge-error { background: #E03C31; color: #FFFFFF;
                               display: inline-block; font-size: 9px;
                               font-weight: 700; padding: 3px 8px;
                               text-transform: uppercase; letter-spacing: 0.05em;
                               margin-right: 6px; }
          ")),
          verbatimTextOutput("log_view", placeholder = TRUE)
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small opacity-75",
                     "ANYthings for Mycoteam"))
)


# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    raw_data       = NULL,
    processed_data = NULL,
    log_entries    = list()
  )

  # ════════════════════════════════════════════════════════════════
  # LOGGING — user-visible event/error log (see Log tab)
  # ════════════════════════════════════════════════════════════════

  log_event <- function(level, where, message, details = NULL) {
    entry <- list(
      time    = Sys.time(),
      level   = level,
      where   = where,
      message = as.character(message),
      details = if (is.null(details)) NULL else as.character(details)
    )
    isolate(rv$log_entries <- c(rv$log_entries, list(entry)))
  }

  # Run `expr` with error + warning capture. Errors are logged, shown as a
  # notification, and swallowed (returns invisible(NULL)). Warnings are
  # logged and muffled from the console.
  with_log <- function(where, expr) {
    trace_text <- NULL
    withCallingHandlers(
      tryCatch(
        force(expr),
        error = function(e) {
          log_event("error", where, conditionMessage(e), details = trace_text)
          showNotification(sprintf("%s — %s", where, conditionMessage(e)),
                           type = "error", duration = 15)
          invisible(NULL)
        }
      ),
      error = function(e) {
        # Capture the call stack at the point of the error (before unwind)
        calls <- sys.calls()
        trace_text <<- paste(
          vapply(calls, function(c) paste(deparse(c), collapse = " "),
                 character(1)),
          collapse = "\n"
        )
      },
      warning = function(w) {
        log_event("warn", where, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }

  log_system_info <- function() {
    ua <- tryCatch(session$request$HTTP_USER_AGENT, error = function(e) NULL)
    if (is.null(ua) || !nzchar(ua)) ua <- "unknown"
    paste(
      "---- MycoTools diagnostic report ----",
      sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      sprintf("R:         %s.%s", R.version$major, R.version$minor),
      sprintf("Shiny:     %s", as.character(utils::packageVersion("shiny"))),
      sprintf("bslib:     %s", as.character(utils::packageVersion("bslib"))),
      sprintf("OS:        %s %s", Sys.info()[["sysname"]], Sys.info()[["release"]]),
      sprintf("Browser:   %s", ua),
      sep = "\n"
    )
  }

  format_log_entries <- function(entries, filter = "all") {
    if (!length(entries)) return("(no events recorded)")
    lvl_rank <- c(info = 1L, warn = 2L, error = 3L)
    min_rank <- switch(filter, "error" = 3L, "warn" = 2L, 1L)
    filtered <- Filter(function(e) lvl_rank[[e$level]] >= min_rank, entries)
    if (!length(filtered)) return("(no matching events)")

    lines <- vapply(filtered, function(e) {
      ts  <- format(e$time, "%Y-%m-%d %H:%M:%S")
      lvl <- sprintf("%-5s", toupper(e$level))
      hdr <- sprintf("[%s] %s %s — %s", ts, lvl, e$where, e$message)
      if (!is.null(e$details) && nzchar(e$details)) {
        det <- paste(strsplit(e$details, "\n", fixed = TRUE)[[1]],
                     collapse = "\n    ")
        paste0(hdr, "\n    ", det)
      } else {
        hdr
      }
    }, character(1))
    paste(lines, collapse = "\n")
  }

  # Opening marker
  log_event("info", "session", sprintf(
    "Session started · R %s.%s · Shiny %s · %s",
    R.version$major, R.version$minor,
    as.character(utils::packageVersion("shiny")),
    Sys.info()[["sysname"]]
  ))

  # Capture client-side render errors (wired via shiny:error JS handler)
  observeEvent(input$log_render_error, {
    err <- input$log_render_error
    log_event("error", paste0("render:", err$target %||% "output"),
              err$message %||% "(no message)")
  }, ignoreInit = TRUE)

  # Capture uncaught JS errors
  observeEvent(input$log_js_error, {
    err <- input$log_js_error
    where <- if (nzchar(err$source %||% "")) sprintf("js:%s:%s", err$source, err$line) else "js"
    log_event("error", where, err$message %||% "(no message)")
  }, ignoreInit = TRUE)

  # Reactive row/col footer counter — prefers processed, falls back to raw
  output$footer_count <- renderText({
    d <- rv$processed_data %||% rv$raw_data
    if (is.null(d)) return("")
    sprintf("%s rows × %s cols", format(nrow(d), big.mark = ","), ncol(d))
  })

  # ════════════════════════════════════════════════════════════════
  # UPLOAD
  # ════════════════════════════════════════════════════════════════

  output$raw_hint_ui <- renderUI({
    if (is.null(input$file_upload)) {
      div(class = "alert alert-info",
          icon("circle-info", class = "me-1"),
          "Select a file to preview its raw content.")
    }
  })

  output$raw_preview <- renderText({
    req(input$file_upload)
    lines <- tryCatch(
      readLines(input$file_upload$datapath, n = 100, warn = FALSE, encoding = "UTF-8"),
      error = function(e) paste("Could not read file:", conditionMessage(e))
    )
    paste(lines, collapse = "\n")
  })

  output$upload_hint_ui <- renderUI({
    if (is.null(rv$raw_data)) {
      div(class = "alert alert-info",
          icon("circle-info", class = "me-1"),
          "Select a file and click ", tags$strong("Import"), " to load your data.")
    }
  })

  observeEvent(input$btn_import, {
    req(input$file_upload)

    with_log("import", {
      comment_arg <- trimws(input$import_comment)
      comment_arg <- if (nzchar(comment_arg)) comment_arg else NULL

      data <- import_data(
        path    = input$file_upload$datapath,
        sheet   = as.integer(input$import_sheet),
        delim   = input$import_delim,
        skip    = as.integer(input$import_skip),
        comment = comment_arg
      )

      rv$raw_data       <- data
      rv$processed_data <- NULL

      # Populate column selectors on Configure tab
      col_choices <- c("— None —" = "", setNames(names(data), names(data)))
      for (id in c("map_date", "map_time", "map_sensor", "map_port",
                   "map_temp", "map_rhum", "map_wood", "map_ohm", "map_site_id")) {
        updateSelectInput(session, id, choices = col_choices)
      }

      # Auto-detect date column
      date_guess <- grep("date|time|datum|dato|timestamp",
                         names(data), ignore.case = TRUE, value = TRUE)[1]
      if (!is.na(date_guess)) {
        updateSelectInput(session, "map_date", selected = date_guess)
      }

      log_event("info", "import",
                sprintf("Imported %s rows × %s cols (file: %s, delim: %s)",
                        format(nrow(data), big.mark = ","),
                        ncol(data),
                        input$file_upload$name,
                        input$import_delim))

      showNotification(
        sprintf("Imported %s rows × %s columns",
                format(nrow(data), big.mark = ","), ncol(data)),
        type = "message"
      )
    })
  })

  output$upload_badge_ui <- renderUI({
    req(rv$raw_data)
    d <- rv$raw_data

    # Outlined badges: cols, possible sensor count, date range (best-effort)
    sensor_badge <- NULL
    sensor_guess <- grep("sensor|id|logger", names(d), ignore.case = TRUE,
                         value = TRUE)[1]
    if (!is.na(sensor_guess)) {
      n_sens <- length(unique(d[[sensor_guess]]))
      sensor_badge <- tags$span(class = "badge-outline",
                                sprintf("%s sensors", n_sens))
    }

    date_badge <- NULL
    date_guess <- grep("date|time|datum|dato|timestamp", names(d),
                       ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(date_guess)) {
      parsed <- suppressWarnings(parsedate::parse_date(d[[date_guess]]))
      if (any(!is.na(parsed))) {
        rng <- range(parsed, na.rm = TRUE)
        date_badge <- tags$span(
          class = "badge-outline",
          sprintf("%s → %s",
                  format(as.Date(rng[1]), "%Y-%m-%d"),
                  format(as.Date(rng[2]), "%Y-%m-%d"))
        )
      }
    }

    tagList(
      tags$span(class = "badge-filled",
                sprintf("%s rows", format(nrow(d), big.mark = ","))),
      tags$span(class = "badge-outline", sprintf("%s cols", ncol(d))),
      sensor_badge,
      date_badge
    )
  })

  output$import_status_ui <- renderUI({
    req(rv$raw_data)
    d <- rv$raw_data
    div(class = "alert alert-success py-2 mt-3",
        icon("circle-check", class = "me-1"),
        sprintf("Imported %s rows × %s columns",
                format(nrow(d), big.mark = ","), ncol(d)))
  })

  output$tbl_preview <- renderDT({
    req(rv$raw_data)
    datatable(head(rv$raw_data, 200),
              options  = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
              rownames = FALSE,
              class    = "table-sm table-striped")
  })


  # ════════════════════════════════════════════════════════════════
  # PROCESS
  # ════════════════════════════════════════════════════════════════

  output$process_prereq_ui <- renderUI({
    if (is.null(rv$raw_data)) {
      div(class = "alert alert-warning mb-2",
          icon("triangle-exclamation", class = "me-1"),
          "Upload data first (Upload tab).")
    } else if (!nzchar(input$map_date)) {
      div(class = "alert alert-warning mb-2",
          icon("triangle-exclamation", class = "me-1"),
          "Select a date column in the Columns tab.")
    }
  })

  observeEvent(input$btn_process, {
    req(rv$raw_data, nzchar(input$map_date))

    withProgress(message = "Processing data…", value = 0, {
      with_log("process", {
        data <- rv$raw_data

        # ── 1. Date / time ──────────────────────────────────────────
        incProgress(0.10, detail = "Parsing dates…")

        if (nzchar(input$map_time)) {
          data <- define_variables_datetime(
            data,
            input_date = input$map_date,
            input_time = input$map_time,
            tz = input$map_timezone
          )
        } else {
          data <- define_variables_date(
            data,
            input_date = input$map_date,
            tz = input$map_timezone
          )
        }

        # ── 2. Sensor ID ────────────────────────────────────────────
        if (nzchar(input$map_sensor)) {
          if (nzchar(input$map_port)) {
            data <- define_variables_sensorID(
              data,
              input_sensor = input$map_sensor,
              input_port   = input$map_port
            )
          } else {
            data <- define_variables_sensorID(
              data,
              input_sensor = input$map_sensor
            )
          }
        } else if (nzchar(trimws(input$manual_sensor_id))) {
          # No sensor column — assign a constant label to all rows
          data <- dplyr::mutate(data, gen_sensorID = trimws(input$manual_sensor_id))
        }

        # ── 3. Measurement columns ──────────────────────────────────
        if (nzchar(input$map_temp))
          data <- define_variables_temp(data, input_temp = input$map_temp)
        if (nzchar(input$map_rhum))
          data <- define_variables_rhum(data, input_rhum = input$map_rhum)
        if (nzchar(input$map_wood))
          data <- define_variables_wood(data, input_wood = input$map_wood)
        if (nzchar(input$map_ohm))
          data <- define_variables_ohm(data, input_ohm  = input$map_ohm)

        # ── 4. Date completion ──────────────────────────────────────
        incProgress(0.35, detail = "Completing date sequence…")

        if (isTRUE(input$do_complete)) {
          tf         <- input$complete_timeframe
          # Use gen_datetime for hourly resolution, gen_date otherwise
          date_col   <- if (tf == "hour") "gen_datetime" else "gen_date"
          site_col   <- if (nzchar(input$map_site_id)) input$map_site_id else NULL
          has_sensor <- "gen_sensorID" %in% names(data)

          if (date_col %in% names(data)) {
            date_spine <- make_complete_date(
              data,
              input_date      = date_col,
              input_site_id   = site_col,
              input_sensor_id = if (has_sensor) "gen_sensorID" else NULL,
              timeframe       = tf
            )

            # make_complete_date returns date spine only; join back full data
            date_spine <- date_spine %>% select(-any_of(c("min_date", "max_date")))
            join_keys  <- intersect(names(date_spine), names(data))
            data       <- left_join(date_spine, data, by = join_keys)
          }
        }

        # ── 5. MYCOindex ────────────────────────────────────────────
        incProgress(0.60, detail = "Computing MYCOindexes…")

        if ("mold" %in% input$do_indexes && "gen_rhum" %in% names(data)) {
          data <- make_mycoindex_mold(
            data, input_mold = gen_rhum,
            mold_low  = input$mold_low,
            mold_mid  = input$mold_mid,
            mold_high = input$mold_high
          )
        }
        if ("temp" %in% input$do_indexes && "gen_temp" %in% names(data)) {
          data <- make_mycoindex_temp(
            data, input_temp = gen_temp,
            temp_low  = input$temp_low,
            temp_mid  = input$temp_mid,
            temp_high = input$temp_high,
            temp_max  = input$temp_max
          )
        }
        if ("wood" %in% input$do_indexes && "gen_wood" %in% names(data)) {
          data <- make_mycoindex_wood(
            data, input_wood = gen_wood,
            wood_low  = input$wood_low,
            wood_mid  = input$wood_mid,
            wood_high = input$wood_high,
            wood_max  = input$wood_max
          )
        }

        # ── 6. Seasons ──────────────────────────────────────────────
        incProgress(0.85, detail = "Adding calendar variables…")

        if (isTRUE(input$do_seasons) && "gen_datetime" %in% names(data)) {
          data <- add_date_seasons(data)
        }

        rv$processed_data <- data
        incProgress(1, detail = "Complete.")

        log_event("info", "process",
                  sprintf("Processing complete — %s rows × %s cols",
                          format(nrow(data), big.mark = ","), ncol(data)))

        showNotification(
          sprintf("Done — %s rows × %s columns",
                  format(nrow(data), big.mark = ","), ncol(data)),
          type = "message"
        )

        # Auto-navigate to Dashboard (design spec)
        nav_select("main_nav", "tab_dashboard")
      })
    })
  })

  output$process_status_ui <- renderUI({
    req(rv$processed_data)
    d <- rv$processed_data
    div(class = "alert alert-success py-2",
        icon("circle-check", class = "me-1"),
        sprintf("Processed: %s rows × %s columns",
                format(nrow(d), big.mark = ","), ncol(d)))
  })

  output$tbl_processed <- renderDT({
    req(rv$processed_data)
    datatable(head(rv$processed_data, 200),
              options  = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
              rownames = FALSE,
              class    = "table-sm")
  })


  # ════════════════════════════════════════════════════════════════
  # DASHBOARD — sidebar filters
  # ════════════════════════════════════════════════════════════════

  # Update static variable-selector inputs once processed data exists
  observe({
    req(rv$processed_data)
    d <- rv$processed_data

    prim_avail <- MEASURE_COLS[MEASURE_COLS %in% names(d)]
    if (length(prim_avail)) {
      sel <- if ("gen_rhum" %in% prim_avail) "gen_rhum" else prim_avail[[1]]
      updateSelectInput(session, "dash_primary_var", choices = prim_avail, selected = sel)
    }

    mix_avail <- MIX_COLS[MIX_COLS %in% names(d)]
    if (length(mix_avail)) {
      updateCheckboxGroupInput(session, "dash_mix_overlay",
                               choices = mix_avail, selected = character(0))
      updateSelectInput(session, "heatmap_index",
                        choices = mix_avail, selected = mix_avail[[1]])
    }
  })

  # Dynamic sensor checkboxes (rendered when data is available)
  output$dash_sensor_ui <- renderUI({
    req(rv$processed_data)
    d <- rv$processed_data
    if (!"gen_sensorID" %in% names(d)) return(NULL)

    sensors <- sort(unique(as.character(d$gen_sensorID[!is.na(d$gen_sensorID)])))
    if (!length(sensors)) return(NULL)

    tagList(
      checkboxGroupInput("dash_sensors", "Sensors",
                         choices  = sensors,
                         selected = sensors,
                         width    = "100%"),
      actionLink("dash_select_all",   "Select all",   class = "small me-2"),
      actionLink("dash_deselect_all", "Deselect all", class = "small")
    )
  })

  output$sensor_rename_ui <- renderUI({
    req(rv$processed_data)
    d <- rv$processed_data
    if (!"gen_sensorID" %in% names(d)) return(NULL)

    sensors <- sort(unique(as.character(d$gen_sensorID[!is.na(d$gen_sensorID)])))
    if (!length(sensors)) return(NULL)

    tags$details(
      tags$summary(
        class = "small text-muted mt-1 mb-1",
        style = "cursor:pointer; user-select:none",
        icon("tag", class = "me-1"), "Rename sensors"
      ),
      lapply(sensors, function(s) {
        input_id <- paste0("slbl_", gsub("[^a-zA-Z0-9]", "_", s))
        div(class = "mb-1",
            textInput(input_id, label = NULL, value = s,
                      placeholder = s, width = "100%"))
      })
    )
  })

  sensor_labels <- reactive({
    req(rv$processed_data)
    d <- rv$processed_data
    if (!"gen_sensorID" %in% names(d)) return(NULL)

    sensors <- sort(unique(as.character(d$gen_sensorID[!is.na(d$gen_sensorID)])))
    labels <- vapply(sensors, function(s) {
      val <- input[[paste0("slbl_", gsub("[^a-zA-Z0-9]", "_", s))]]
      if (is.null(val) || !nzchar(trimws(val))) s else trimws(val)
    }, character(1))
    setNames(labels, sensors)
  })

  observeEvent(input$dash_select_all, {
    req(rv$processed_data)
    if ("gen_sensorID" %in% names(rv$processed_data)) {
      sensors <- sort(unique(as.character(rv$processed_data$gen_sensorID)))
      updateCheckboxGroupInput(session, "dash_sensors", selected = sensors)
    }
  })
  observeEvent(input$dash_deselect_all, {
    updateCheckboxGroupInput(session, "dash_sensors", selected = character(0))
  })

  # Dynamic date range
  output$dash_daterange_ui <- renderUI({
    req(rv$processed_data)
    d <- rv$processed_data
    if (!"gen_date" %in% names(d)) return(NULL)

    rng <- range(d$gen_date, na.rm = TRUE)
    tagList(
      dateRangeInput("dash_dates", "Date range",
                     start = rng[1], end = rng[2],
                     min = rng[1], max = rng[2], width = "100%"),
      actionLink("dash_reset_dates", "Reset to full range",
                 class = "small text-muted")
    )
  })

  observeEvent(input$dash_reset_dates, {
    req(rv$processed_data)
    if ("gen_date" %in% names(rv$processed_data)) {
      rng <- range(rv$processed_data$gen_date, na.rm = TRUE)
      updateDateRangeInput(session, "dash_dates", start = rng[1], end = rng[2])
    }
  })

  # ── Filtered data ──
  dash_data <- reactive({
    req(rv$processed_data)
    d <- rv$processed_data

    if ("gen_date" %in% names(d) && !is.null(input$dash_dates)) {
      d <- filter(d, !is.na(gen_date),
                  gen_date >= input$dash_dates[1],
                  gen_date <= input$dash_dates[2])
    }
    if ("gen_sensorID" %in% names(d) && length(input$dash_sensors) > 0) {
      d <- filter(d, gen_sensorID %in% input$dash_sensors)
    }

    # Apply custom sensor labels
    lbl <- sensor_labels()
    if (!is.null(lbl) && "gen_sensorID" %in% names(d)) {
      d$gen_sensorID <- lbl[as.character(d$gen_sensorID)]
    }

    d
  })

  # ── Stat tiles ─────────────────────────────────────────────────────
  output$dash_stat_tiles <- renderUI({
    req(rv$processed_data)
    d <- dash_data()

    fmt_num <- function(x, digits = 1) {
      if (!length(x) || all(is.na(x))) return("—")
      formatC(round(x, digits), format = "f", digits = digits, big.mark = ",")
    }

    n_obs <- nrow(d)
    mean_rh <- if ("gen_rhum" %in% names(d)) mean(d$gen_rhum, na.rm = TRUE) else NA_real_
    peak_rh <- if ("gen_rhum" %in% names(d)) max(d$gen_rhum,  na.rm = TRUE) else NA_real_
    peak_alert <- isTRUE(is.finite(peak_rh) && peak_rh >= 85)

    n_highrisk <- if ("MIx_mold" %in% names(d)) {
      sum(d$MIx_mold >= 0.5, na.rm = TRUE)
    } else NA_integer_
    risk_alert <- isTRUE(!is.na(n_highrisk) && n_highrisk > 0)

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      stat_tile("Observations",
                format(n_obs, big.mark = ","),
                sub = "rows in filter"),
      stat_tile("Mean RH",
                if (is.finite(mean_rh)) fmt_num(mean_rh, 1) else "—",
                unit = "%",
                sub = "relative humidity"),
      stat_tile("Peak RH",
                if (is.finite(peak_rh)) fmt_num(peak_rh, 1) else "—",
                unit = "%",
                sub = if (peak_alert) "≥ 85% threshold" else "within range",
                alert = peak_alert),
      stat_tile("High-risk periods",
                if (!is.na(n_highrisk)) format(n_highrisk, big.mark = ",") else "—",
                sub = "MIx_mold ≥ 0.5",
                alert = risk_alert)
    )
  })

  # ── Dashboard placeholder / tabs ──
  output$dashboard_main_ui <- renderUI({
    if (is.null(rv$processed_data)) {
      div(class = "alert alert-info m-3",
          icon("circle-info", class = "me-1"),
          "Process your data first (Configure tab) to enable the dashboard.")
    } else {
      tagList(
        div(style = "margin-bottom: 14px;", uiOutput("dash_stat_tiles")),
        navset_card_tab(
          nav_panel(
            title = tagList(icon("chart-line"), " Time Series"),
            plotlyOutput("plot_timeseries", height = "460px")
          ),
          nav_panel(
            title = tagList(icon("table"), " Summary"),
            DTOutput("tbl_summary")
          ),
          nav_panel(
            title = tagList(icon("chart-bar"), " Distributions"),
            layout_columns(
              col_widths = c(6, 6),
              plotlyOutput("plot_hist_primary", height = "380px"),
              plotlyOutput("plot_hist_mix",     height = "380px")
            )
          ),
          nav_panel(
            title = tagList(icon("border-all"), " Heatmap"),
            plotlyOutput("plot_heatmap", height = "520px")
          )
        )
      )
    }
  })


  # ════════════════════════════════════════════════════════════════
  # DASHBOARD — plots
  # ════════════════════════════════════════════════════════════════

  # ── Time series ──────────────────────────────────────────────────
  output$plot_timeseries <- renderPlotly({
    req(dash_data())
    d <- dash_data()

    x_col   <- if ("gen_datetime" %in% names(d)) "gen_datetime" else "gen_date"
    y_var   <- input$dash_primary_var
    overlay <- input$dash_mix_overlay
    vars    <- unique(c(y_var, overlay))
    vars    <- vars[vars %in% names(d)]

    if (!length(vars) || !x_col %in% names(d))
      return(empty_plotly("Select a variable in the sidebar"))

    has_sensor <- "gen_sensorID" %in% names(d)
    sensors    <- if (has_sensor) sort(unique(as.character(d$gen_sensorID))) else NULL
    p <- plot_ly()

    for (var in vars) {
      lbl <- col_label(var)

      if (has_sensor) {
        for (i in seq_along(sensors)) {
          snsr <- sensors[i]
          dd <- filter(d, gen_sensorID == snsr) %>% arrange(.data[[x_col]])
          p  <- add_trace(p,
            x    = dd[[x_col]], y = dd[[var]],
            type = "scatter", mode = "lines",
            name = paste0(snsr, " — ", lbl),
            line = list(color = nycta_color(i), width = 1.75),
            hovertemplate = paste0(
              "%{x}<br>", lbl, ": %{y:.3f}<extra>", snsr, "</extra>")
          )
        }
      } else {
        dd <- arrange(d, .data[[x_col]])
        p  <- add_trace(p,
          x    = dd[[x_col]], y = dd[[var]],
          type = "scatter", mode = "lines", name = lbl,
          line = list(color = nycta_color(1), width = 1.75)
        )
      }
    }

    apply_plotly_theme(p, xaxis_title = "", yaxis_title = col_label(y_var))
  })

  # ── Summary table ─────────────────────────────────────────────────
  output$tbl_summary <- renderDT({
    req(dash_data())
    d <- dash_data()

    # Measurement columns only (drop MIx_* indices)
    num_cols <- intersect(
      c("gen_temp", "gen_rhum", "gen_wood", "gen_ohm"),
      names(d)
    )

    if (!length(num_cols))
      return(datatable(data.frame(Note = "No processed numeric columns.")))

    fmt_num <- function(x, digits = 2) {
      if (!is.finite(x)) return("—")
      formatC(x, format = "f", digits = digits, big.mark = ",")
    }

    # Build one-column-per-sensor summaries
    has_sensor <- "gen_sensorID" %in% names(d) &&
      any(!is.na(d$gen_sensorID))

    if (has_sensor) {
      d$.grp <- as.character(d$gen_sensorID)
      d$.grp[is.na(d$.grp)] <- "(unlabeled)"
      groups <- sort(unique(d$.grp))
    } else {
      d$.grp <- "All data"
      groups <- "All data"
    }

    # Pick a representative date column for the date-range row
    date_col <- if ("gen_datetime" %in% names(d)) "gen_datetime"
                else if ("gen_date" %in% names(d)) "gen_date"
                else NULL

    # Compute per-sensor stats as a named list of character vectors
    # (one entry per group; each entry is a vector of cell values aligned to rows)
    per_group <- lapply(groups, function(g) {
      dd <- d[d$.grp == g, , drop = FALSE]
      n_obs <- nrow(dd)

      # Date range
      dr <- if (!is.null(date_col)) {
        dv <- dd[[date_col]]
        dv <- dv[!is.na(dv)]
        if (length(dv)) {
          sprintf("%s – %s",
                  format(min(dv), "%Y-%m-%d"),
                  format(max(dv), "%Y-%m-%d"))
        } else "—"
      } else "—"

      cells <- c(dr, format(n_obs, big.mark = ","))

      for (col in num_cols) {
        x <- dd[[col]]
        x <- x[is.finite(x)]
        if (!length(x)) {
          cells <- c(cells, "—", "—", format(0L, big.mark = ","))
        } else {
          m  <- mean(x); sdv <- stats::sd(x)
          lo <- min(x);  hi  <- max(x)
          cells <- c(cells,
                     sprintf("%s (%s)", fmt_num(m, 2),
                             if (is.finite(sdv)) fmt_num(sdv, 2) else "—"),
                     sprintf("%s – %s", fmt_num(lo, 2), fmt_num(hi, 2)),
                     format(sum(!is.na(dd[[col]])), big.mark = ","))
        }
      }
      cells
    })

    # Build the Variable column (row labels)
    var_rows <- c("Date range", "N observations")
    for (col in num_cols) {
      lbl <- col_label(col)
      var_rows <- c(var_rows,
                    sprintf("%s — Mean (SD)", lbl),
                    sprintf("%s — Range",     lbl),
                    sprintf("%s — N",         lbl))
    }

    smry <- data.frame(Variable = var_rows, stringsAsFactors = FALSE)
    for (i in seq_along(groups)) {
      smry[[groups[i]]] <- per_group[[i]]
    }

    datatable(
      smry,
      options = list(
        scrollX    = TRUE,
        paging     = FALSE,
        searching  = FALSE,
        info       = FALSE,
        ordering   = FALSE,
        dom        = "t"
      ),
      rownames = FALSE,
      class    = "table-sm table-striped"
    )
  })

  # ── Histogram — primary variable ──────────────────────────────────
  output$plot_hist_primary <- renderPlotly({
    req(dash_data())
    d   <- dash_data()
    var <- input$dash_primary_var

    if (!var %in% names(d)) return(empty_plotly())

    lbl <- col_label(var)

    p <- if ("gen_sensorID" %in% names(d) &&
             length(unique(na.omit(d$gen_sensorID))) > 1) {

      sensors <- sort(unique(as.character(na.omit(d$gen_sensorID))))
      pal     <- setNames(vapply(seq_along(sensors), nycta_color, character(1)),
                          sensors)

      plot_ly(d, x = ~.data[[var]],
              color  = ~as.character(gen_sensorID),
              colors = pal,
              type = "histogram", nbinsx = 40, opacity = 0.85,
              marker = list(line = list(color = "#FFFFFF", width = 0.5))) %>%
        layout(barmode = "overlay")

    } else {
      plot_ly(d, x = ~.data[[var]], type = "histogram", nbinsx = 40,
              marker = list(color = nycta_color(1),
                            line  = list(color = "#FFFFFF", width = 0.5)),
              opacity = 0.85)
    }

    apply_plotly_theme(p,
      xaxis_title = lbl, yaxis_title = "Count",
      hovermode = "closest"
    )
  })

  # ── Histogram — MYCOindex ──────────────────────────────────────────
  output$plot_hist_mix <- renderPlotly({
    req(dash_data())
    d     <- dash_data()
    avail <- intersect(unname(MIX_COLS), names(d))

    if (!length(avail)) return(empty_plotly("No MYCOindex columns available"))

    p <- plot_ly()

    for (key in avail) {
      p <- add_trace(p,
        x       = d[[key]],
        type    = "histogram",
        name    = col_label(key),
        opacity = 0.85,
        nbinsx  = 10,
        marker  = list(
          color = MIX_HIST_COLORS[[key]] %||% nycta_color(1),
          line  = list(color = "#FFFFFF", width = 0.5)
        )
      )
    }

    apply_plotly_theme(p,
      xaxis_title = "MYCOindex (0–1)",
      yaxis_title = "Count",
      hovermode   = "closest"
    ) %>% layout(
      barmode = "overlay",
      xaxis = list(
        title    = "MYCOindex (0–1)", range = c(-0.05, 1.1),
        showgrid = TRUE, gridcolor = "#D9D9D9", gridwidth = 0.5,
        zeroline = FALSE, linecolor = "#333333", linewidth = 1,
        tickfont = list(family = "JetBrains Mono", size = 9)
      )
    )
  })

  # ── Heatmap ────────────────────────────────────────────────────────
  output$plot_heatmap <- renderPlotly({
    req(dash_data())
    d   <- dash_data()
    idx <- input$heatmap_index

    if (is.null(idx) || !idx %in% names(d))
      return(empty_plotly("Select a MYCOindex that has been computed"))

    lbl      <- col_label(idx)
    has_date <- "gen_date"     %in% names(d)
    has_snsr <- "gen_sensorID" %in% names(d)

    if (!has_date) return(empty_plotly("Date column not available"))

    risk_note <- list(
      text      = "0 = No risk · 0.25 = Low · 0.5 = Moderate · 1.0 = High/Critical",
      showarrow = FALSE,
      xref      = "paper", yref = "paper",
      x = 0, y = -0.22, xanchor = "left",
      font = list(family = "Inter", size = 9, color = "#767676")
    )

    if (has_snsr) {
      heat_df <- d %>%
        filter(!is.na(gen_date)) %>%
        mutate(sensor = as.character(gen_sensorID)) %>%
        group_by(date = gen_date, sensor) %>%
        summarise(value = mean(.data[[idx]], na.rm = TRUE), .groups = "drop")

      wide   <- pivot_wider(heat_df, names_from = sensor, values_from = value)
      x_vals <- wide$date
      y_vals <- setdiff(names(wide), "date")
      z_mat  <- as.matrix(wide[, y_vals, drop = FALSE])

      plot_ly(
        x = x_vals, y = y_vals, z = t(z_mat),
        type       = "heatmap",
        colorscale = MIX_COLORSCALE,
        zmin = 0, zmax = 1,
        xgap = 0.5, ygap = 0.5,
        colorbar   = list(
          title     = list(text = lbl, font = list(size = 10)),
          tickvals  = c(0, 0.25, 0.5, 1),
          tickfont  = list(family = "JetBrains Mono", size = 9)
        ),
        hovertemplate = paste0(
          "<b>%{x|%Y-%m-%d}</b><br>Sensor: %{y}<br>",
          lbl, ": %{z:.2f}<extra></extra>")
      ) %>% layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font  = list(family = "Inter", size = 10, color = "#555555"),
        xaxis = list(title = "",
                     tickfont = list(family = "Inter", size = 9)),
        yaxis = list(title = "", autorange = "reversed",
                     tickfont = list(family = "Inter", size = 10)),
        annotations = list(risk_note),
        margin = list(t = 20, r = 20, b = 80, l = 80)
      )

    } else {
      # No sensor column: area chart of daily mean
      trend <- d %>%
        filter(!is.na(gen_date)) %>%
        group_by(date = gen_date) %>%
        summarise(value = mean(.data[[idx]], na.rm = TRUE), .groups = "drop") %>%
        arrange(date)

      p <- plot_ly(trend, x = ~date, y = ~value,
              type = "scatter", mode = "lines",
              fill = "tozeroy",
              fillcolor = "rgba(11,61,145,0.12)",
              line = list(color = nycta_color(1), width = 1.75),
              hovertemplate = "<b>%{x|%Y-%m-%d}</b><br>Value: %{y:.2f}<extra></extra>")

      apply_plotly_theme(p, xaxis_title = "", yaxis_title = lbl,
                         hovermode = "x unified", show_legend = FALSE) %>%
        layout(
          yaxis = list(
            title    = lbl, range = c(0, 1.05),
            showgrid = TRUE, gridcolor = "#D9D9D9", gridwidth = 0.5,
            zeroline = FALSE, linecolor = "#333333", linewidth = 1,
            tickfont = list(family = "JetBrains Mono", size = 9)
          ),
          annotations = list(risk_note)
        )
    }
  })


  # ════════════════════════════════════════════════════════════════
  # DOWNLOAD
  # ════════════════════════════════════════════════════════════════

  output$dl_prereq_ui <- renderUI({
    if (is.null(rv$processed_data)) {
      div(class = "alert alert-warning mb-2",
          icon("triangle-exclamation", class = "me-1"),
          "Process data first (Configure tab).")
    }
  })

  output$btn_download <- downloadHandler(
    filename = function() {
      ext  <- switch(input$dl_format, "xlsx" = ".xlsx", ".csv")
      base <- trimws(input$dl_filename)
      if (!nzchar(base)) base <- paste0("mycotools_", format(Sys.time(), "%Y-%m-%d-%H-%M"))
      paste0(base, ext)
    },
    content = function(file) {
      req(rv$processed_data)
      d <- rv$processed_data
      switch(input$dl_format,
        "xlsx" = writexl::write_xlsx(d, file),
        "csv2" = readr::write_csv2(d, file),
                 readr::write_csv(d, file)
      )
    }
  )


  # ════════════════════════════════════════════════════════════════
  # LOG tab — view, badge, copy, download, clear
  # ════════════════════════════════════════════════════════════════

  log_report_text <- function(filter = "all") {
    paste(
      log_system_info(),
      "",
      format_log_entries(rv$log_entries, filter),
      sep = "\n"
    )
  }

  output$log_view <- renderText({
    log_report_text(input$log_filter %||% "all")
  })

  output$log_badge_ui <- renderUI({
    entries <- rv$log_entries
    n_err <- sum(vapply(entries, function(e) e$level == "error", logical(1)))
    n_wrn <- sum(vapply(entries, function(e) e$level == "warn",  logical(1)))
    tagList(
      if (n_err > 0)
        tags$span(class = "log-badge-error",
                  sprintf("%s error%s", n_err, if (n_err == 1) "" else "s")),
      if (n_wrn > 0)
        tags$span(class = "badge-outline",
                  sprintf("%s warning%s", n_wrn, if (n_wrn == 1) "" else "s")),
      tags$span(class = "badge-outline",
                sprintf("%s events", length(entries)))
    )
  })

  observeEvent(input$log_copy, {
    session$sendCustomMessage("copy_to_clipboard", log_report_text("all"))
  })

  observeEvent(input$log_copy_ok, {
    showNotification("Log copied to clipboard", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  observeEvent(input$log_copy_fail, {
    showNotification("Clipboard copy failed — use Download instead",
                     type = "error", duration = 6)
  }, ignoreInit = TRUE)

  output$log_download <- downloadHandler(
    filename = function() {
      paste0("mycotools_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content  = function(file) {
      writeLines(log_report_text("all"), file, useBytes = TRUE)
    }
  )

  observeEvent(input$log_clear, {
    rv$log_entries <- list()
    log_event("info", "session", "Log cleared by user")
  })
}

shinyApp(ui = ui, server = server)
