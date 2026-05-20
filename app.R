# MycoTools Shiny — bslib-themed app.R (package-first, cleaned)
# -----------------------------------------------------------------------------
# - Uses {bslib} Bootstrap 5 + Google Inter (self-hosted) for a clean theme.
# - Removes shinyvalidate; uses simple req() validation.
# - Imports all data-processing from your MycoTools package.
# - Adds a header with LEFT customer logo and RIGHT company logo.
# - **Logos**: place files in ./www/mycoteam_logo.png and ./www/lang_logo.png
# - **Placeholders**: change logo sizes and header font size via CSS variables
#   at the top of the CSS block (see `:root { --myc-... }`).
# - Works both with Run App and shinyApp(ui, server) thanks to addResourcePath().
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(rlang)
  library(ggplot2)
  library(MycoTools)  # your package
})

# ---- Global options ----------------------------------------------------------
# Increase max upload size (defaults to 500 MB; override via env var)
options(shiny.maxRequestSize = as.numeric(Sys.getenv("MYCOTOOLS_MAX_UPLOAD_MB", "500")) * 1024^2)

# Serve ./www as /assets even when launching with shinyApp(ui, server)
app_www <- normalizePath("www", mustWork = FALSE)
if (dir.exists(app_www)) {
  shiny::addResourcePath("assets", app_www)
} else {
  warning("www directory not found at: ", getwd(), "/www")
}

# Safe null-coalescing helper (works even if rlang isn't attached)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- Theme (palette approximates customer site) ------------------------------
# Primary teal-green, lime accent; warm paper background set via CSS below
theme <- bs_theme(
  version   = 5,
  primary   = "#37A29C",  # teal-green
  secondary = "#C7D65A",  # lime accent #00688B
  info      = "#5DC2BB",
  success   = "#39B27F",
  warning   = "#F2C94C",
  danger    = "#E15759",
  # Self-host Inter so no external calls to Google Fonts
  base_font    = font_google("Inter", local = TRUE),
  heading_font = font_google("Inter", local = TRUE)
)

# ---- CSS (edit the variables below to adjust sizes quickly) ------------------
custom_css <- HTML('
  /* === Editable variables (placeholders you can tweak) =================== */
  :root {
    --myc-logo-left-h:  36px;   /* <- LEFT logo height */
    --myc-logo-right-h: 36px;   /* <- RIGHT logo height */
    --myc-header-font-size: 1.8rem; /* <- Header title font size */
  }

  /* === Base look & typography ============================================ */
  body { background-color: #F7F5EF; font-family: "Inter", var(--bs-font-sans-serif) !important; }
  .btn, .navbar, .nav, .nav-link, .form-control, .myc-header {
    font-family: "Inter", var(--bs-font-sans-serif) !important;
  }

  /* === Header bar ======================================================== */
  .myc-header { background-color: #FFFFFF; border-bottom: 1px solid #e8e6e2; }
  .myc-header .app-title { font-weight: 600; font-size: var(--myc-header-font-size); color: #2B2A29; }
  .myc-header .logo-left  { height: var(--myc-logo-left-h); }
  .myc-header .logo-right { height: var(--myc-logo-right-h); }

  /* === Minor polish ====================================================== */
  .btn-primary { box-shadow: 0 1px 1px rgba(0,0,0,0.05); font-weight: 600; }
  .card, .well, .tabbable { border-radius: .75rem; }
  .sidebarPanel { background: transparent; }
')

# ---- UI ----------------------------------------------------------------------
ui <- page_fluid(
  theme = theme,
  tags$head(tags$style(custom_css)),

  # Header: left customer logo + title, right company logo --------------------
  tags$div(
    class = "myc-header container-fluid py-2",
    tags$div(
      class = "d-flex align-items-center justify-content-between",
      # LEFT: Customer logo + title (PUT FILE IN ./www/mycoteam_logo.png)
      tags$div(
        class = "d-flex align-items-center gap-2",
        tags$img(src = "assets/mycoteam_logo.png", class = "logo-left", alt = "Customer logo"),
        tags$span(class = "app-title", "MycoTools — Logger Data Normalization & Metrics")
      ),
      # RIGHT: Your company logo (PUT FILE IN ./www/lang_logo.png)
      tags$img(src = "assets/lang_logo.png", class = "logo-right", alt = "Company logo")
    )
  ),

  # Main layout ---------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      h4("1) Upload"),
      fileInput("file", "Upload CSV or Excel", accept = c(".csv", ".txt", ".xls", ".xlsx")),
      uiOutput("excel_sheet_ui"),
      tags$hr(),

      h4("Read options"),
      fluidRow(
        column(6, selectInput(
          "delim", "Delimiter",
          choices = c("Auto" = "auto", "Comma" = ",", "Semicolon" = ";", "Tab" = "	"),
          selected = "auto"
        )),
        column(6, selectInput(
          "decimal", "Decimal mark",
          choices = c("Auto" = "auto", "Dot (.)" = ".", "Comma (,)" = ","),
          selected = "auto"
        ))
      ),
      numericInput("skip", "Skip first lines", value = 0, min = 0, step = 1),
      textInput("comment", "Comment prefix (single char; blank = none)", value = "#"),
      checkboxInput("has_header", "First non-skipped line has column names", TRUE),

      tags$hr(),

      h4("2) Map datetime"),
      radioButtons("dt_mode", "Datetime mode", choices = c("Unified" = "unified", "Split date + time" = "split"), inline = TRUE),
      selectInput("col_datetime", "Unified datetime column", choices = NULL),
      selectInput("col_date", "Date column", choices = NULL),
      selectInput("col_time", "Time column", choices = NULL),
      selectInput("tz", "Target time zone", choices = OlsonNames(), selected = "Europe/Oslo"),
      tags$hr(),

      h4("3) Map measurements"),
      selectInput("col_temp", "Temperature column", choices = NULL),
      selectInput("col_rhum", "Relative humidity column", choices = NULL),
      selectInput("col_wood", "Wood moisture column", choices = NULL),
      selectInput("id_col", "Grouping ID (optional)", choices = NULL),
      tags$hr(),

      h4("4) Rolling options (row-based in your package)"),
      numericInput("roll_k", "Rolling window (rows)", value = 24, min = 2, step = 1),
      checkboxGroupInput("roll_do", "Compute rolling means for:",
                         choices = c("MIx_mold", "MIx_temp", "MIx_wood"),
                         selected = c()),
      tags$hr(),

      actionButton("process", "Process data", class = "btn btn-primary"),
      width = 4
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Raw preview", DTOutput("raw_tbl")),
        tabPanel("Processed preview", DTOutput("proc_tbl")),
        tabPanel("Explore",
                 fluidRow(
                   column(4, selectInput("y_var", "Y variable", choices = NULL)),
                   column(4, dateRangeInput("date_rng", "Filter by date (gen_date)")),
                   column(4, checkboxInput("prefer_ra", "Prefer *_roll_* if available", value = TRUE))
                 ),
                 plotOutput("ts_plot", height = "380px")
        ),
        tabPanel("Download",
                 downloadButton("download_csv", "Download processed CSV"),
                 br(), br(),
                 verbatimTextOutput("summary_out")
        )
      )
    )
  )
)

# ---- SERVER ------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive storage for raw data ---------------------------------------------
  raw_data <- reactiveVal(NULL)

  # Show sheet selector when an Excel file is uploaded ------------------------
  output$excel_sheet_ui <- renderUI({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (tolower(ext) %in% c("xls", "xlsx")) {
      pth <- input$file$datapath
      sheets <- tryCatch(readxl::excel_sheets(pth), error = function(e) NULL)
      selectInput("excel_sheet", "Excel sheet", choices = sheets %||% 1, selected = 1)
    } else NULL
  })

  # Read file using MycoTools::import_data ------------------------------------
  observeEvent(list(input$file, input$excel_sheet), {
    req(input$file)
    pth <- input$file$datapath
    ext <- tools::file_ext(input$file$name)
    df <- tryCatch({
      if (tolower(ext) %in% c("xls","xlsx")) {
        MycoTools::import_data(
          path  = pth,
          sheet = if (!is.null(input$excel_sheet)) input$excel_sheet else 1
        )
      } else {
        MycoTools::import_data(
          path      = pth,
          delim     = input$delim   %||% "auto",
          decimal   = input$decimal %||% "auto",
          skip      = input$skip    %||% 0,
          comment   = if (!is.null(input$comment) && nzchar(input$comment)) substr(input$comment, 1, 1) else NULL,
          col_names = isTRUE(input$has_header),
          guess_max = 10000
        )
      }
    }, error = function(e) {
      showNotification(paste("Failed to read file:", e$message), type = "error"); NULL
    })
    raw_data(df)
  }, ignoreInit = FALSE)

  # Update mapping dropdowns after data load ----------------------------------
  observeEvent(raw_data(), {
    df <- raw_data(); req(df)
    nms <- names(df)
    updateSelectInput(session, "col_datetime", choices = c("", nms))
    updateSelectInput(session, "col_date", choices = c("", nms))
    updateSelectInput(session, "col_time", choices = c("", nms))
    updateSelectInput(session, "col_temp", choices = c("", nms))
    updateSelectInput(session, "col_rhum", choices = c("", nms))
    updateSelectInput(session, "col_wood", choices = c("", nms))
    updateSelectInput(session, "id_col", choices = c("", nms))

    num_candidates <- nms[vapply(df, is.numeric, logical(1))]
    updateSelectInput(session, "y_var", choices = c("", num_candidates))
  })

  # Main processing pipeline ---------------------------------------------------
  processed_data <- eventReactive(input$process, {
    req(raw_data())

    # Inline validation (no shinyvalidate)
    if (identical(input$dt_mode, "unified")) {
      req(!is.null(input$col_datetime), nzchar(input$col_datetime))
    } else {
      req(!is.null(input$col_date), nzchar(input$col_date))
      # time is optional
    }

    df <- raw_data()

    # 1) Normalize datetime via MycoTools
    tz_val <- if (is.null(input$tz) || input$tz == "") "Europe/Oslo" else input$tz
    if (identical(input$dt_mode, "unified")) {
      col_dt <- if (!is.null(input$col_datetime) && nzchar(input$col_datetime)) input$col_datetime else NULL
      df <- MycoTools::define_variables_datetime(
        data = df, input_datetime = col_dt, tz = tz_val, quiet = TRUE
      )
    } else {
      col_d <- if (!is.null(input$col_date) && nzchar(input$col_date)) input$col_date else NULL
      col_t <- if (!is.null(input$col_time) && nzchar(input$col_time)) input$col_time else NULL
      df <- MycoTools::define_variables_datetime(
        data = df, input_date = col_d, input_time = col_t, tz = tz_val, quiet = TRUE
      )
    }

    # 2) Map measurements to canonical columns expected by MycoIndex funcs
    if (!is.null(input$col_temp) && nzchar(input$col_temp))  df$gen_temp <- df[[input$col_temp]]
    if (!is.null(input$col_rhum) && nzchar(input$col_rhum))  df$gen_rhum <- df[[input$col_rhum]]
    if (!is.null(input$col_wood) && nzchar(input$col_wood))  df$gen_wood <- df[[input$col_wood]]
    if (!("gen_temp" %in% names(df))) df$gen_temp <- NA_real_
    if (!("gen_rhum" %in% names(df))) df$gen_rhum <- NA_real_
    if (!("gen_wood" %in% names(df))) df$gen_wood <- NA_real_

    # 3) Optional date-derived features
    if ("add_date_seasons" %in% getNamespaceExports("MycoTools")) {
      df <- MycoTools::add_date_seasons(df, input_date = gen_datetime)
    }

    # 4) MycoIndex components
    if ("make_mycoindex_mold" %in% getNamespaceExports("MycoTools")) df <- MycoTools::make_mycoindex_mold(df)
    if ("make_mycoindex_temp" %in% getNamespaceExports("MycoTools")) df <- MycoTools::make_mycoindex_temp(df)
    if ("make_mycoindex_wood" %in% getNamespaceExports("MycoTools")) df <- MycoTools::make_mycoindex_wood(df)

    # 5) Optional rolling means (row-based wrappers)
    k <- if (is.null(input$roll_k)) 24 else as.integer(input$roll_k)
    if (length(input$roll_do)) {
      if ("MIx_mold" %in% names(df) && "MIx_mold" %in% input$roll_do &&
          "make_rolling_mix_mold" %in% getNamespaceExports("MycoTools")) {
        out_sym <- rlang::sym(paste0("MIx_mold_roll_", k, "rows"))
        df <- MycoTools::make_rolling_mix_mold(df, input = MIx_mold, output_name = out_sym, roll_interval = k)
      }
      if ("MIx_temp" %in% names(df) && "MIx_temp" %in% input$roll_do &&
          "make_rolling_mix_temp" %in% getNamespaceExports("MycoTools")) {
        out_sym <- rlang::sym(paste0("MIx_temp_roll_", k, "rows"))
        df <- MycoTools::make_rolling_mix_temp(df, input = MIx_temp, output_name = out_sym, roll_interval = k)
      }
      if ("MIx_wood" %in% names(df) && "MIx_wood" %in% input$roll_do &&
          "make_rolling_mix_wood" %in% getNamespaceExports("MycoTools")) {
        out_sym <- rlang::sym(paste0("MIx_wood_roll_", k, "rows"))
        df <- MycoTools::make_rolling_mix_wood(df, input = MIx_wood, output_name = out_sym, roll_interval = k)
      }
    }

    # 6) Order columns for readability
    canon_first <- c("gen_datetime", "gen_date", "gen_time", "gen_temp", "gen_rhum", "gen_wood",
                     "MIx_mold", "MIx_temp", "MIx_wood")
    rest <- setdiff(names(df), canon_first)
    df[, c(intersect(canon_first, names(df)), rest)]
  })

  # Tables ---------------------------------------------------------------------
  output$raw_tbl  <- renderDT({ req(raw_data());       datatable(raw_data(),      options = list(scrollX = TRUE, pageLength = 10)) })
  output$proc_tbl <- renderDT({ req(processed_data()); datatable(processed_data(), options = list(scrollX = TRUE, pageLength = 10)) })

  # Explorer plot --------------------------------------------------------------
  output$ts_plot <- renderPlot({
    req(processed_data(), input$y_var)
    d <- processed_data(); req("gen_datetime" %in% names(d))

    # Optional date filter
    if (!is.null(input$date_rng) && all(!is.na(input$date_rng))) {
      d <- d %>% filter(gen_date >= input$date_rng[1], gen_date <= input$date_rng[2])
    }

    y <- input$y_var
    if (isTRUE(input$prefer_ra)) {
      cand <- grep(paste0("^", y, "_roll_"), names(d), value = TRUE)
      if (length(cand)) y <- cand[1]
    }

    d <- d %>% arrange(gen_datetime)
    ggplot(d, aes(x = gen_datetime, y = .data[[y]])) +
      geom_line() +
      labs(x = "gen_datetime", y = y) +
      theme_minimal(base_size = 12)
  })

  # Download -------------------------------------------------------------------
  output$download_csv <- downloadHandler(
    filename = function() paste0("mycotools_processed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) readr::write_csv(processed_data(), file, na = "")
  )

  # Summary --------------------------------------------------------------------
  output$summary_out <- renderPrint({
    req(processed_data()); d <- processed_data()
    list(
      n_rows = nrow(d),
      n_cols = ncol(d),
      cols   = names(d),
      range_gen_datetime = if ("gen_datetime" %in% names(d)) range(d$gen_datetime, na.rm = TRUE) else NULL
    )
  })
}

shinyApp(ui, server)


#' # MycoTools Shiny mockup — app.R (package-first)
#' # -----------------------------------------------------------------------------
#' # This version **imports from your MycoTools package** instead of inlining
#' # helpers. Drop this file in a dir and run `shiny::runApp()` with MycoTools
#' # installed and loaded. All processing steps call your package functions.
#' # -----------------------------------------------------------------------------
#'
#' suppressPackageStartupMessages({
#'   library(shiny)
#'   #library(shinyvalidate)
#'   library(DT)
#'   library(readr)
#'   library(readxl)
#'   library(dplyr)
#'   library(tidyr)
#'   library(lubridate)
#'   library(rlang)
#'   library(ggplot2)
#'   library(bslib)
#'   library(MycoTools)  # <— use your package
#' })
#'
#'
#' # ---- Theme & CSS ------------------------------------------------------------
#' # Primary teal-green, lime accent, warm paper background, charcoal text
#' theme <- bs_theme(
#'   version = 5,
#'   primary = "#37A29C", # teal-green
#'   secondary = "#C7D65A", # lime accent
#'   info = "#5DC2BB",
#'   success = "#39B27F",
#'   warning = "#F2C94C",
#'   danger = "#E15759",
#'   # Use system fonts to avoid network dependency; swap to font_google("Inter") if desired
#'   # base_font = font_family(sans_serif()),
#'   # heading_font = font_family(sans_serif())
#'   # base_font = font_google("Inter"),
#'   # heading_font = font_google("Inter")
#'   base_font    = font_google("Inter", local = TRUE),
#'   heading_font = font_google("Inter", local = TRUE)
#' )
#'
#' # --- Custom CSS to echo site feel --------------------------------------------
#' .custom_css <- HTML('
#' body { background-color: #F7F5EF; } /* warm paper */
#' .myc-header { background-color: #FFFFFF; border-bottom: 1px solid #e8e6e2; }
#' .myc-header .app-title { font-weight: 600; font-size: 1.10rem; color: #2B2A29; }
#' .myc-header img { height: 36px; }
#' .btn-primary { box-shadow: 0 1px 1px rgba(0,0,0,0.05); }
#' .card, .well, .tabbable { border-radius: .75rem; }
#' .sidebarPanel { background: transparent; }
#' ')
#'
#' # ---- Small Helpers ----------------------------------------------------------
#'
#' # Increase max upload size (defaults to 500 MB; override via env var)
#' options(shiny.maxRequestSize = as.numeric(Sys.getenv("MYCOTOOLS_MAX_UPLOAD_MB", "500")) * 1024^2)
#'
#' # Safe null-coalescing helper (works even if rlang isn't attached)
#' `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
#'
#' # --- Read helpers for CSV/CSV2/TXT with skip and single-char comments ---
#' best_delim <- function(path, comment = NULL, n_max = 50) {
#'   lines <- try(readLines(path, n = n_max, warn = FALSE), silent = TRUE)
#'   if (inherits(lines, "try-error")) return(",")
#'   lines <- lines[nzchar(lines)]
#'
#'   if (!is.null(comment) && nzchar(comment)) {
#'     # escape comment char for regex
#'     esc <- gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1", comment)
#'     lines <- lines[!grepl(paste0("^\\s*", esc), lines)]
#'   }
#'
#'   cand <- c(",", ";", "\t")
#'   score <- vapply(cand, function(d) {
#'     mean(vapply(strsplit(lines, d, fixed = TRUE), length, integer(1)), na.rm = TRUE)
#'   }, numeric(1))
#'
#'   cand[which.max(score)]
#' }
#'
#' read_any_tabular <- function(path,
#'                              delim = "auto",
#'                              decimal = "auto",
#'                              skip = 0,
#'                              comment = NULL,
#'                              col_names = TRUE,
#'                              guess_max = 10000) {
#'   if (identical(delim, "auto")) delim <- best_delim(path, comment)
#'   if (identical(decimal, "auto")) {
#'     dec <- if (identical(delim, ";")) "," else "."
#'   } else {
#'     dec <- decimal
#'   }
#'   loc <- readr::locale(decimal_mark = dec, encoding = "UTF-8")
#'
#'   readr::read_delim(
#'     file = path,
#'     delim = delim,
#'     skip = skip,
#'     comment = if (!is.null(comment) && nzchar(comment)) comment else NULL,
#'     col_names = col_names,
#'     guess_max = guess_max,
#'     locale = loc,
#'     na = c("", "NA", "NaN")
#'   )
#' }
#'
#'
#' # ---- UI ----------------------------------------------------------------------
#' ui <- page_fluid(#fluidPage(
#'   theme = theme,
#'
#'   # --- Header with left (customer) & right (your) logo ---
#'   tags$head(
#'     tags$style(HTML("
#'       /* Make sure the Inter font is used everywhere important */
#'       body, .btn, .nav, .navbar, .form-control, .tabbable, .myc-header {
#'         font-family: 'Inter', var(--bs-font-sans-serif) !important;
#'       }
#'       .myc-header .app-title { font-weight: 600; }
#'     "
#'     #' "
#'     #'   .myc-header { border-bottom: 1px solid #e5e7eb; }
#'     #'   .myc-header .app-title { font-weight: 600; font-size: 1.15rem; }
#'     #'   .myc-header img { height: 36px; }
#'     #'   @media (max-width: 576px) {
#'     #'     .myc-header .app-title { font-size: 1rem; }
#'     #'     .myc-header img { height: 28px; }
#'     #'   }
#'     #' "
#'     ))
#'   ),
#'   tags$div(
#'     class = "myc-header container-fluid py-2",
#'     tags$div(
#'       class = "d-flex align-items-center justify-content-between",
#'       # LEFT: Customer logo + title
#'       tags$div(
#'         class = "d-flex align-items-center gap-2",
#'         # --- PLACEHOLDER: put your customer's logo file at ./www/customer_logo.png ---
#'         tags$img(src = "mycoteam_logo.png", alt = "Customer logo"),
#'         tags$span(class = "app-title", "MycoTools — Logger Data Normalization & Metrics")
#'       ),
#'       # RIGHT: Your company logo
#'       # --- PLACEHOLDER: put your company logo at ./www/company_logo.png ---
#'       tags$img(src = "lang_logo.png", alt = "Company logo")
#'     )
#'   ),
#'
#'   # --- Your existing layout (unchanged) ---
#'   sidebarLayout(
#'     sidebarPanel(
#'       h4("1) Upload"),
#'       fileInput("file", "Upload CSV or Excel", accept = c(".csv", ".txt", ".xls", ".xlsx")),
#'       uiOutput("excel_sheet_ui"),
#'       tags$hr(),
#'
#'       h4("Read options"),
#'       fluidRow(
#'         column(6, selectInput(
#'           "delim", "Delimiter",
#'           choices = c("Auto" = "auto", "Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
#'           selected = "auto"
#'         )),
#'         column(6, selectInput(
#'           "decimal", "Decimal mark",
#'           choices = c("Auto" = "auto", "Dot (.)" = ".", "Comma (,)" = ","),
#'           selected = "auto"
#'         ))
#'       ),
#'       numericInput("skip", "Skip first lines", value = 0, min = 0, step = 1),
#'       textInput("comment", "Comment prefix (single char; blank = none)", value = "#"),
#'       checkboxInput("has_header", "First non-skipped line has column names", TRUE),
#'
#'       tags$hr(),
#'
#'
#'       h4("2) Map datetime"),
#'       radioButtons("dt_mode", "Datetime mode", choices = c("Unified" = "unified", "Split date + time" = "split"), inline = TRUE),
#'       selectInput("col_datetime", "Unified datetime column", choices = NULL),
#'       selectInput("col_date", "Date column", choices = NULL),
#'       selectInput("col_time", "Time column", choices = NULL),
#'       selectInput("tz", "Target time zone", choices = OlsonNames(), selected = "Europe/Oslo"),
#'       tags$hr(),
#'
#'       h4("3) Map measurements"),
#'       selectInput("col_temp", "Temperature column", choices = NULL),
#'       selectInput("col_rhum", "Relative humidity column", choices = NULL),
#'       selectInput("col_wood", "Wood moisture column", choices = NULL),
#'       selectInput("id_col", "Grouping ID (optional)", choices = NULL),
#'       tags$hr(),
#'
#'       h4("4) Rolling options (row-based in your package)"),
#'       numericInput("roll_k", "Rolling window (rows)", value = 24, min = 2, step = 1),
#'       checkboxGroupInput("roll_do", "Compute rolling means for:",
#'                          choices = c("MIx_mold", "MIx_temp", "MIx_wood"),
#'                          selected = c()),
#'       tags$hr(),
#'
#'       actionButton("process", "Process data", class = "btn-primary"),
#'       width = 4
#'     ),
#'     mainPanel(
#'       tabsetPanel(
#'         tabPanel("Raw preview", DTOutput("raw_tbl")),
#'         tabPanel("Processed preview", DTOutput("proc_tbl")),
#'         tabPanel("Explore",
#'                  fluidRow(
#'                    column(4, selectInput("y_var", "Y variable", choices = NULL)),
#'                    column(4, dateRangeInput("date_rng", "Filter by date (gen_date)")),
#'                    column(4, checkboxInput("prefer_ra", "Prefer *_roll_* if available", value = TRUE))
#'                  ),
#'                  plotOutput("ts_plot", height = "380px")
#'         ),
#'         tabPanel("Download",
#'                  downloadButton("download_csv", "Download processed CSV"),
#'                  br(), br(),
#'                  verbatimTextOutput("summary_out")
#'         )
#'       )
#'     )
#'   )
#' )
#' #
#' #
#' # ui <- fluidPage(
#' #   titlePanel("MycoTools — Data Normalization & Metrics"),
#' #
#' #   sidebarLayout(
#' #     sidebarPanel(
#' #       img(src="mycoteam_logo",height=72,width=72),
#' #       h4("1) Upload"),
#' #       fileInput("file", "Upload CSV or Excel", accept = c(".csv", ".txt", ".xls", ".xlsx")),
#' #       uiOutput("excel_sheet_ui"),
#' #
#' #       h4("Read options"),
#' #       selectInput("delim", "Delimiter",
#' #                   choices = c("Auto" = "auto", "Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
#' #                   selected = "auto"),
#' #       selectInput("decimal", "Decimal mark",
#' #                   choices = c("Auto" = "auto", "Dot (.)" = ".", "Comma (,)" = ","),
#' #                   selected = "auto"),
#' #       numericInput("skip", "Skip first lines", value = 0, min = 0, step = 1),
#' #       textInput("comment", "Comment prefix (single char; leave blank for none)", value = "#"),
#' #       checkboxInput("has_header", "First non-skipped line has column names", TRUE),
#' #
#' #       tags$hr(),
#' #
#' #       h4("2) Map datetime"),
#' #       radioButtons("dt_mode", "Datetime mode", choices = c("Unified" = "unified", "Split date + time" = "split"), inline = TRUE),
#' #       selectInput("col_datetime", "Unified datetime column", choices = NULL),
#' #       selectInput("col_date", "Date column", choices = NULL),
#' #       selectInput("col_time", "Time column", choices = NULL),
#' #       selectInput("tz", "Target time zone", choices = OlsonNames(), selected = "Europe/Oslo"),
#' #       tags$hr(),
#' #
#' #       h4("3) Map measurements"),
#' #       selectInput("col_temp", "Temperature column", choices = NULL),
#' #       selectInput("col_rhum", "Relative humidity column", choices = NULL),
#' #       selectInput("col_wood", "Wood moisture column", choices = NULL),
#' #       selectInput("id_col", "Grouping ID (optional)", choices = NULL),
#' #       tags$hr(),
#' #
#' #       h4("4) Rolling options (row-based in your package)"),
#' #       numericInput("roll_k", "Rolling window (rows)", value = 24, min = 2, step = 1),
#' #       checkboxGroupInput("roll_do", "Compute rolling means for:",
#' #                          choices = c("MIx_mold", "MIx_temp", "MIx_wood"),
#' #                          selected = c()),
#' #       tags$hr(),
#' #
#' #       actionButton("process", "Process data", class = "btn-primary"),
#' #       width = 4
#' #     ),
#' #
#' #     mainPanel(
#' #       tabsetPanel(
#' #         tabPanel("Raw preview", DTOutput("raw_tbl")),
#' #         tabPanel("Processed preview", DTOutput("proc_tbl")),
#' #         tabPanel("Explore",
#' #                  fluidRow(
#' #                    column(4, selectInput("y_var", "Y variable", choices = NULL)),
#' #                    column(4, dateRangeInput("date_rng", "Filter by date (gen_date)")),
#' #                    column(4, checkboxInput("prefer_ra", "Prefer *_roll_* if available", value = TRUE))
#' #                  ),
#' #                  plotOutput("ts_plot", height = "380px")
#' #         ),
#' #         tabPanel("Download",
#' #                  downloadButton("download_csv", "Download processed CSV"),
#' #                  br(), br(),
#' #                  verbatimTextOutput("summary_out")
#' #         )
#' #       )
#' #     )
#' #   )
#' # )
#'
#' # ---- SERVER ------------------------------------------------------------------
#' server <- function(input, output, session) {
#'
#'   # -- Loaders -----------------------------------------------------------------
#'   raw_data <- reactiveVal(NULL)
#'
#'   output$excel_sheet_ui <- renderUI({
#'     req(input$file)
#'     ext <- tools::file_ext(input$file$name)
#'     if (tolower(ext) %in% c("xls", "xlsx")) {
#'       pth <- input$file$datapath
#'       sheets <- tryCatch(readxl::excel_sheets(pth), error = function(e) NULL)
#'       selectInput("excel_sheet", "Excel sheet", choices = sheets %||% 1, selected = 1)
#'     } else NULL
#'   })
#'
#'   observeEvent(list(input$file, input$excel_sheet), {
#'     req(input$file)
#'     pth <- input$file$datapath
#'     ext <- tools::file_ext(input$file$name)
#'     df <- tryCatch({
#'       if (tolower(ext) %in% c("xls","xlsx")) {
#'         MycoTools::import_data(
#'           path  = pth,
#'           sheet = if (!is.null(input$excel_sheet)) input$excel_sheet else 1
#'         )
#'       } else {
#'         MycoTools::import_data(
#'           path      = pth,
#'           delim     = input$delim   %||% "auto",
#'           decimal   = input$decimal %||% "auto",
#'           skip      = input$skip    %||% 0,
#'           # import_data supports single-char comment markers → take the first char if user typed more
#'           comment   = if (!is.null(input$comment) && nzchar(input$comment)) substr(input$comment, 1, 1) else NULL,
#'           col_names = isTRUE(input$has_header),
#'           guess_max = 10000
#'         )
#'       }
#'     }, error = function(e) {
#'       showNotification(paste("Failed to read file:", e$message), type = "error")
#'       NULL
#'     })
#'
#'     # df <- tryCatch({
#'     #   if (tolower(ext) %in% c("xls","xlsx")) {
#'     #     readxl::read_excel(pth, sheet = if (!is.null(input$excel_sheet)) input$excel_sheet else 1)
#'     #   } else {
#'     #     read_any_tabular(
#'     #       path      = pth,
#'     #       delim     = input$delim %||% "auto",
#'     #       decimal   = input$decimal %||% "auto",
#'     #       skip      = input$skip %||% 0,
#'     #       comment   = input$comment %||% "",
#'     #       col_names = isTRUE(input$has_header),
#'     #       guess_max = 10000
#'     #     )
#'     #   }
#'     # }, error = function(e) {
#'     #   showNotification(paste("Failed to read file:", e$message), type = "error")
#'     #   NULL
#'     # })
#'     raw_data(df)
#'   }, ignoreInit = FALSE)
#'
#'   observeEvent(raw_data(), {
#'     df <- raw_data(); req(df)
#'     nms <- names(df)
#'     updateSelectInput(session, "col_datetime", choices = c("", nms))
#'     updateSelectInput(session, "col_date", choices = c("", nms))
#'     updateSelectInput(session, "col_time", choices = c("", nms))
#'     updateSelectInput(session, "col_temp", choices = c("", nms))
#'     updateSelectInput(session, "col_rhum", choices = c("", nms))
#'     updateSelectInput(session, "col_wood", choices = c("", nms))
#'     updateSelectInput(session, "id_col", choices = c("", nms))
#'
#'     num_candidates <- nms[vapply(df, is.numeric, logical(1))]
#'     updateSelectInput(session, "y_var", choices = c("", num_candidates))
#'   })
#'
#'   processed_data <- eventReactive(input$process, {
#'     req(raw_data())
#'     req(iv$is_valid())   # <-- hard gate: don’t run unless inputs are valid
#'
#'     df <- raw_data()
#'
#'     # 1) Normalize datetime via MycoTools
#'     tz_val <- if (is.null(input$tz) || input$tz == "") "Europe/Oslo" else input$tz
#'     if (identical(input$dt_mode, "unified")) {
#'       col_dt <- if (!is.null(input$col_datetime) && input$col_datetime != "") input$col_datetime else NULL
#'       df <- MycoTools::define_variables_datetime(
#'         data = df,
#'         input_datetime = col_dt,
#'         tz = tz_val,
#'         quiet = TRUE
#'       )
#'     } else {
#'       col_d <- if (!is.null(input$col_date) && input$col_date != "") input$col_date else NULL
#'       col_t <- if (!is.null(input$col_time) && input$col_time != "") input$col_time else NULL
#'       df <- MycoTools::define_variables_datetime(
#'         data = df,
#'         input_date = col_d,
#'         input_time = col_t,
#'         tz = tz_val,
#'         quiet = TRUE
#'       )
#'     }
#'
#'     # 2) Map measurements to canonical columns used by your MycoIndex funcs
#'     #    (do this with plain assignments to avoid tidy-eval edge cases)
#'     if (!is.null(input$col_temp) && input$col_temp != "")  df$gen_temp <- df[[input$col_temp]]
#'     if (!is.null(input$col_rhum) && input$col_rhum != "")  df$gen_rhum <- df[[input$col_rhum]]
#'     if (!is.null(input$col_wood) && input$col_wood != "")  df$gen_wood <- df[[input$col_wood]]
#'     if (!("gen_temp" %in% names(df))) df$gen_temp <- NA_real_
#'     if (!("gen_rhum" %in% names(df))) df$gen_rhum <- NA_real_
#'     if (!("gen_wood" %in% names(df))) df$gen_wood <- NA_real_
#'
#'     # 3) Add seasons / ISO week (only if exported)
#'     if ("add_date_seasons" %in% getNamespaceExports("MycoTools")) {
#'       df <- MycoTools::add_date_seasons(df, input_date = gen_datetime)
#'     }
#'
#'     # 4) MycoIndex components
#'     if ("make_mycoindex_mold" %in% getNamespaceExports("MycoTools")) df <- MycoTools::make_mycoindex_mold(df)
#'     if ("make_mycoindex_temp" %in% getNamespaceExports("MycoTools")) df <- MycoTools::make_mycoindex_temp(df)
#'     if ("make_mycoindex_wood" %in% getNamespaceExports("MycoTools")) df <- MycoTools::make_mycoindex_wood(df)
#'
#'     # 5) Optional rolling means (row-based, using your wrappers)
#'     k <- if (is.null(input$roll_k)) 24 else as.integer(input$roll_k)
#'     if (length(input$roll_do)) {
#'       if ("MIx_mold" %in% names(df) && "MIx_mold" %in% input$roll_do &&
#'           "make_rolling_mix_mold" %in% getNamespaceExports("MycoTools")) {
#'         out_name <- rlang::sym(paste0("MIx_mold_roll_", k, "rows"))
#'         df <- MycoTools::make_rolling_mix_mold(df, input = MIx_mold, output_name = out_name, roll_interval = k)
#'       }
#'       if ("MIx_temp" %in% names(df) && "MIx_temp" %in% input$roll_do &&
#'           "make_rolling_mix_temp" %in% getNamespaceExports("MycoTools")) {
#'         out_name <- rlang::sym(paste0("MIx_temp_roll_", k, "rows"))
#'         df <- MycoTools::make_rolling_mix_temp(df, input = MIx_temp, output_name = out_name, roll_interval = k)
#'       }
#'       if ("MIx_wood" %in% names(df) && "MIx_wood" %in% input$roll_do &&
#'           "make_rolling_mix_wood" %in% getNamespaceExports("MycoTools")) {
#'         out_name <- rlang::sym(paste0("MIx_wood_roll_", k, "rows"))
#'         df <- MycoTools::make_rolling_mix_wood(df, input = MIx_wood, output_name = out_name, roll_interval = k)
#'       }
#'     }
#'
#'     # 6) Order columns for readability
#'     canon_first <- c("gen_datetime","gen_date","gen_time","gen_temp","gen_rhum","gen_wood",
#'                      "MIx_mold","MIx_temp","MIx_wood")
#'     keep <- intersect(canon_first, names(df))
#'     df[, c(keep, setdiff(names(df), keep))]
#'   })
#'
#'   # -- Tables ------------------------------------------------------------------
#'   output$raw_tbl <- renderDT({ req(raw_data()); datatable(raw_data(), options = list(scrollX = TRUE, pageLength = 10)) })
#'   output$proc_tbl <- renderDT({ req(processed_data()); datatable(processed_data(), options = list(scrollX = TRUE, pageLength = 10)) })
#'
#'   # -- Explorer ----------------------------------------------------------------
#'   output$ts_plot <- renderPlot({
#'     req(processed_data(), input$y_var)
#'     d <- processed_data(); req("gen_datetime" %in% names(d))
#'
#'     # Optional date filter
#'     if (!is.null(input$date_rng) && all(!is.na(input$date_rng))) {
#'       d <- d %>% filter(gen_date >= input$date_rng[1], gen_date <= input$date_rng[2])
#'     }
#'
#'     y <- input$y_var
#'     # prefer matching *_roll_* var if requested
#'     if (isTRUE(input$prefer_ra)) {
#'       cand <- grep(paste0("^", y, "_roll_"), names(d), value = TRUE)
#'       if (length(cand)) y <- cand[1]
#'     }
#'
#'     d <- d %>% arrange(gen_datetime)
#'     ggplot(d, aes(x = gen_datetime, y = .data[[y]])) +
#'       geom_line() +
#'       labs(x = "gen_datetime", y = y) +
#'       theme_minimal(base_size = 12)
#'   })
#'
#'   # -- Download ----------------------------------------------------------------
#'   output$download_csv <- downloadHandler(
#'     filename = function() paste0("mycotools_processed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
#'     content  = function(file) readr::write_csv(processed_data(), file, na = "")
#'   )
#'
#'   output$summary_out <- renderPrint({
#'     req(processed_data()); d <- processed_data()
#'     list(
#'       n_rows = nrow(d),
#'       n_cols = ncol(d),
#'       cols   = names(d),
#'       range_gen_datetime = if ("gen_datetime" %in% names(d)) range(d$gen_datetime, na.rm = TRUE) else NULL
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
