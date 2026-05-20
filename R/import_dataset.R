#' Import tabular logger files (CSV/CSV2/TXT) and Excel
#'
#' @description
#' Reads text-delimited files (CSV, CSV2, TXT) with auto-detected delimiter
#' (comma/semicolon/tab) and decimal mark (dot/comma), supports skipping leading
#' lines and ignoring comment-prefixed lines. Also reads Excel (XLS/XLSX).
#'
#' @param path File path to the input file.
#' @param sheet For Excel files: sheet name or index (default 1).
#' @param delim Delimiter for text files: "auto" (default), ",", ";", or "\\t".
#' @param decimal Decimal mark for text files: "auto" (default), "." or ",".
#' @param skip Integer. Number of lines to skip at the top (default 0).
#' @param comment Single character used to indicate comment lines (e.g., "#").
#'   Lines starting with this (after optional whitespace) are ignored. Use `NULL`
#'   or "" to disable.
#' @param col_names Logical. Whether the first non-skipped, non-comment line
#'   contains column names (default TRUE).
#' @param guess_max Passed to readr for type guessing (default 10000).
#'
#' @return A tibble.
#'
#' @examples
#' # Text file with metadata header and '#' comments:
#' # MycoTools::import_data("log.txt", skip = 2, comment = "#")
#'
#' # Excel first sheet:
#' # MycoTools::import_data("log.xlsx")
#'
#' @importFrom tools file_ext
#' @importFrom readr read_delim locale
#' @importFrom readxl read_excel
#' @export
import_data <- function(path,
                        sheet = 1,
                        delim = c("auto", ",", ";", "\t"),
                        decimal = c("auto", ".", ","),
                        skip = 0,
                        comment = NULL,
                        col_names = TRUE,
                        guess_max = 10000) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xls", "xlsx")) {
    # Detect the actual format from file content — some exports use an
    # Excel extension but contain HTML, CSV, or an unsupported old binary.
    fmt <- tryCatch(readxl::excel_format(path, guess = TRUE),
                    error = function(e) NULL)
    if (is.null(fmt)) {
      stop(sprintf(
        "The file has an %s extension but is not a valid Excel binary. ",
        toupper(ext)
      ), "It may be an HTML export, a very old Excel 5/95 file, or corrupt. ",
         "Open it in Excel and save as .xlsx, then try again.",
         call. = FALSE)
    }
    return(tryCatch(
      readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("libxls|Unable to open|BIFF", msg, ignore.case = TRUE)) {
          stop(sprintf(
            "Could not read this %s file (%s). It may be an unsupported old Excel format (5.0/95) or corrupt. Open it in Excel and save as .xlsx, then try again.",
            toupper(ext), msg), call. = FALSE)
        }
        stop(e)
      }
    ))
  }

  # Text-delimited path
  delim   <- match.arg(delim)
  decimal <- match.arg(decimal)

  if (identical(delim, "auto")) {
    delim <- .myc_read_best_delim(path, comment)
  }

  dec_mark <- if (identical(decimal, "auto")) {
    # csv2 (;) usually pairs with decimal comma
    if (identical(delim, ";")) "," else "."
  } else {
    decimal
  }

  loc <- readr::locale(decimal_mark = dec_mark, encoding = "UTF-8")

  readr::read_delim(
    file      = path,
    delim     = delim,
    skip      = skip,
    comment   = if (!is.null(comment) && nzchar(comment)) comment else "",
    col_names = col_names,
    guess_max = guess_max,
    locale    = loc,
    na        = c("", "NA", "NaN")
  )
}

# Internal helper: pick best delimiter from first N lines (ignoring comments)
# Not exported.
# @keywords internal
# @importFrom utils readLines
.myc_read_best_delim <- function(path, comment = NULL, n_max = 50) {
  lines <- try(readLines(path, n = n_max, warn = FALSE), silent = TRUE)
  if (inherits(lines, "try-error")) return(",")

  lines <- lines[nzchar(lines)]

  if (!is.null(comment) && nzchar(comment)) {
    # Escape regex metacharacters in comment char
    esc <- gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1", comment)
    lines <- lines[!grepl(paste0("^\\s*", esc), lines)]
  }

  if (!length(lines)) return(",")

  cands <- c(",", ";", "\t")
  score <- vapply(cands, function(d) {
    mean(vapply(strsplit(lines, d, fixed = TRUE), length, integer(1)), na.rm = TRUE)
  }, numeric(1))

  cands[which.max(score)]
}


# Omnisense
# Scantronic
# Klimadata
# Rotronic
#
#
# import_data_csv <- function(input_file,
#                             site_ID,
#                             encoding,
#                             sep,
#                             dec,
#                             fill,
#                             na.strings) {
#   data <- data.table::fread(
#     file = input_file,
#     encoding = encoding,
#     sep = sep,
#     dec = dec,
#     fill = fill,
#     na.strings = na.strings,
#   ) %>%
#     mutate(site_ID = {{ site_ID }}) %>%
#     select(site_ID, everything())
#
# import_data_csv_omnisense <- function(input_file,
#                                       lines_to_skip,
#                                       site_ID,
#                                       fill = TRUE,
#                                       blank.lines.skip = TRUE,
#                                       encoding,
#                                       sep,
#                                       dec,
#                                       na.strings) {
#   data <- data.table::fread(
#     file = input_file,
#     skip = lines_to_skip,
#     encoding = encoding,
#     sep = sep,
#     dec = dec,
#     fill = fill,
#     blank.lines.skip = blank.lines.skip,
#     na.strings = na.strings,
#   ) %>%
#     mutate(site_ID = {{ site_ID }}) %>%
#     select(site_ID, everything())
# }
#
# import_data_tsv_omnisense <- function(input_file,
#                                       lines_to_skip,
#                                       site_ID,
#                                       encoding,
#                                       sep,
#                                       dec,
#                                       fill,
#                                       na.strings) {
#   data <- readxl::read_excel(
#     file = input_file,
#     skip = lines_to_skip,
#     encoding = encoding,
#     sep = sep,
#     dec = dec,
#     fill = fill,
#     na.strings = na.strings,
#   ) %>%
#     mutate(site_ID = {{ site_ID }}) %>%
#     select(site_ID, everything())
# }
