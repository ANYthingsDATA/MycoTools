#' Launch the MycoTools Shiny application
#'
#' Starts the bundled MycoTools data platform: import, process, visualise and
#' export indoor-climate and moisture sensor data. The app lives in
#' `inst/shiny/` and is launched via [shiny::runApp()].
#'
#' The app's dependencies (\pkg{shiny}, \pkg{bslib}, \pkg{DT}, \pkg{plotly},
#' \pkg{writexl}) are declared in `Imports` (since 0.2.1), so they are installed
#' automatically with the package and `run_app()` works standalone with no extra
#' setup. As a safeguard, `run_app()` still verifies they are present and stops
#' with an actionable message if any are missing.
#'
#' @param display.mode Passed to [shiny::runApp()]; one of `"normal"` or
#'   `"showcase"`. Defaults to `"normal"`.
#' @param ... Further arguments forwarded to [shiny::runApp()] (e.g. `port`,
#'   `launch.browser`, `host`).
#' @return Invisibly returns `NULL`; called for the side effect of running the
#'   Shiny app.
#' @export
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function(display.mode = c("normal", "showcase"), ...) {
  display.mode <- match.arg(display.mode)

  required <- c("shiny", "bslib", "DT", "plotly", "writexl")
  installed <- vapply(required, requireNamespace, logical(1), quietly = TRUE)
  if (any(!installed)) {
    missing <- required[!installed]
    stop(
      "run_app() needs these packages, which are not installed: ",
      paste(missing, collapse = ", "), ".\n",
      "Install them with:\n  install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", package = "MycoTools")
  if (!nzchar(app_dir)) {
    stop(
      "Could not locate the bundled Shiny app (inst/shiny). ",
      "Try reinstalling MycoTools.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, display.mode = display.mode, ...)
}
