# MycoTools 0.2.2

## Bug fixes

* `define_variables_datetime()` / `define_variables_date()` no longer corrupt
  a date column that already carries a time-of-day. When a single datetime
  column (e.g. ISO-8601 `2015-10-07T12:00:00Z`) was mapped as the date with no
  separate time column, the split date+time path appended a default
  `"00:00:00"`, producing an unparseable doubled time (`... 12:00:00 00:00:00`)
  that yielded `NA` for every row of `gen_datetime`/`gen_date`/`gen_time`. This
  also broke downstream date completion (`make_complete_date()` had no valid
  dates to build a spine from). The split path now preserves the time the date
  already carries when no time column is given, and when a time column *is*
  given it combines that time with only the **date part** of the parsed value.
  A `Ymd HM` parse order was added so seconds-less times like `"14:30"` parse.

# MycoTools 0.2.1

## Dependencies

* Moved the bundled app's dependencies (`shiny`, `bslib`, `DT`, `plotly`,
  `writexl`) from `Suggests` to `Imports`. The Shiny app is an optional,
  user-friendly front-end (`run_app()`) — not the package's main purpose —
  but making these hard dependencies means `run_app()` works standalone in
  RStudio without setup, and the Connect Cloud deploy resolves the full stack
  automatically (renv and `rsconnect::writeManifest()` follow `Imports`, not
  `Suggests`).

# MycoTools 0.2.0

## Bundled Shiny app

* The MycoTools Shiny application is now bundled inside the package
  (`inst/shiny/`) and launched with the new exported `run_app()`. It
  previously lived in the separate `ShinyMycoTools` repo and was vendored
  into the app as a tarball; it now travels with the package.
* App-only dependencies (`shiny`, `bslib`, `DT`, `plotly`, `writexl`) are
  declared in `Suggests`. `run_app()` checks they are installed and stops
  with an actionable message if any are missing — the core data-processing
  API has no hard dependency on them.

## Cleanup

* Removed leftover package-skeleton files (`R/hello.R`, `R/test.R`) and the
  stray `hello()` export.

# MycoTools 0.1.0

First release intended for downstream consumption by `ShinyMycoTools`.
Backports fixes and API improvements that originated in the app.

## Bug fixes

* `make_complete_date()`: fixed "`to` must be of length 1" error when
  the data had multiple groups (site / sensor / port). Now generates the
  date sequence per group via `rowwise()` + `tidyr::unnest()` instead of
  passing a vector to `seq()`. Also filters non-finite min/max before
  generating the sequence.
* `import_data()`: detects files with an Excel extension that are not
  valid Excel binaries (HTML exports, very old Excel 5/95 files) via
  `readxl::excel_format()` and raises a user-friendly error instead of
  a cryptic libxls/BIFF message.
* `import_data()`: passes `comment = ""` instead of `NULL` to
  `readr::read_delim()` when the caller did not provide a comment
  character (newer `readr` errors on `NULL`).

## API

* All `define_variables_*()` functions now accept column references as
  **either** a bare symbol **or** a single character string. This
  enables direct use from Shiny (`input$map_temp`) without
  `rlang::sym()` / `!!` gymnastics at the call site. Implemented via a
  new internal helper `.resolve_col()`.
* `make_complete_date()` accepts plain character column names for
  `input_date`, `input_site_id`, `input_sensor_id`, `input_sensor_ports`.
* `define_variables_datetime()`: gained a string-aware `get_col()`
  branch; same string-or-symbol behaviour as the rest of the family.

## Internals

* New internal helper `.parse_numeric()` handles decimal-comma input
  (European locale CSVs) transparently. Used by
  `define_variables_temp()`, `_rhum()`, `_wood()`, and `_ohm()` — these
  no longer fall back to `as.double()`, which silently produced `NA`
  on `"23,5"`-style values.
* `define_variables_datetime()` parses datetimes with
  `lubridate::parse_date_time()` (orders `YmdHMS`, `Ymd HMS`, `dmYHMS`,
  `mdYHMS`, `Ymd`) instead of `parsedate::parse_datetime()`. The split
  date+time path still uses `parsedate::parse_date()` for the date
  half, so `parsedate` remains a dependency.
* `add_variables.R::add_date_seasons()`: refactored so all season
  calculations happen inside a single `dplyr::mutate()` using a
  `.tmp_yr` scratch column that is dropped on the way out. Output is
  unchanged.

## Dependencies

* Added: `readr`, `tidyr`, `tools` (all already used by the code; now
  declared explicitly).

# MycoTools 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
