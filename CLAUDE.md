# CLAUDE.md — MycoTools

R package providing the data-processing **engine** for the Mycoteam tooling,
plus an optional, bundled Shiny app (`run_app()`) as a user-friendly
front-end for the same functions. The engine is the package's main purpose;
the app is an important convenience. Consumed by analyst scripts and by the
`ShinyMycoTools/` deploy shell (which installs this package from GitHub and
serves its bundled app).

Standard R package layout: exported engine logic under `R/`, the bundled
app under `inst/shiny/`, docs under `man/`, dependencies in `DESCRIPTION`.

## Layout

```
MycoTools/
├── DESCRIPTION           # version, Imports, Suggests (app deps), license, authors
├── NAMESPACE             # hand-managed: exportPattern("^[[:alpha:]]+")
├── R/                    # engine source + run_app.R; "." prefix = internal
├── inst/shiny/app.R      # the bundled Shiny app (served via run_app())
├── man/                  # roxygen-generated; do not hand-edit
├── tests/testthat/       # (empty — see TODOS)
├── NEWS.md
├── LICENSE               # proprietary (placeholder wording — see TODOS)
└── frost_api_v1.R        # standalone example, NOT part of the package build
```

## API surface

| Function | What it does |
|----------|--------------|
| `run_app()` (`run_app.R`) | Launches the bundled Shiny app via `shiny::runApp(system.file("shiny", package = "MycoTools"))`. App deps are hard `Imports`, so it runs standalone with no extra setup. |
| `import_data()` (`import_dataset.R`) | CSV / CSV2 / TSV / Excel reader with auto delimiter + decimal-mark detection. |
| `define_variables_datetime()` | Parses unified or split date/time columns into `gen_datetime`, `gen_date`, `gen_time`. |
| `define_variables_date()` | Wrapper around `_datetime()` for date-only inputs. |
| `define_variables_sensorID()` | Builds `gen_sensorID` from a sensor (and optional port) column. |
| `define_variables_temp / _rhum / _wood / _ohm()` | Numeric coercion with decimal-comma handling. |
| `make_complete_date()` (`make_complete_date.R`) | Generates a per-group regular-interval date spine. Returns spine only; the app left-joins it back to the full data. |
| `make_mycoindex_mold / _temp / _wood()` | MYCOindex risk scoring (0, 0.25/0.2, 0.5/0.4, 1). |
| `add_date_seasons()` (`add_variables.R`) | Season, `gen_year_season`, ISO week/year, month label & number. |
| `make_rolling_mix_mold / _temp / _wood()` (`make_rolling.R`) | Time-aware rolling mean over the MYCOindex columns. Wrappers around the (currently unbuilt) helper `make_rolling_time_mean()`. |

## The bundled Shiny app

Since 0.2.0 the Shiny UI lives in `inst/shiny/app.R` and is launched with the
exported `run_app()`. The app is an **optional, user-facing front-end** — not
the package's main purpose — and is a thin UI that calls into this package's
own exported functions for all data processing. Its dependencies (`shiny`,
`bslib`, `DT`, `plotly`, `writexl`) are hard `Imports` (since 0.2.1), so
`run_app()` works standalone in RStudio for advanced users and the Connect
Cloud deploy resolves them automatically (renv/`writeManifest()` follow
`Imports`, not `Suggests`).

For deployment, the `ShinyMycoTools/` repo serves this bundled app via
`shiny::shinyAppDir(system.file("shiny", package = "MycoTools"))`. Do not copy
app code back into `ShinyMycoTools/` — change it here, then push a tagged
release and bump the pin.

## Internal helpers (not exported)

Two convention helpers live inside `R/define_variables.R`. They are not
exported because they start with `.`:

- **`.resolve_col(x)`** — accepts a column reference as either a bare symbol
  or a single character string and returns the column name as a string. This
  enables the package to be called both from analyst R scripts
  (`define_variables_temp(df, Temperature)`) and from Shiny
  (`define_variables_temp(df, input$map_temp)`).
- **`.parse_numeric(x)`** — coerces to `double`, but first translates `,` →
  `.` so European-locale CSVs work. Numeric inputs pass through unchanged.
  Used by `_temp / _rhum / _wood / _ohm`.

Do not bypass these — call `as.double()` directly only if you have
specifically decided not to handle decimal comma.

## Tidy-eval conventions

- **`define_variables_*`** and **`make_mycoindex_*`**: use `{{ }}` for the
  *output* column (`{{ output_temp }} := ...`) and resolve the *input* column
  via `.resolve_col()` so callers can pass a bare name *or* a string. Inside
  `mutate()` the value is accessed with `.data[[col]]`.
- **`make_complete_date()`**: takes **plain character column names only**
  (`input_date = "gen_date"`). Converts to `rlang::sym()` internally.
- **`add_date_seasons()`**: takes a bare name via `{{ input_date }}` (default
  `gen_datetime`).

In Shiny code, never wrap an input value with `!!sym(input$X)` — `!!sym`
inside this package's `mutate()` is double-negation in base R and does *not*
inject. Pass `input$X` as a plain string; the package resolves it.

## Dependencies

Declared in `DESCRIPTION`.

- **Imports — engine:** `dplyr`, `tidyr`, `lubridate` (data manipulation,
  datetime parsing); `rlang` (`:=`, `sym`, `ensym`, `enquo`, `as_name`,
  `.data`); `parsedate` (`parse_date()` in the split date+time path);
  `readr`, `readxl`, `tools` (`import_data()`); `magrittr` (pipe re-export).
  `data.table` and `zoo` are declared but only referenced from commented-out
  code — they produce an `R CMD check` NOTE; see `../TODOS.md`.
- **Imports — bundled app:** `shiny`, `bslib`, `DT`, `plotly`, `writexl`
  (hard deps since 0.2.1 so `run_app()` and the deploy resolve them without
  relying on `Suggests`-following).
- **Suggests:** `testthat`.

## Build & release

The old vendored-tarball flow (`vendor_mycotools.R`) is **retired**. Build
and install normally:

```r
devtools::install("MycoTools")   # local install for dev / analysts
MycoTools::run_app()             # launch the bundled app
devtools::document("MycoTools")  # regenerate man/ (NAMESPACE is hand-managed,
                                 # has no roxygen sentinel, so roxygen skips it)
```

Release: bump `Version:` + `NEWS.md`, commit, tag `vX.Y.Z`, and push to the
public GitHub repo. The deploy shell pins this tag via
`renv::install("git::https://github.com/ANYthingsDATA/MycoTools.git@vX.Y.Z")`
(the `git::` clone form — the `owner/repo@ref` API form hits GitHub's
unauthenticated rate limit). See the umbrella
`CLAUDE.md` for the full cross-repo flow.

## Tests

`tests/testthat/` is currently empty. Tracked in `../TODOS.md`. Priority
tests when added:

- `.parse_numeric()`: `"23,5"`, `"23.5"`, `23.5`, factor, NA, empty.
- `make_complete_date()`: multi-group case that exercises the per-group
  `rowwise() + unnest()` path (the bug fixed in 0.1.0).
- `define_variables_*`: string input vs bare-name input produce the same
  output.
- `import_data()`: HTML-export-with-`.xlsx`-extension raises a clean error.

## What's NOT here

- Branding rules beyond what's embedded in `inst/shiny/app.R`.
- The `frost_api_v1.R` at the repo root is a standalone exploration script,
  not part of the package build (it is `.Rbuildignore`d). It reads
  `FROST_CLIENT_ID` from `Sys.getenv()` — never hardcode credentials.
