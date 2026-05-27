# MycoTools

![Version](https://img.shields.io/badge/version-0.2.2-blue.svg)
![R](https://img.shields.io/badge/R-%E2%89%A5%204.3-blue.svg)
![License](https://img.shields.io/badge/license-Proprietary-red.svg)

> Proprietary software. Copyright © 2024–2026 ANYthings v/ Anders B. Nygaard
> and Mycoteam AS. All rights reserved. See [LICENSE](LICENSE).

## Overview

**MycoTools** is an R package for importing, normalising, and analysing
indoor-climate and moisture sensor data from heterogeneous logger formats. It
is the data-processing **engine** behind the Mycoteam tooling, and also ships
with an optional, user-friendly **Shiny app** front-end (`run_app()`).

The package provides:

- **Robust import** of CSV / CSV2 / TSV / Excel logger exports, with
  auto-detected delimiter and decimal mark (handles European decimal commas).
- **Datetime normalisation** from either a unified datetime column or split
  date + time columns.
- **Sensor-ID normalisation**, decimal-comma-aware numeric coercion, and
  gap-filling onto a regular per-group time interval.
- **Season and ISO-week derivation** for calendar-based aggregation.
- The **MYCOindex** risk-scoring system for mold, temperature, and wood
  moisture conditions.

Developed by [ANYthings](https://anythings.no) v/ Anders B. Nygaard for
Mycoteam AS.

## Prerequisites

- **R ≥ 4.3** — https://cran.r-project.org/bin/windows/base/
- **RStudio Desktop** (recommended for analysts) —
  https://posit.co/download/rstudio-desktop/
- **Rtools** (Windows, recommended for building packages from source) —
  https://cran.r-project.org/bin/windows/Rtools/

Install the `devtools` helper if you don't already have it:

```r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
```

Runtime dependencies (`dplyr`, `tidyr`, `lubridate`, `readr`, `readxl`,
`shiny`, `bslib`, `DT`, `plotly`, `writexl`, …) are resolved automatically on
install.

## Installation

### From the public repository (current)

While the repository is public, install the latest tagged release directly —
**no GitHub token required**:

```r
# Latest commit on the default branch
devtools::install_github("ANYthingsDATA/MycoTools")

# …or pin a specific release tag
devtools::install_github("ANYthingsDATA/MycoTools@v0.2.2")
```

### From a private repository (token fallback)

If the repository has been made private again, you must have been granted
access to it and authenticate with a GitHub personal access token (PAT):

```r
# 1. Create a PAT (opens github.com in your browser). Choose the "repo" scope
#    and a sensible expiry; copy the token before you close the page.
usethis::create_github_token()

# 2. Store it so R can authenticate (paste the token when prompted; choose
#    "Replace these credentials" if you are renewing an expired one).
gitcreds::gitcreds_set()

# 3. Install using the stored token.
devtools::install_github(
  "ANYthingsDATA/MycoTools",
  auth_token = gh::gh_token()
)
```

A `401 Unauthorized` error means your token is missing, expired, or lacks
access to the repository — repeat the steps above with a fresh token.

## Usage

A typical pipeline reads a logger file, normalises the columns, scores the
MYCOindex, and adds calendar features. Every `define_variables_*` and
`make_mycoindex_*` function accepts a column reference as **either a bare name
or a string**, so the same code works from analyst scripts and from the Shiny
app.

```r
library(MycoTools)

raw <- import_data("logger_export.csv")        # auto delimiter + decimal mark

processed <- raw |>
  define_variables_datetime(input_datetime = Timestamp, tz = "Europe/Oslo") |>
  define_variables_sensorID(input_sensor = Sensor, input_port = Port) |>
  define_variables_temp(input_temp = Temperature) |>
  define_variables_rhum(input_rhum = RelHumidity) |>
  define_variables_wood(input_wood = WoodMoisture) |>
  make_mycoindex_mold(input_mold = gen_rhum) |>  # RH: <75=0, 85=0.5, ≥95=1
  make_mycoindex_temp(input_temp = gen_temp) |>  # °C: <4=0, 14–35=1, ≥35=0
  make_mycoindex_wood(input_wood = gen_wood) |>
  add_date_seasons(gen_datetime)                 # season, ISO week/year, month

# Fill gaps onto a regular per-group time spine (returns the spine; left-join
# it back to `processed` to expose the gaps).
spine <- make_complete_date(
  processed,
  input_date      = "gen_date",
  input_site_id   = "SiteID",
  input_sensor_id = "gen_sensorID",
  timeframe       = "hour"
)
```

### Launch the Shiny app

The package bundles a Shiny front-end that wraps the same functions for
import, processing, visualisation, and export. Its dependencies are hard
`Imports`, so it runs standalone with no extra setup:

```r
MycoTools::run_app()
```

## Function reference

| Function | Purpose |
|----------|---------|
| `run_app()` | Launch the bundled Shiny app. |
| `import_data()` | Read CSV / CSV2 / TSV / Excel with auto delimiter & decimal-mark detection. |
| `define_variables_datetime()` | Normalise a unified or split date/time into `gen_datetime`, `gen_date`, `gen_time`. |
| `define_variables_date()` | Date-only convenience wrapper around `define_variables_datetime()`. |
| `define_variables_sensorID()` | Build `gen_sensorID` from a sensor (and optional port) column. |
| `define_variables_temp()` / `_rhum()` / `_wood()` / `_ohm()` | Numeric coercion with decimal-comma handling. |
| `make_mycoindex_mold()` / `_temp()` / `_wood()` | MYCOindex risk scoring against configurable thresholds. |
| `make_complete_date()` | Generate a per-group regular-interval date spine. |
| `add_date_seasons()` | Add season, year-season, ISO week/year, and month label/number. |
| `make_rolling_mix_mold()` / `_temp()` / `_wood()` | _Experimental — time-aware rolling means over the MYCOindex columns (helper not yet implemented; not functional)._ |

## Documentation

Every exported function has roxygen help — use `?function_name` in R, e.g.
`?import_data` or `?make_mycoindex_mold`. A `_pkgdown.yml` is included for
building a documentation site with `pkgdown::build_site()`.

## Contributing

This is an internal, proprietary package. If you have access and want to
contribute, change the engine in `R/` and the bundled app in `inst/shiny/`,
bump `Version:` in `DESCRIPTION`, add a `NEWS.md` entry, and update the tests.
Report bugs and feature requests on the
[GitHub Issues page](https://github.com/ANYthingsDATA/MycoTools/issues).

## Acknowledgments

MycoTools builds on the R ecosystem — notably **dplyr**, **tidyr**,
**lubridate**, **readr**, **readxl**, **parsedate**, and **rlang** for the
data-processing engine, and **shiny**, **bslib**, **DT**, **plotly**, and
**writexl** for the bundled app.

## License

This package is proprietary software. Copyright © 2024–2026 ANYthings v/
Anders B. Nygaard and Mycoteam AS. All rights reserved. See the
[LICENSE](LICENSE) file for the full terms.

## Author

- **[ANYthings](https://anythings.no) v/ Anders B. Nygaard** · anders [at] anythings.no
- Copyright holders: ANYthings v/ Anders B. Nygaard and Mycoteam AS
