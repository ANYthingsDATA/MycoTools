# Frost API example (frost.met.no)
#
# Adapted from https://frost.met.no/r_example.html
#
# Setup
# -----
# 1. Register a client at https://frost.met.no/auth/requestCredentials.html
# 2. Add the credentials to your user-level ~/.Renviron (NOT committed):
#
#      FROST_CLIENT_ID=<your client id>
#      FROST_CLIENT_SECRET=<your client secret>
#
# 3. Restart R so the new environment variables are picked up.
#
# The Frost API uses the client ID as the HTTP basic-auth username
# (password empty). The secret is only needed if you switch to the
# OAuth2 token flow — this example uses the simpler client-id flow.

library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(frostr) # https://cran.r-project.org/web/packages/frostr/frostr.pdf

client_id <- Sys.getenv("FROST_CLIENT_ID")
if (!nzchar(client_id)) {
  stop(
    "FROST_CLIENT_ID is not set. See the header of this file for setup ",
    "instructions.",
    call. = FALSE
  )
}

# Quick exploration via the frostr package
frostr_elements           <- frostr::get_elements(client_id = client_id)
frostr_elements_available <- frostr::get_available_timeseries(client_id = client_id, sources = "SN18700")

# Direct API call: build a URL to a Frost endpoint with parameters for
# sources, elements, and the reference-time range.
endpoint      <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")
sources       <- "SN18700,SN90450"
elements      <- "mean(air_temperature P1D), mean(relative_humidity P1D), sum(precipitation_amount P1D)"
referenceTime <- "2010-04-01/2022-04-03"

url <- paste0(
  endpoint, "?",
  "sources=",        sources,
  "&referencetime=", referenceTime,
  "&elements=",      elements
)

# See https://frost.met.no/dataclarifications.html for timeOffset semantics.

xs <- try(jsonlite::fromJSON(URLencode(url), flatten = TRUE))

if (!inherits(xs, "try-error")) {
  df <- tidyr::unnest(xs$data, cols = c(observations))
  message("Data retrieved from frost.met.no.")
} else {
  stop("Frost data retrieval failed.", call. = FALSE)
}

df2 <- df %>%
  dplyr::group_by(sourceId) %>%
  dplyr::select(sourceId, referenceTime, elementId, value, unit, timeOffset) %>%
  dplyr::filter(!(elementId == "mean(air_temperature P1D)"     & timeOffset == "PT0H")) %>%
  dplyr::filter(!(elementId == "sum(precipitation_amount P1D)" & timeOffset == "PT6H")) %>%
  dplyr::mutate(gen_date = lubridate::ymd_hms(referenceTime))

df2 %>%
  dplyr::filter(elementId == "mean(relative_humidity P1D)") %>%
  ggplot2::ggplot(ggplot2::aes(y = value, x = gen_date, color = sourceId, group = sourceId)) +
  ggplot2::geom_point()
