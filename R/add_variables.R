#' Add season, year-season, month (numeric/label), and ISO week variables
#'
#' @description
#' Creates calendar features from a datetime column:
#' - `gen_date_month_num` (1–12), `gen_date_month_lab` (Jan–Dec),
#' - `gen_season` ("Winter","Spring","Summer","Fall"),
#' - `gen_year_season` (e.g., "2024, Winter") where **December is assigned to the next year's Winter**
#'   (intentional, to keep winter seasons together: Dec–Feb),
#' - `gen_isoweek` (ISO 8601 week, Monday-start) and `gen_isoyear` (paired ISO year).
#'
#' Note: ISO weeks follow ISO-8601 rules (week 1 is the week with the first Thursday).
#' At year boundaries, `gen_isoyear` may differ from the calendar year.
#'
#' @param data A data frame.
#' @param input_date Bare name of the datetime column (default `gen_datetime`).
#'
#' @return The input data with added season/month/isoweek columns.
#'
#' @examples
#' # df %>% add_date_seasons(gen_datetime)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom lubridate month year isoweek isoyear
#' @importFrom rlang :=
#' @export
add_date_seasons <- function(data = data,
                             input_date = gen_datetime) {

  data %>%
    dplyr::mutate(
      gen_date_month_num = lubridate::month({{ input_date }}),
      gen_date_month_lab = lubridate::month({{ input_date }}, label = TRUE, abbr = TRUE),
      .tmp_yr            = lubridate::year({{ input_date }}),
      # Season mapping by numeric month (robust to locale)
      gen_season = dplyr::case_when(
        gen_date_month_num %in% c(12, 1, 2) ~ "Winter",
        gen_date_month_num %in% 3:5         ~ "Spring",
        gen_date_month_num %in% 6:8         ~ "Summer",
        gen_date_month_num %in% 9:11        ~ "Fall",
        TRUE                                ~ NA_character_
      ),
      # NOTE: Intentional — December assigned to next year's Winter (Dec–Feb).
      gen_year_season = dplyr::case_when(
        gen_date_month_num == 12 ~ paste0(.tmp_yr + 1, ", ", gen_season),
        TRUE                     ~ paste0(.tmp_yr, ", ", gen_season)
      ),
      gen_isoweek = lubridate::isoweek({{ input_date }}),
      gen_isoyear = lubridate::isoyear({{ input_date }})
    ) %>%
    dplyr::select(-dplyr::starts_with(".tmp_"))
}


#' #' Add date and seasons to a data frame
#' #'
#' #' This function takes a data frame and a date column as input, adds month, year, season, and a combined year-season column to the data frame.
#' #'
#' #' @param data A data frame. Default is 'data'.
#' #' @param input_date A date column in the data frame. Default is 'gen_date'.
#' #' @return A data frame with added columns for month, year, season, and year-season.
#' #' @importFrom tidyverse "%>%"
#' #' @importFrom lubridate month year
#' #' @export
#' #'
#' add_date_seasons <- function(data = data,
#'                              input_date = gen_datetime) {
#'   library(tidyverse)
#'   library(lubridate)
#'   x <- data %>%
#'     # Get month and year
#'     mutate(
#'       gen_date_month = month({{ input_date }}, label = TRUE),
#'       gen_date_year = year({{ input_date }})
#'     ) %>%
#'     # Generate season
#'     mutate(gen_season = case_when(
#'       gen_date_month == "Dec" | gen_date_month == "Jan" | gen_date_month == "Feb" ~ "Winter",
#'       gen_date_month == "Mar" | gen_date_month == "Apr" | gen_date_month == "May" ~ "Spring",
#'       gen_date_month == "Jun" | gen_date_month == "Jul" | gen_date_month == "Aug" ~ "Summer",
#'       gen_date_month == "Sep" | gen_date_month == "Oct" | gen_date_month == "Nov" ~ "Fall"
#'     )) %>%
#'     # Generate season and year variable
#'     mutate(gen_year_season = case_when(
#'       gen_date_month == "Dec" ~ paste0(gen_date_year + 1, ", ", gen_season),
#'       TRUE ~ paste0(gen_date_year, ", ", gen_season)
#'     ))
#'
#'   return(x)
#' }
