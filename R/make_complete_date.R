#' Make Date Variable Complete with Regular Intervals
#'
#' This function takes a data frame and completes a date variable to fill in missing time gaps
#' with regular intervals specified by the 'timeframe' argument. It also performs grouping by site
#' and sensor before the completion and ungroups the dataset prior to return.
#'
#' @param data The input data frame containing the date variable to be completed.
#' @param input_date The name or expression of the date variable to complete.
#' @param input_site_id The name or expression of the site ID variable.
#' @param input_sensor_id The name or expression of the sensor ID variable.
#' @param timeframe A character vector specifying the timeframe for regular intervals
#'                 (e.g., "hour", "day", "week", "month").
#'
#' @import dplyr
#'
#' @return A modified data frame with the date variable completed with regular intervals.
#'
#' @examples
#' # Example usage:
#' completed_data <- make_complete_date(
#'   data = my_data, input_date = Date,
#'   input_site_id = SiteID, input_sensor_id = SensorID,
#'   timeframe = "hour"
#' )
#'
#' @export
#'


make_complete_date <- function(data,
                               input_date = "gen_date",
                               input_site_id = NULL,
                               input_sensor_id = NULL,
                               input_sensor_ports = NULL,
                               timeframe = "hour") {

  if (!timeframe %in% c("hour", "day", "week", "month")) {
    stop("Invalid timeframe: choose from 'hour', 'day', 'week', 'month'.")
  }

  interval <- switch(timeframe,
                     hour  = "1 hour",
                     day   = "1 day",
                     week  = "1 week",
                     month = "1 month")

  # All column name arguments are plain character strings — convert to syms
  date_sym <- rlang::sym(input_date)

  group_syms <- list()
  if (!is.null(input_site_id))    group_syms <- c(group_syms,    list(rlang::sym(input_site_id)))
  if (!is.null(input_sensor_id))  group_syms <- c(group_syms,    list(rlang::sym(input_sensor_id)))
  if (!is.null(input_sensor_ports)) group_syms <- c(group_syms,  list(rlang::sym(input_sensor_ports)))

  # Compute per-group min/max, then generate a sequence per row (rowwise),
  # then unnest — avoids seq() receiving a vector of dates when there are
  # multiple groups (which causes "to must be of length 1").
  x <- data %>%
    group_by(!!!group_syms) %>%
    summarise(
      min_date = min(!!date_sym, na.rm = TRUE),
      max_date = max(!!date_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(min_date) & !is.na(max_date) & is.finite(min_date) & is.finite(max_date)) %>%
    rowwise() %>%
    mutate(!!date_sym := list(seq(min_date, max_date, by = interval))) %>%
    ungroup() %>%
    select(-min_date, -max_date) %>%
    tidyr::unnest(cols = !!date_sym)

  return(x)
}
# make_complete_date <- function(data,
#                                input_date = "gen_date",
#                                input_site_id = NULL,       # Optional
#                                input_sensor_id = NULL,     # Optional
#                                input_sensor_ports = NULL,   # Optional
#                                timeframe = "hour") {
#
#   # Ensure that 'timeframe' is a valid unit accepted by lubridate's interval function
#   if (!timeframe %in% c("hour", "day", "week", "month")) {
#     stop("Invalid timeframe: choose from 'hour', 'day', 'week', 'month'.")
#   }
#
#   # Create the appropriate duration for seq()
#   interval <- switch(timeframe,
#                      hour = hours(1),
#                      day = days(1),
#                      week = weeks(1),
#                      month = months(1))
#
#   # Start the pipeline
#   x <- data
#
#   # Add grouping only for non-null inputs
#   group_vars <- quos()  # Create an empty quosure to dynamically add grouping variables
#
#   if (!is.null(input_site_id)) {
#     group_vars <- c(group_vars, quos({{ input_site_id }}))
#   }
#   if (!is.null(input_sensor_id)) {
#     group_vars <- c(group_vars, quos({{ input_sensor_id }}))
#   }
#   if (!is.null(input_sensor_ports)) {
#     group_vars <- c(group_vars, quos({{ input_sensor_ports }}))
#   }
#
#   # Group by selected variables and complete missing dates
#   x <- x %>%
#     group_by(!!!group_vars) %>%
#     # Check for date column and calculate minimum and maximum
#     summarise(
#       min_date = min({{ input_date }}, na.rm = TRUE),
#       max_date = max({{ input_date }}, na.rm = TRUE),
#       .groups = 'drop'  # Drop the groups after summarising to avoid NA groups
#     ) %>%
#     filter(!is.na(min_date) & !is.na(max_date)) %>% # Ensure valid min/max
#     # Create a complete date sequence
#     complete({{ input_date }} := seq(min_date, max_date, by = interval)) %>%
#     ungroup()
#
#   return(x)
# }
# make_complete_date <- function(data,
#                                input_date = "gen_date",
#                                input_site_id = NULL,       # Optional
#                                input_sensor_id = NULL,     # Optional
#                                input_sensor_ports = NULL,   # Optional
#                                timeframe = "day") {
#
#   # Ensure that 'timeframe' is a valid unit accepted by lubridate's interval function
#   if (!timeframe %in% c("hour", "day", "week", "month")) {
#     stop("Invalid timeframe: choose from 'hour', 'day', 'week', 'month'.")
#   }
#
#   # Create the appropriate duration for seq()
#   interval <- switch(timeframe,
#                      hour = hours(1),
#                      day = days(1),
#                      week = weeks(1),
#                      month = months(1))
#
#   # Start the pipeline
#   x <- data
#
#   # Add grouping only for non-null inputs
#   group_vars <- quos()  # Create an empty quosure to dynamically add grouping variables
#
#   if (!is.null(input_site_id)) {
#     group_vars <- c(group_vars, quos({{ input_site_id }}))
#   }
#   if (!is.null(input_sensor_id)) {
#     group_vars <- c(group_vars, quos({{ input_sensor_id }}))
#   }
#   if (!is.null(input_sensor_ports)) {
#     group_vars <- c(group_vars, quos({{ input_sensor_ports }}))
#   }
#
#   # Group by selected variables and complete missing dates
#   x <- x %>%
#     group_by(!!!group_vars) %>%
#     # Check if input_date column contains valid values before computing min and max
#     mutate(
#       min_date = min({{ input_date }}, na.rm = TRUE),
#       max_date = max({{ input_date }}, na.rm = TRUE)
#     ) %>%
#     filter(!is.na(min_date) & !is.na(max_date)) %>%
#     complete({{ input_date }} := seq(min_date, max_date, by = interval)) %>%
#     ungroup() %>%
#     select(-min_date, -max_date)  # Drop the temporary min_date and max_date columns
#
#   # # Group by selected variables and complete missing dates
#   # x <- x %>%
#   #   group_by(!!!group_vars) %>%  # Use unquoting to apply the group_vars
#   #   complete({{ input_date }} := seq(min({{ input_date }}),
#   #                                    max({{ input_date }}),
#   #                                    by = interval)) %>%
#   #   ungroup()
#
#   return(x)
# }
# make_complete_date <- function(data,
#                                input_date,
#                                input_site_id,
#                                input_sensor_id,
#                                input_sensor_ports,
#                                timeframe = "day") {
#
#   # Ensure that 'timeframe' is a valid unit accepted by lubridate's interval function
#   if (!timeframe %in% c("hour", "day", "week", "month")) {
#     stop("Invalid timeframe: choose from 'hour', 'day', 'week', 'month'.")
#   }
#
#   # Create the appropriate duration for seq()
#   interval <- switch(timeframe,
#                      hour = hours(1),
#                      day = days(1),
#                      week = weeks(1),
#                      month = months(1))
#
#   x <- data %>%
#     # Add grouping for different sites and sensors
#     group_by({{ input_site_id }}, {{ input_sensor_id }}, {{ input_sensor_ports }}) %>%
#     # Complete date variable to fill in missing time gaps
#     complete({{ input_date }} := seq(min({{ input_date }}), max({{ input_date }}), by = interval)) %>%
#     # Ungroup dataset prior to return
#     ungroup()
#
#   return(x)
# }
# make_complete_date <- function(data = data,
#                                input_date = gen_date,
#                                input_site_id = gen_site_ID,
#                                input_sensor_id = gen_sensor_ID,
#                                timeframe = c("hour", "day", "week", "month")) {
#   x <- data %>%
#     # Add grouping for different sites and sensors
#     group_by({{ input_site_id }}, {{ input_sensor_id }}) %>%
#     # Complete date variable to fill in missing time gaps
#     complete({{ input_date }} := seq(min({{ input_date }}), max({{ input_date }}), by = timeframe)) %>%
#     # Ungroup dataset prior to return
#     ungroup()
#
#   return(x)
# }

# make_complete_date <- function(data = data,
#                                input_date = gen_date,
#                                input_site_id = gen_site_ID,
#                                input_sensor_id = gen_sensor_ID,
#                                timeframe = c("hour", "day", "week", "month")) {
#   x <- data %>%
#     # Add grouping for different sites and sensors
#     group_by({{ site_ID }}, {{ sensor_ID }}) %>%
#     # Complete date variable to fill in missing timegaps
#     complete({{ input_date }} := seq(min({{ input_date }}), max({{ input_date }}), by = timeframe)) %>%
#     # Ungroup dataset prior to return
#     ungroup()
# }
