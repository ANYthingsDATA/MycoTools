#' #' Create Rolling Mean for MycoIndex (MIx_mold)
#' #'
#' #' This function calculates the rolling mean for the specified MycoIndex (MIx_mold)
#' #' variable in the given data frame. The rolling mean is calculated using a
#' #' specified rolling interval.
#' #'
#' #' @param data The data frame containing the MycoIndex data.
#' #' @param input The name of the MycoIndex variable (e.g., MIx_mold) to calculate
#' #'   the rolling mean for.
#' #' @param output_name The name of the new variable to store the rolling mean
#' #'   values. If not provided, a default name will be generated.
#' #' @param roll_interval The rolling interval (in hours) used to calculate the mean.
#' #'
#' #' @return The input data frame with an additional column containing the rolling
#' #'   mean values.
#' #'
#' #' @examples
#' #' # Calculate a rolling mean for MIx_mold with a 24-hour interval
#' #' result_data <- make_rolling_mix_mold(
#' #'   data = my_data, input = MIx_mold,
#' #'   output_name = "MIx_mold_rolling_mean",
#' #'   roll_interval = 24
#' #' )
#' #'
#' #' @seealso \code{\link{zoo::rollmean}} for additional details on rolling mean calculations.
#' #'
#' #' @importFrom dplyr mutate
#' #' @importFrom rlang enquo
#' #' @importFrom zoo rollmean
#' #'
#' #' @export
#' make_rolling_mix_mold <- function(data = data,
#'                                   input = MIx_mold,
#'                                   output_name,
#'                                   roll_interval = 24) {
#'   input <- enquo(input)
#'   if (missing(output_name)) {
#'     # If output_name is missing, create a new variable with a default name
#'     data %>%
#'       mutate("{{input}}_roll_{{roll_interval}}" := zoo::rollmean(
#'         x = {{ input }},
#'         k = roll_interval,
#'         na.pad = TRUE
#'       ))
#'   } else {
#'     # If output_name is provided, create or overwrite a variable with the specified name
#'     data %>%
#'       mutate({{ output_name }} := zoo::rollmean(
#'         x = {{ input }},
#'         k = roll_interval,
#'         na.pad = TRUE
#'       ))
#'   }
#' }
#'
#' #####
#'
#' make_rolling_mix_wood <- function(data = data,
#'                                   input = MIx_wood,
#'                                   output_name,
#'                                   roll_interval = 24) {
#'   input <- enquo(input)
#'   if (missing(output_name)) {
#'     data %>%
#'       mutate("{{input}}_roll_{{roll_interval}}" := zoo::rollmean(
#'         x = {{ input }},
#'         k = roll_interval,
#'         na.pad = TRUE
#'       ))
#'   } else {
#'     data %>%
#'       mutate({{ output_name }} := zoo::rollmean(
#'         x = {{ input }},
#'         k = roll_interval,
#'         na.pad = TRUE
#'       ))
#'   }
#' }
#'
#' #####
#'
#' #' Create Rolling Mean for MycoIndex Temperature (MIx_temp)
#' #'
#' #' This function calculates the rolling mean for the specified MycoIndex Temperature
#' #' (MIx_temp) variable in the given data frame. The rolling mean is calculated using a
#' #' specified rolling interval.
#' #'
#' #' @param data The data frame containing the MycoIndex Temperature data.
#' #' @param input The name of the MycoIndex Temperature variable (e.g., MIx_temp) to calculate
#' #'   the rolling mean for.
#' #' @param output_name The name of the new variable to store the rolling mean
#' #'   values. If not provided, a default name will be generated.
#' #' @param roll_interval The rolling interval (in hours) used to calculate the mean.
#' #'
#' #' @return The input data frame with an additional column containing the rolling
#' #'   mean values for MycoIndex Temperature.
#' #'
#' #' @examples
#' #' # Calculate a rolling mean for MIx_temp with a 24-hour interval
#' #' result_data <- make_rolling_mix_temp(
#' #'   data = my_data, input = MIx_temp,
#' #'   output_name = "MIx_temp_rolling_mean",
#' #'   roll_interval = 24
#' #' )
#' #'
#' #' @seealso \code{\link{zoo::rollmean}} for additional details on rolling mean calculations.
#' #'
#' #' @importFrom dplyr mutate
#' #' @importFrom rlang enquo
#' #' @importFrom zoo rollmean
#' #'
#' #' @export
# make_rolling_mix_temp <- function(data = data,
#                                   input = MIx_temp,
#                                   output_name,
#                                   roll_interval = 24) {
#   input <- enquo(input)
#   if (missing(output_name)) {
#     data %>%
#       mutate("{{input}}_roll_{{roll_interval}}" := zoo::rollmean(
#         x = {{ input }},
#         k = roll_interval,
#         na.pad = TRUE
#       ))
#   } else {
#     data %>%
#       mutate({{ output_name }} := zoo::rollmean(
#         x = {{ input }},
#         k = roll_interval,
#         na.pad = TRUE
#       ))
#   }
# }


#' Rolling mean for MIx_mold (time-aware)
#' @importFrom rlang ensym as_name
#' @export
make_rolling_mix_mold <- function(data,
                                  input = MIx_mold,
                                  index = gen_datetime,
                                  id = NULL,
                                  output_name,
                                  roll_interval = 24,
                                  align = "right",
                                  na_rm = TRUE,
                                  complete = TRUE) {
  input_q <- rlang::enquo(input)
  make_rolling_time_mean(
    data = data,
    input = !!input_q,
    index = {{ index }},
    id    = {{ id }},
    output_name = output_name %||% paste0(rlang::as_name(input_q), "_roll_", roll_interval, "h"),
    window_hours = roll_interval,
    align = align,
    na_rm = na_rm,
    complete = complete
  )
}

#' Rolling mean for MIx_wood (time-aware)
#' @export
make_rolling_mix_wood <- function(data,
                                  input = MIx_wood,
                                  index = gen_datetime,
                                  id = NULL,
                                  output_name,
                                  roll_interval = 24,
                                  align = "right",
                                  na_rm = TRUE,
                                  complete = TRUE) {
  input_q <- rlang::enquo(input)
  make_rolling_time_mean(
    data, !!input_q, {{ index }}, {{ id }},
    output_name %||% paste0(rlang::as_name(input_q), "_roll_", roll_interval, "h"),
    window_hours = roll_interval, align = align, na_rm = na_rm, complete = complete
  )
}

#' Rolling mean for MIx_temp (time-aware)
#' @export
make_rolling_mix_temp <- function(data,
                                  input = MIx_temp,
                                  index = gen_datetime,
                                  id = NULL,
                                  output_name,
                                  roll_interval = 24,
                                  align = "right",
                                  na_rm = TRUE,
                                  complete = TRUE) {
  input_q <- rlang::enquo(input)
  make_rolling_time_mean(
    data, !!input_q, {{ index }}, {{ id }},
    output_name %||% paste0(rlang::as_name(input_q), "_roll_", roll_interval, "h"),
    window_hours = roll_interval, align = align, na_rm = na_rm, complete = complete
  )
}
