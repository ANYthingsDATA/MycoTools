#' Create Temperature Index Variable
#'
#' This function takes a data frame and creates a temperature index variable based on the provided
#' temperature measurements, using user-defined thresholds for different temperature levels.
#'
#' @param data The input data frame containing the temperature measurements.
#' @param input_temp The name or expression of the temperature measurement variable.
#' @param output_index The name of the new temperature index variable to be created.
#' @param temp_low The threshold for the low temperature level (e.g., 4).
#' @param temp_mid The threshold for the mid-range temperature level (e.g., 8).
#' @param temp_high The threshold for the high temperature level (e.g., 14).
#' @param temp_max The maximum temperature threshold (e.g., 35).
#'
#' @import dplyr
#'
#' @return A modified data frame with the new temperature index variable added.
#'
#' @examples
#' # Example usage:
#' data_with_temp_index <- make_mycoindex_temp(
#'   data = my_data, input_temp = Temperature,
#'   output_index = TempIndex, temp_low = 4,
#'   temp_mid = 8, temp_high = 14, temp_max = 35
#' )
#'
#' @export
make_mycoindex_temp <- function(data = data,
                                input_temp = gen_temp,
                                output_index = MIx_temp,
                                temp_low = 4,
                                temp_mid = 8,
                                temp_high = 14,
                                temp_max = 35) {
  x <- dplyr::mutate(
    data,
    {{ output_index }} := dplyr::if_else(
      {{ input_temp }} < temp_low, 0, dplyr::if_else(
        {{ input_temp }} >= temp_low & {{ input_temp }} < temp_mid, 0.2, dplyr::if_else(
          {{ input_temp }} >= temp_mid & {{ input_temp }} < temp_high, 0.4, dplyr::if_else(
            {{ input_temp }} >= temp_high & {{ input_temp }} < temp_max, 1, dplyr::if_else(
              {{ input_temp }} >= temp_max, 0, 999
            )
          )
        )
      )
    )
  )

  return(x)
}


# make_mycoindex_temp <- function(data = data,
#                                 input_temp = gen_temp,
#                                 output_index = MIx_temp,
#                                 temp_low = 4,
#                                 temp_mid = 8,
#                                 temp_high = 14,
#                                 temp_max = 35) {
#   x <- dplyr::mutate(data, {{ output_index }} := dplyr::if_else(
#     {{ input_temp }} < temp_low, 0, dplyr::if_else(
#       {{ input_temp }} >= temp_low & {{ input_temp }} < temp_mid, 0.2, dplyr::if_else(
#         {{ input_temp }} >= temp_mid & {{ input_temp }} < temp_high, 0.4, dplyr::if_else(
#           {{ input_temp }} >= temp_high & {{ input_temp }} < temp_max, 1, dplyr::if_else(
#             {{ input_temp }} >= temp_max, 0, NULL
#           )
#         )
#       )
#     )
#   ))
# }
