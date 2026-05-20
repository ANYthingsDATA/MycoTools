#' Create Mold Index Variable
#'
#' This function takes a data frame and creates a mold index variable based on the provided
#' relative humidity measurements, using user-defined thresholds for different mold levels.
#'
#' @param data The input data frame containing the relative humidity measurements.
#' @param input_mold The name or expression of the relative humidity measurement variable.
#' @param output_index The name of the new mold index variable to be created.
#' @param mold_low The threshold for the lower mold level (e.g., 75).
#' @param mold_mid The threshold for the mid-range mold level (e.g., 85).
#' @param mold_high The threshold for the high mold level (e.g., 95).
#'
#' @import dplyr
#'
#' @return A modified data frame with the new mold index variable added.
#'
#' @examples
#' # Example usage:
#' data_with_mold_index <- make_mycoindex_mold(
#'   data = my_data, input_mold = MoldMeasurement,
#'   output_index = MoldIndex, mold_low = 75,
#'   mold_mid = 85, mold_high = 95
#' )
#'
#' @export
make_mycoindex_mold <- function(data = data,
                                input_mold = gen_rhum,
                                output_index = MIx_mold,
                                mold_low = 75,
                                mold_mid = 85,
                                mold_high = 95) {
  x <- dplyr::mutate(
    data,
    {{ output_index }} := dplyr::if_else(
      {{ input_mold }} < mold_low, 0, dplyr::if_else(
        {{ input_mold }} >= mold_low & {{ input_mold }} < mold_mid, 0.25, dplyr::if_else(
          {{ input_mold }} >= mold_mid & {{ input_mold }} < mold_high, 0.5, dplyr::if_else(
            {{ input_mold }} >= mold_high, 1, 999
          )
        )
      )
    )
  )

  return(x)
}


# make_mycoindex_mold <- function(data = data,
#                                 input_mold = gen_rhum,
#                                 output_index = MIx_mold,
#                                 mold_low = 75,
#                                 mold_mid = 85,
#                                 mold_high = 95) {
#   x <- dplyr::mutate(data, {{ output_index }} := dplyr::if_else(
#     {{ input_mold }} < mold_low, 0, dplyr::if_else(
#       {{ input_mold }} >= mold_low & {{ input_mold }} < mold_mid, 0.25, dplyr::if_else(
#         {{ input_mold }} >= mold_mid & {{ input_mold }} < mold_high, 0.5, dplyr::if_else(
#           {{ input_mold }} >= mold_high, 1,
#         )
#       )
#     )
#   ))
# }
