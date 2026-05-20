#' Create Wood Moisture Index Variable
#'
#' This function takes a data frame and creates a wood moisture index variable based on the provided
#' wood moisture measurements, using user-defined thresholds for different wood moisture levels.
#'
#' @param data The input data frame containing the wood moisture measurements.
#' @param input_wood The name or expression of the wood moisture measurement variable.
#' @param output_index The name of the new wood moisture index variable to be created.
#' @param wood_low The threshold for the low wood moisture level (e.g., 20).
#' @param wood_mid The threshold for the mid-range wood moisture level (e.g., 25).
#' @param wood_high The threshold for the high wood moisture level (e.g., 30).
#' @param wood_max The maximum wood moisture threshold (e.g., 100).
#'
#' @import dplyr
#'
#' @return A modified data frame with the new wood moisture index variable added.
#'
#' @examples
#' # Example usage:
#' data_with_wood_index <- make_mycoindex_wood(
#'   data = my_data, input_wood = WoodMoisture,
#'   output_index = WoodMoistureIndex, wood_low = 20,
#'   wood_mid = 25, wood_high = 30, wood_max = 100
#' )
#'
#' @export
# This function is called "make_mycoindex_wood" and it takes several arguments.
# The arguments are defined with default values, so if they are not provided, the function will use the given default values.

# The first argument "data" is a data frame.
# The default value for this argument is a variable called "data" that is assumed to be available in the global environment.

# The second argument "input_wood" is a character string representing the column name containing wood data in the "data" data frame.
# The default value for this argument is a character string "gen_wood".

# The third argument "output_index" is a character string representing the column name in which the calculated index values will be stored.
# The default value for this argument is a character string "MIx_wood".

# The next four arguments "wood_low", "wood_mid", "wood_high", "wood_max" are numeric values representing thresholds for wood categories.
# The default values for these arguments are 20, 25, 30, and 100 respectively.

# Inside the main body of the function:

# A new variable "x" is created by mutating the "data" data frame using dplyr::mutate() function.
# The new variable is named with the value of "output_index". The double curly braces ({{}}) syntax is used to refer to the argument value dynamically.

# The value of the new variable "x" is calculated using nested if_else() statements.
# These statements check various conditions on the "input_wood" column using the provided thresholds.
# If a condition is met, a specific value is assigned to the new variable "x".
# If none of the conditions are met, a value of 999 is assigned to "x".

# Finally, the mutated data frame "x" is returned by the function.

####

# # This is a function definition named "make_mycoindex_wood"
# # It takes four arguments:
# #   - data (which defaults to a variable named "data")
# #   - input_wood (which defaults to a variable named "gen_wood")
# #   - output_index (which defaults to a variable named "MIx_wood")
# #   - wood_low (which defaults to the value 20)
# #   - wood_mid (which defaults to the value 25)
# #   - wood_high (which defaults to the value 30)
# #   - wood_max (which defaults to the value 100)
make_mycoindex_wood <- function(data = data,
                                input_wood = gen_wood,
                                output_index = MIx_wood,
                                wood_low = 20,
                                wood_mid = 25,
                                wood_high = 30,
                                wood_max = 100) {
  x <- dplyr::mutate(data,
                     {{ output_index }} := dplyr::if_else(
                       {{ input_wood }} < wood_low, 0, dplyr::if_else(
                         {{ input_wood }} >= wood_low & {{ input_wood }} < wood_mid, 0.25, dplyr::if_else(
                           {{ input_wood }} >= wood_mid & {{ input_wood }} < wood_high, 0.5, dplyr::if_else(
                             {{ input_wood }} >= wood_high & {{ input_wood }} < wood_max, 1, dplyr::if_else(
                               {{ input_wood }} >= wood_max, 0, 999
                             )
                           )
                         )
                       )
                     )
  )

  # Return the modified dataframe "x"
  return(x)
}

####

# make_mycoindex_wood <- function(data = data,
#                                 input_wood = gen_wood,
#                                 output_index = MIx_wood,
#                                 wood_low = 20,
#                                 wood_mid = 25,
#                                 wood_high = 30,
#                                 wood_max = 100) {
#   x <- dplyr::mutate(data, {{ output_index }} := dplyr::if_else(
#     {{ input_wood }} < wood_low, 0, dplyr::if_else(
#       {{ input_wood }} >= wood_low & {{ input_wood }} < wood_mid, 0.25, dplyr::if_else(
#         {{ input_wood }} >= wood_mid & {{ input_wood }} < wood_high, 0.5, dplyr::if_else(
#           {{ input_wood }} >= wood_high & {{ input_wood }} < wood_max, 1, dplyr::if_else(
#             {{ input_wood }} >= wood_max, 0, NULL
#           )
#         )
#       )
#     )
#   ))
# }
