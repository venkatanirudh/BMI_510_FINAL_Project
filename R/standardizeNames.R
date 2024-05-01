#' Standardize Variable Names
#'
#' This function standardizes the names of variables in a data frame data to a specified case.
#' Uses janitor::clean_names and dplyr::rename_with to convert names.
#'
#' @importFrom snakecase to_lower_camel_case
#' @importFrom janitor make_clean_names
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @param data A data frame whose variable names are to be standardized.
#' @return A data frame with standardized variable names.
#' @examples
#' data <- tibble::tibble('Variable Name' = 1:5, 'Another_Variable' = 6:10)
#' data_clean <- standardizeNames(data)
#' @export
standardizeNames <- function(data, case = "small_camel") {

  data %>%
    janitor::clean_names(case = "snake") %>%
    dplyr::rename_with(~ snakecase::to_any_case(.x, case = case))
}
