#' Change column names to be compatible with BayesPharma functions
#'
#' @description Given the response, log_dose, and predictors column of a
#'   data.frame, change the column names to `response` and `log_dose` in the
#'   data.frame and return it.
#'
#' @param data data.frame.
#' @param response_col_name expression for response column in the input
#' data.frame (default = NULL).
#' @param log_dose_col_name expression for log_dose column in the input
#'   data.frame (default = NULL). if log_dose hasn't been calculated yet,
#'   calculate log_dose, use the \code{calculate_log_dose} function.
#' @return input data.frame with new column header names for the response
#'    column (`response`) and log_dose column (`log_dose`).
#'
#' @examples
#'\dontrun{
#' # consider the response column name is `normalized_measurement` and log dose
#' # column name is `log_10_dose`.
#' data %>% BayesPharma::change_col_names(
#'   response_col_name = normalized_measurement,
#'   log_dose_col_name = log_10_dose)
#'}
#'@export

change_col_names <- function(data,
                             response_col_name = NULL,
                             log_dose_col_name = NULL) {
  data %>%
    dplyr::rename(response = {{response_col_name}}) %>%
    dplyr::rename(log_dose = {{log_dose_col_name}})

}
