#' Change column names to work with BayesPharma functions
#'
#'@param response_col_name string. Name of current column name for response.
#'if column name is 'response' then use NULL.
#'@param log_dose_col_name string. Name of current column name for log_dose.
#'if column name is 'log_dose' or log_dose hasn't been calculated yet, then use NULL.
#'to calculate log_dose, use the calc log_dose function.
#'@param predictors_col_name string. Name of current column name for perturbation
#'name (i.e. drug, temperature, etc.). if column name is 'predictors' or there
#'is only one perturbation, then use NULL.
#'@return tibble or data.frame.
#'
#'@export

change_col_names <- function(data,
                             response_col_name,
                             log_dose_col_name,
                             predictors_col_name) {
  data %>%
    dplyr::rename(response = response_col_name) %>%
    dplyr::rename(log_dose = log_dose_col_name) %>%
    dplyr::rename(predictors = predictors_col_name)
}
