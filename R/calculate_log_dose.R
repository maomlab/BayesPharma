#' Add a log dose (in base 10) column to the input data.frame
#'
#' @description Given the dose as a column in a data.frame with a given molar concentration
#'    add a new column of the log base-10 dose, `log_dose`, in the data.frame and return it.
#'
#' @usage
#'   data %>% calculate_log_dose(
#'     dose_col = <dose_col>,
#'     molar_concentration = <molar_concentration>)
#'
#' @param data data.frame containing a column representing a dose in molar units
#' @param dose_col expression for dose column in the input data.frame (default = dose)
#' @param molar_concentration numeric units of molar concentration (default = 1)
#' @return input data.frame with an additional `log_dose` column
#'
#' @examples
#' # Consider observations at doses of `1 μM` and `0.1 μM`.
#' # If the doses are given in molar units e.g. `M` then,
#' data <- data.frame(dose = c(1e-6, 1e-7)) %>%
#'    BayesPharma::calculate_log_dose(dose_col = dose, molar_concentration = 1)
#' data$log_dose == c(-6, -7)
#'
#' # If the doses are given with in units of nanomolar units e.g. `nM` then
#' data <- data.frame(dose_nM = c(1000, 100)) %>%
#'   BayesPharma::calculate_log_dose(dose_col = dose_nM, molar_concentration = 1e-9)
#' data$log_dose == c(-6, -7)
#'
#'@export
calculate_log_dose <- function(
    data,
    dose_col = dose,
    molar_concentration = 1){

  if ("log_dose" %in% names(data.frame)) {
    warning("Calculating log_dose but a log_dose column already exists, overwriting.")
  }

  data %>% dplyr::mutate(log_dose = log10({{dose_col}} * molar_concentration))

}

