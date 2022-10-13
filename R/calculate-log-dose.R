#' Add a log dose (in base 10) column to the input data.frame
#'
#' @description Given the dose as a column in a data.frame with a given molar
#'   concentration add a new column of the log base-10 dose, \code{log_dose}, in
#'   the \code{data.frame} and return it.
#' @param data data.frame containing a column representing a dose in molar
#'   units.
#' @param dose_col expression for dose column in the input data.frame
#' @param molar_concentration numeric units of molar concentration of the dose
#'   column (default = 1).
#' @return input \code{data.frame} with an additional \code{log_dose} column.
#'
#' @examples
#' \dontrun{
#' # Consider observations at doses of \code{1 μM} and \code{0.1 μM}.
#' # If the doses are given in molar units e.g. \code{M} then,
#' data <- data.frame(dose = c(1e-6, 1e-7)) |>
#'    BayesPharma::calculate_log_dose(dose_col = dose, molar_concentration = 1)
#' data$log_dose == c(-6, -7)
#'
#' # If the doses are given with in units of nanomolar units e.g. \code{nM} then
#' data <- data.frame(dose_nM = c(1000, 100)) |>
#'   BayesPharma::calculate_log_dose(
#'     dose_col = dose_nM,
#'     molar_concentration = 1e-9)
#' data$log_dose == c(-6, -7)
#'}
#'@export
calculate_log_dose <- function(
    data,
    dose_col = "dose",
    molar_concentration = 1) {

  if ("log_dose" %in% names(data)) {
    warning(
      "Calculating log_dose but a log_dose column already exists, overwriting.")
  }

  if (rlang::is_symbol(rlang::expr(dose_col))) {
    dose_col <- tidyselect::eval_select(rlang::enquo(dose_col), data)
  } else if (is.character(dose_col)) {
    if (!(dose_col %in% names(data))) {
      stop("dose_col: '", dose_col, "' is not a column of data\n")
    }
  } else if (is.numeric(dose_col)) {
    if (dose_col < 1 || dose_col > ncol(data)) {
      stop(
        "dose_col indexes column ", dose_col, ", but data only has ",
        ncol(data), " column\n")
    }
  } else {
    stop(paste0("Unrecognized class for dose_col ", class(dose_col), "\n"))
  }

  data$log_dose <- log10(data[, dose_col] * molar_concentration)
  data
}
