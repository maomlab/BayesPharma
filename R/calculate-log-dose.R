#' Add a Log(dose) (in base 10) Column to the Input `data.frame`
#'
#' @description Given the dose as a column in a `data.frame` with a given
#'   molar concentration add a new column of the log base-10 dose,
#'   `log_dose`, in the `data.frame` and return it.
#'
#' @param data `data.frame` containing a column representing a dose in
#'   molar units.
#' @param dose_col `expression` for dose column in the input
#'   `data.frame` (Default: `"dose"`)
#' @param molar_concentration `numeric` units of molar concentration of the
#'   dose column (Default: 1).
#'
#' @returns input `data.frame` with an appended `log_dose` column
#'   which is the `log10(<dose_col>) * molar_concentration`.
#'
#' @examples
#' \dontrun{
#' # Consider observations at doses of 1 μM and 0.1 μM.
#' # If the doses are given in molar units (M) then
#' data <- data.frame(dose = c(1e-6, 1e-7)) |>
#'    BayesPharma::calculate_log_dose(dose_col = dose, molar_concentration = 1)
#' data$log_dose == c(-6, -7)
#'
#' # If the doses are given with in units of nanomolar (nM) then
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
