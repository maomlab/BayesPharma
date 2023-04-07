#' Sigmoid Function
#'
#' Functional form for the sigmoid model.
#'
#' @param ac50 numeric, the log_dose of half maximal activity
#' @param hill numeric, slope of curve at the ac50 units of
#'     response/log_dose
#' @param top numeric, value of maximal response for positive slope,
#'     this occurs when log_dose = Inf, with negative slope when
#'     log_dose = -Inf
#' @param bottom numeric, value of minimal response for positive
#'     slope, this occurs when log_dose = -Inf, with negative slope
#'     when log_dose = Inf
#' @param log_dose numeric, where to evaluate the the response where
#'     the log is base 10.
#' @returns numeric, response given the log_dose and parameters
#'
#' @examples
#' \dontrun{
#'  # Generate an agonist curve with an ec50 of 1 μM with the response
#'  # normalized to [0, 1] over the range 100 nM to 10 μM with half-log steps
#'  data <- data.frame(
#'    log_dose = seq(-7, -5, length.out = 5)) |>
#'      dplyr::mutate(
#'        response = stats::rnorm(
#'          n = length(log_dose),
#'          mean = BayesPharma::sigmoid(
#'            ac50 = -6,
#'            hill = 1,
#'            top = 1,
#'            bottom = 0,
#'            log_dose = log_dose),
#'        sd = .2))
#' }
#'
#' @export
sigmoid <- Vectorize(
  function(ac50, hill, top, bottom, log_dose) {
    bottom + (top - bottom) / (1 + 10 ^ ((ac50 - log_dose) * hill))
})
