#' Wrapper for DRC Dose Response Models
#'
#' @param data `data.frame`
#' @param formula `formula` formula relating the `response` to the `log_dose`,
#'  if the [drc::L.4] sigmoid link function is used or `dose` if the [drc::LL.4]
#'  log-linear sigmoid link functions used with the `fct` argument.
#' @param fct `Boltzmann` non-linear link function. Default is the 4 parameter
#'     log-linear sigmoid equation, [drc::L.4], where all parameters are to be
#'     estimated. To fix a subset them, set the parameters to constant
#'     values.
#' @param ... additional arguments passed to [drc::drm]
#'
#' @returns An object of (S3) class `drc` from the \pkg{drc} package.
#'
#' @examples
#' \dontrun{
#' BayesPharma::drm_model(
#'   data = tibble::tibble(
#'     log_dose = seq(-7, -5, length.out = 20),
#'     mean_response = BayesPharma::sigmoid(
#'       ac50 = -6, hill = -1, top = 1, bottom = 0, log_dose = log_dose),
#'     response = stats::rnorm(n = 20, mean = 0, sd = 0.2))
#' }
#'
#' @export
drc_model <- function(
    data,
    formula = response ~ log_dose,
    fct = drc::L.4(
      fixed = c(NA, NA, NA, NA),
      names = c("hill", "bottom", "top", "ec50")),
    ...) {

    drc::drm(
      formula = formula,
      data = data,
      fct = fct,
      ...)
}
