#' Wrapper for DRC dose response models
#'
#' @param data `data.frame`
#' @param formula `formula` formulate relating the `response` to the `log_dose`,
#'   if the [drc::L.4] or `dose` if the [drc::LL.4] non-linear link functions
#'   are used with the `fct` argument.
#' @param fct Non-linear link function. Default is the 4 parameter
#'     log-linear sigmoid equation, [drc::L.4], where all parameters are to be
#'     estimated. To fix a subset them, set the parameters to constant
#'     values.
#' @param ... additional arguments passed to [drc::drm]
#'
#' @returns An object of (S3) class `drc` from the [drc] package.
#'
#' \examples
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
