#' Wrapper for DRC dose response models
#'
#' @param data \code{data.frame}
#' @param formula \code{formula}. Default: \code{response ~ log_dose}
#' @param fct Non-linear link function. Default is the 4 parameter
#'     log-linear sigmoid equation, where all parameters are to be
#'     estimated. To fix a subset them, set the parameters to constant
#'     values.
#' @param ... additional arguments passed to \code{drc::drm}
#'
#' @returns An object of (S3) class 'drc'.
#'
#' @export
drc_model <- function(
    data,
    formula = response ~ log_dose,
    fct = drc::L.4(
      fixed = c(NA, NA, NA, NA),
      names = c("hill", "bottom", "top", "ic50")),
    ...) {

    drc::drm(
      formula = formula,
      data = data,
      fct = fct,
      ...)
}
