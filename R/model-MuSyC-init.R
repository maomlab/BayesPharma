#' Create initialization for the MuSyC model
#'
#' @description Initial values for the MuSyC model
#'
#' @param logE0 numeric or function returning array of length 1. Initial value
#'   for the `logE0` parameter. Default: `log(0.5)`
#' @param logC1 numeric or function returning array of length 1. Initial value
#'   for the `logC1` parameter. Default: `0 = log(1)``
#' @param logE1 numeric or function returning array of length 1. Initial value
#'   for the `logE1` parameter. Default: log(0.5)
#' @param h1 numeric or function returning array of length 1. Initial value
#'   for the `h1` parameter. Default: `4 =
#'   MuSyC_si_to_hi(si=1, Ci=1, E0=1, Ei=0.0)` the exponent for treatment `1``
#'   corresponding to a slope of `1`
#' @param logC2 numeric or function returning array of length 1. Initial value
#'   for the `logC2` parameter. Default: `0 = log(1)`
#' @param logE2 numeric or function returning array of length 1. Initial value
#'   for the `logE2` parameter. Default: `log(0.5)`
#' @param h2 numeric or function returning array of length 1. Initial value
#'   for the `h2` parameter. Default: `4 =
#'   MuSyC_si_to_hi(si=1, Ci=1, E0=1, Ei=0.0)` the exponent for treatment `1``
#'   corresponding to a slope of 1
#' @param logE3 numeric or function returning array of length 1. Initial value
#'   for the `logE3` parameter. Default: `log(0.5)`
#' @param logalpha numeric or function returning array of length 1. Initial
#'   value for the `logalpha` parameter. Default: `0 =  log(1)`
#' @return input for `BayesPharma::model_MuSyC(init = ...)` parameter.
#'
#' @examples
#'\dontrun{
#'}
#'@export
#'
#'
MuSyC_init <- function(
    logE0 = log(0.5),
    logC1 = 0,
    logE1 = log(0.5),
    h1 = 4,
    logC2 = 0,
    logE2 = log(0.5),
    h2 = 4,
    logE3 = log(0.5),
    logalpha = 0) {

  function() {
    list(
      b_logE0 = prepare_init(logE0),
      b_logC1 = prepare_init(logC1),
      b_logE1 = prepare_init(logE1),
      b_h1 = prepare_init(h1),
      b_logC2 = prepare_init(logC1),
      b_logE2 = prepare_init(logE1),
      b_h2 = prepare_init(h1),
      b_logE3 = prepare_init(logE3),
      b_logalpha = prepare_init(logalpha))}
}  
  