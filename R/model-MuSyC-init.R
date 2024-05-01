#' Create Initial Parameter Values for the MuSyC Model
#'
#' @description Initial values for the MuSyC model
#'
#' @param logE0 `numeric` or function returning `array` of length `1`. Initial
#'     value for the `logE0` parameter. Default: `log(0.5)`
#' @param logE1 `numeric` or function returning array of length
#'     1. Initial value for the `logE1` parameter. Default:
#'     log(0.5)
#' @param logC1 `numeric` or function returning array of length
#'     1. Initial value for the `logC1` parameter. Default:
#'     `0 = log(1)`
#' @param h1 `numeric` or function returning array of length
#'     1. Initial value for the `h1` parameter. Default: `4
#'     = MuSyC_si_to_hi(si=1, Ci=1, E0=1, Ei=0.0)` the exponent for
#'     treatment `1` corresponding to a slope of `1`
#' @param logE2 `numeric` or function returning array of length
#'     `1`. Initial value for the `logE2` parameter. Default:
#'     `log(0.5)`
#' @param logC2 `numeric` or function returning array of length
#'     `1`. Initial value for the `logC2`
#'     parameter. Default: `0 = log(1)`
#' @param h2 `numeric` or function returning array of length
#'     `1`. Initial value for the `h2` parameter. Default:
#'     `4 = MuSyC_si_to_hi(si=1, Ci=1, E0=1, Ei=0.0)` the
#'     exponent for treatment `1` corresponding to a slope of
#'     `1`
#' @param logE3 `numeric` or function returning array of length
#'     1. Initial value for the `logE3` parameter. Default:
#'     `log(0.5)`
#' @param logalpha `numeric` or function returning array of
#'     length 1. Initial value for the `logalpha`
#'     parameter. Default: `0 = log(1)`
#' @param ... additional parameter initialization. Each named argument should be
#'   a function that returns a `numeric` or `array` depending on the dimension
#'   of the parameter, see [rstan_default_init()] to use the \pkg{rstan} default
#'   init.
#'
#' @returns input for `BayesPharma::model_MuSyC(init = ...)`
#'     parameter.
#'
#' @examples
#'\dontrun{
#'   # default prior and init for MuSyC Model
#'   prior <- BayesPharma::MuSyC_prior()
#'   init <- BayesPharma::MuSyC_init()
#'
#'   # Set prior and init for MuSyC model where both treatment 1 and treatment 2
#'   # are assumed to be full inhibitors
#'   BayesPharma::MuSyC_prior(logE0 = 0, E1 = -Inf, E2 = -Inf, E3 = -Inf)
#'   BayesPharma::MuSyC_init(E0 = 1, E1 = -Inf, E2 = -Inf, E3 = -Inf)
#'}
#'@export
MuSyC_init <- function(
  logE0 = \() runif(n = 1, min = log(0.4), max = log(0.6)),
  logE1 = \() runif(n = 1, min = log(0.4), max = log(0.6)),
  logC1 = \() runif(n = 1, min = -0.2, max = 0.2),
  h1 = \() runif(n = 1, min = 3, max = 5),
  logE2 = \() runif(n = 1, min = log(0.4), max = log(0.6)),
  logC2 = \() runif(n = 1, min = -0.2, max = 0.2),
  h2 = \() runif(n = 1, min = 3, max = 5),
  logE3 = \() runif(n = 1, min = log(0.4), max = log(0.6)),
  logalpha = \() runif(n = 1, min = -0.2, max = 0.2),
  ...) {

  init <- list(
    b_logE0 = logE0,
    b_logE1 = logE1,
    b_logC1 = logC1,
    b_h1 = h1,
    b_logE2 = logE1,
    b_logC2 = logC1,
    b_h2 = h2,
    b_logE3 = logE3,
    b_logalpha = logalpha,
    ...) |>
    purrr::compact()
  class(init) <- c("bpinit", class(init))
  init
}
