#' Create initialization for the MuSyC model
#'
#' @description Initial values for the MuSyC model
#'
#' @param logE0 \code{numeric} or function returning \code{array} of
#'     length \code{1}.  Initial value for the \code{logE0}
#'     parameter. Default: \code{log(0.5)}
#' @param logC1 \code{numeric} or function returning array of length
#'     1. Initial value for the \code{logC1} parameter. Default:
#'     \code{0 = log(1)}
#' @param logE1 \code{numeric} or function returning array of length
#'     1. Initial value for the \code{logE1} parameter. Default:
#'     log(0.5)
#' @param h1 \code{numeric} or function returning array of length
#'     1. Initial value for the \code{h1} parameter. Default: \code{4
#'     = MuSyC_si_to_hi(si=1, Ci=1, E0=1, Ei=0.0)} the exponent for
#'     treatment \code{1} corresponding to a slope of \code{1}
#' @param logC2 \code{numeric} or function returning array of length
#'     \code{1}. Initial value for the \code{logC2}
#'     parameter. Default: \code{0 = log(1)}
#' @param logE2 \code{numeric} or function returning array of length
#'     \code{1}. Initial value for the \code{logE2} parameter. Default:
#'     \code{log(0.5)}
#' @param h2 \code{numeric} or function returning array of length
#'     \code{1}. Initial value for the \code{h2} parameter. Default:
#'     \code{4 = MuSyC_si_to_hi(si=1, Ci=1, E0=1, Ei=0.0)} the
#'     exponent for treatment \code{1} corresponding to a slope of
#'     \code{1}
#' @param logE3 \code{numeric} or function returning array of length
#'     1. Initial value for the \code{logE3} parameter. Default:
#'     \code{log(0.5)}
#' @param logalpha \code{numeric} or function returning array of
#'     length 1. Initial value for the \code{logalpha}
#'     parameter. Default: \code{0 = log(1)}
#' @return input for \code{BayesPharma::model_MuSyC(init = ...)}
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
      b_logalpha = prepare_init(logalpha))
  }
}
