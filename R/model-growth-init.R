#' Create initialization for the sigmoid growth model
#'
#' @description Creating initial values for sigmoid growth model
#'   parameters that can be passed to the
#'.  \code{richards_growth_model}.
#'
#' @param K \code{numeric} or \code{numeric} returning \code{function}
#'   (Default: 1).
#' @param K0 \code{numeric} or \code{numeric} returning \code{function}
#'   (Default: 0).
#' @param rate \code{numeric} or \code{numeric} returning \code{function}
#'   (Default: 1).
#' @param lambda \code{numeric} or \code{numeric} returning \code{function} 
#'   units of \code{time} (Default: 0.5).
#'
#' @returns input for brms::brm(init = ...)
#'
#' @examples
#'\dontrun{
#' init <- BayesPharma::growth_sigmoid_init(
#'   A = 100)
#'}
#'@export
growth_sigmoid_init <- function(
    K = 1,
    K0 = 0,
    rate = 1,
    lambda = 0.5) {
  
  function() {
    list(
      b_K = prepare_init(K),
      b_K0 = prepare_init(K0),
      b_rate = prepare_init(rate),
      b_lambda = prepare_init(lambda))
  }
}



#' Create initialization for the Richards growth model
#'
#' @description Creating initial values for Richards growth model
#'   parameters that can be passed to the
#'.  \code{richards_growth_model}.
#'
#' @param K \code{numeric} or \code{numeric} returning \code{function}
#'   (Default: 1).
#' @param K0 \code{numeric} or \code{numeric} returning \code{function}
#'   (Default: 0).
#' @param rate \code{numeric} or \code{numeric} returning \code{function}
#'   (Default: 1).
#' @param lambda \code{numeric} or \code{numeric} returning \code{function} 
#'   units of \code{time} (Default: 0.5).
#' @param nu \code{numeric} or \code{numeric} returning \code{function}
#'   values range between [0, Inf], with 1 being standard sigmoid (default = 1).
#'
#' @returns input for brms::brm(init = ...)
#'
#' @examples
#'\dontrun{
#' init <- BayesPharma::growth_richards_init(
#'   A = 100,
#'   nu = 2)
#'}
#'@export
growth_richards_init <- function(
    K = 1,
    K0 = 0,
    rate = 1,
    lambda = 0.5,
    nu = 1) {

  function() {
    list(
      b_K = prepare_init(K),
      b_K0 = prepare_init(K0),
      b_rate = prepare_init(rate),
      b_lambda = prepare_init(lambda),
      b_nu = prepare_init(nu))
  }
}
