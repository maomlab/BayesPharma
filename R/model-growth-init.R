#' Create Initialization for the Sigmoid Growth Model
#'
#' @description Create initial values for sigmoid growth model parameters that
#'   can be passed to the [growth_sigmoid_model].
#'
#' @param K `numeric` or `numeric` returning `function`
#' @param K0 `numeric` or `numeric` returning `function`
#' @param rate `numeric` or `numeric` returning `function`
#' @param lambda `numeric` or `numeric` returning `function`
#'   units of `time`
#' @param ... additional parameter initialization. Each named argument should be
#'   a function that returns a `numeric` or `array` depending on the dimension
#'   of the parameter, see [rstan_default_init()] to use the \pkg{rstan} default
#'   init.
#'
#' @returns input for `[brm][brms::brm](init = ...)`
#'
#' @seealso [growth_sigmoid_formula], [growth_sigmoid_prior], and
#'   [growth_sigmoid_model]
#'
#' @examples
#'\dontrun{
#' init <- BayesPharma::growth_sigmoid_init(
#'   A = 100)
#'}
#'@export
growth_sigmoid_init <- function(
  K = \() runif(n = 1, min = 0.8, max = 1.2),
  K0 = \() runif(n = 1, min = -0.2, max = 0.2),
  rate = \() runif(n = 1, min = 0.8, max = 1.2),
  lambda = \() runif(n = 1, min = 0.3, max = 0.7),
  ...) {

  init <- list(
    b_K = K,
    b_K0 = K0,
    b_rate = rate,
    b_lambda = lambda,
    ...)

  class(init) <- c("bpinit", class(init))
  init
}

#' Create Initialization for the Richards Growth Model
#'
#' @description Create initial values for Richards growth model parameters
#'   that can be passed to the [growth_richards_model].
#'
#' @param K `numeric` or `numeric` returning `function`
#' @param K0 `numeric` or `numeric` returning `function`
#' @param rate `numeric` or `numeric` returning `function`
#' @param lambda `numeric` or `numeric` returning `function`
#' @param nu `numeric` or `numeric` returning `function`
#'   values range between \[0, Inf\], with 1 being standard sigmoid
#' @param ... additional parameter initialization. Each named argument should be
#'   a function that returns an `numeric` `array` of length `1`, see
#'   [rstan_default_scalar_init()] to use the rstan default init.
#'
#' @returns input for `[brm][brms::brm](init = ...)`
#'
#' @seealso [growth_richards_formula], [growth_richards_prior], and
#'   [growth_richards_model]
#'
#' @examples
#'\dontrun{
#' init <- BayesPharma::growth_richards_init(
#'   A = 100,
#'   nu = 2)
#'}
#'@export
growth_richards_init <- function(
  K = \() runif(n = 1, min = 0.8, max = 1.2),
  K0 = \() runif(n = 1, min = -0.2, max = 0.2),
  rate = \() runif(n = 1, min = 0.8, max = 1.2),
  lambda = \() runif(n = 1, min = 0.3, max = 0.7),
  nu = \() runif(n = 1, min = 0.8, max = 1.2),
  ...) {

  init <- list(
    b_K = K,
    b_K0 = K0,
    b_rate = rate,
    b_lambda = lambda,
    b_nu = nu,
    ...)

  class(init) <- c("bpinit", class(init))
  init
}
