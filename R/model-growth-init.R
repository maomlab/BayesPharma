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
  K = 1,
  K0 = 0,
  rate = 1,
  lambda = 0.5,
  ...) {

  function() {
    list(
      b_K = prepare_init(K),
      b_K0 = prepare_init(K0),
      b_rate = prepare_init(rate),
      b_lambda = prepare_init(lambda),
      ...)
  }
}

#' Create Initialization for the Richards Growth Model
#'
#' @description Creat initial values for Richards growth model parameters
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
  K = 1,
  K0 = 0,
  rate = 1,
  lambda = 0.5,
  nu = 1,
  ...) {

  function() {
    list(
      b_K = prepare_init(K),
      b_K0 = prepare_init(K0),
      b_rate = prepare_init(rate),
      b_lambda = prepare_init(lambda),
      b_nu = prepare_init(nu),
      ...)
  }
}
