#' Initialize Parameter Values for an Agonist Sigmoid Model
#'
#' @description Creating initial values for an agonist sigmoid model parameters
#'   that can be passed to the [sigmoid_model()] along with the
#'   [sigmoid_agonist_formula()] and [sigmoid_agonist_prior()].
#'
#' @param ec50 `numeric` or `numeric` returning `function` units of the
#'   treatment. If the treatment is `log_dose`, the default value of `-6`
#'  corresponds `1e-6` molar = `1` μM
#' @param hill `numeric` or `numeric` returning `function` with units
#'   `response/log_dose`
#' @param top `numeric` or `numeric` returning `function` with units of the
#'   response
#' @param bottom `numeric` or `numeric` returning `function` with units of the
#'   response
#' @param ... additional parameter initialization. Each named argument should be
#'   a function that returns a `numeric` or `array` depending on the dimension
#'   of the parameter, see [rstan_default_init()] to use the \pkg{rstan} default
#'   init.
#'
#' @returns input for `[brms::brm](init = ...)`
#'
#' @seealso [sigmoid_agonist_formula()], [sigmoid_agonist_prior()], and
#'   [sigmoid_model()]
#'
#' @examples
#'\dontrun{
#' #Consider an activator that has a max response around 50%, EC50 is estimated
#' #to be around 1 nM, minimum response is known to be 0.
#' init <- BayesPharma::sigmoid_agonist_init(
#'   ec50 = -9,
#'   top = 0.5)
#'}
#'@export
sigmoid_agonist_init <- function(
  ec50 = \() stats::runif(n = 1, min = -7, max = -5),
  hill = \() stats::runif(n = 1, min = 0.8, max = 1.2),
  top = \() stats::runif(n = 1, min = 0.8, max = 1.2),
  bottom = \() stats::runif(n = 1, min = -0.2, max = 0.2),
  ...) {
  init <- list(
    b_ec50 = ec50,
    b_hill = hill,
    b_top = top,
    b_bottom = bottom,
    ...)

  class(init) <- c("bpinit", class(init))
  init
}


#' Initialize Parameter Values for an Antagonist Sigmoid Model
#'
#' @description Creating initial values for an agonist sigmoid model parameters
#'   that can be passed to the [sigmoid_model()] along with the
#'   [sigmoid_antagonist_formula()] and [sigmoid_antagonist_prior()].
#'
#' @param ic50 `numeric` or `numeric` returning `function` units of the
#'   treatment. If the treatment is `log_dose`, the default value of `-6`
#'   corresponds `1e-6` molar = `1` μM
#' @param hill `numeric` or `numeric` returning `function` with units
#'   response/log_dose
#' @param top `numeric` or `numeric` returning `function` in units of the
#'   response
#' @param bottom `numeric` or `numeric` returning `function` in units of the
#'   response
#' @param ... additional parameter initialization. Each named argument should be
#'   a function that returns an `numeric` `array` of length `1`, see
#'   [rstan_default_scalar_init()] to use the rstan default init.
#' @returns input for `[brm][brms::brm](init = ...)`
#'
#' @seealso [sigmoid_antagonist_formula()], [sigmoid_antagonist_prior()], and
#'   [sigmoid_model()]
#'
#' @examples
#'\dontrun{
#' # Consider an inhibitor that has a min response around 50%, IC50 is estimated
#' # to be around 1 nM, maximum response is known to be around 1,
#' init <- BayesPharma::sigmoid_antagonist_init(
#'   ec50 = -9,
#'   bottom = 0.5)
#'
#' # By default rstan initializes unspecified initial values to be uniformly at
#' # random in the range (-2, 2) on the unconstrained scale. For the default
#' # distributional response, family=gaussian(), the shape parameter sigma' is
#' # bounded below by zero through this transformation Y = log(X - 0). So, to
#' # explicitly give the default initialization for 'sigma', we can use
#' init <- BayesPharma::sigmoid_antagonist_init(
#'   sigma = BayesPharma::rstan_default_init(lb = 0))
#'}
#'@export
sigmoid_antagonist_init <- function(
  ic50 = \() stats::runif(n = 1, min = -7, max = -5),
  hill = \() stats::runif(n = 1, min = -1.2, max = -0.8),
  top = \() stats::runif(n = 1, min = 0.8, max = 1.2),
  bottom = \() stats::runif(n = 1, min = -0.2, max = 0.2),
  ...) {
  init <- list(
    b_ic50 = ic50,
    b_hill = hill,
    b_top = top,
    b_bottom = bottom,
    ...)

  class(init) <- c("bpinit", class(init))
  init
}
