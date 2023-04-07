#' Default priors for the Richards growth model
#'
#' @description Creates a \code{data.frame} of priors for the Richards growth
#'     model
#'
#' If the function arguments \code{A}, \code{nu}, \code{mu},
#' \code{lambda} are \code{NULL}, default normal distribution priors
#' will be set.
#'
#' If you would like to set a parameter as a constant, enter a numeric
#' value for the function argument.
#'
#' For other distribution options, reference
#' <http://mc-stan.org/rstanarm/reference/priors.html#arguments>
#'
#' @param K \code{brmsprior} or numeric. Prior for the \code{K} parameter.
#'     (Default: \code{normal(-6, 2.5)} where the mean -7
#'     corresponds to a concentration of 1e-6 or 1 μM. Setting ec50 to
#'     a numeric value constrains it to a constant value.
#' @param K0 \code{brmsprior} or numeric. Prior for the \code{K0} parameter.
#' @param rate \code{brmsprior} or numeric. Prior for the hill
#'     parameter. Default: normal(1, 1) with and a lower bound of
#'     -0.1.
#' @param lambda \code{brmsprior} or numeric. Prior for the top
#'     parameter.  Default: normal(1, 0.5). Setting top to a numeric
#'     value constrains it to a constant value.
#' @param mu \code{brmsprior} or numeric. Prior for the top
#'     parameter.  Default: normal(0, 0.5). Setting bottom to a
#'     numeric value constrains it to a constant value.
#' @param ... additional \code{brmsprior} objects.
#' @returns \code{brmsprior} \code{data.frame}
#'
#' @examples
#'\dontrun{
#' priors <- sigmoid_agonist_prior(
#'   K = brms::prior(prior = normal(100, 20), nlpar = "K"),
#'   nu = brms::prior(prior = normal(2, 4), nlpar = "nu"),
#'   lambda = 1)
#'}
#' @export
growth_richards_prior <- function(
    K = brms::prior(prior = normal(1, 0.5), nlpar = "K"),
    K0 = brms::prior(prior = normal(0, 0.5), nlpar = "K0"),
    rate = brms::prior(prior = normal(2, 0.5), nlpar = "rate", lb = 0),
    lambda = brms::prior(prior = normal(0.5, 0.5), nlpar = "lambda"),
    nu = brms::prior(prior = normal(1, 1), nlpar = "nu", lb = 0),
    ...) {
  
  c(
    prepare_prior(prior = K, nlpar = "K"),
    prepare_prior(prior = K0, nlpar = "K0"),
    prepare_prior(prior = rate, nlpar = "rate"),
    prepare_prior(prior = lambda, nlpar = "lambda"),
    prepare_prior(prior = nu, nlpar = "nu"))
}
