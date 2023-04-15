#' Create a prior for the sigmoid growth model
#'
#' @description Creates a for the priors for the sigmoid growth model. If the
#'   function arguments \code{K}, \code{K0}, \code{rate}, \code{lambda}, or
#'   \code{NULL}, normal distribution priors will be set.
#'
#'   If you would like to set a parameter as a constant, enter a numeric  value
#'   for the function argument.
#'
#'   For other distribution options, reference
#'   <http://mc-stan.org/rstanarm/reference/priors.html#arguments>
#'
#' @param K \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{K} parameter. (Default: \code{normal(1, 0.5)})
#' @param K0 \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{K0} parameter. (Default: \code{normal(0, 0.5)})
#' @param rate \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{rate} parameter. (Default: \code{normal(1, 1)} with and a lower bound
#'   of \code{0}).
#' @param lambda \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{lambda} parameter. (Default: \code{normal(0.5, 0.5)}).
#' @param ... additional \code{\link[brms]{brmsprior}} objects.
#'
#' @returns \code{\link[brms]{brmsprior}} \code{data.frame}
#'
#' @examples
#'\dontrun{
#' priors <- growth_richards_prior(
#'   K = brms::prior(prior = normal(100, 20), nlpar = "K"),
#'   lambda = 1)
#'}
#' @export
growth_richards_prior <- function(
    K = brms::prior(prior = normal(1, 0.5), nlpar = "K"),
    K0 = brms::prior(prior = normal(0, 0.5), nlpar = "K0"),
    rate = brms::prior(prior = normal(2, 0.5), nlpar = "rate", lb = 0),
    lambda = brms::prior(prior = normal(0.5, 0.5), nlpar = "lambda"),
    ...) {
  
  c(
    prepare_prior(prior = K, nlpar = "K"),
    prepare_prior(prior = K0, nlpar = "K0"),
    prepare_prior(prior = rate, nlpar = "rate"),
    prepare_prior(prior = lambda, nlpar = "lambda"),
    prepare_prior(prior = nu, nlpar = "nu"))
}

#' Create a prior for the Richards growth model
#'
#' @description Creates a for the priors for the Richards growth model. If the
#'   function arguments \code{K}, \code{K0}, \code{rate}, \code{lambda}, or
#'   \code{nu} are \code{NULL}, normal distribution priors will be set.
#'
#' If you would like to set a parameter as a constant, enter a numeric
#' value for the function argument.
#'
#' For other distribution options, reference
#' <http://mc-stan.org/rstanarm/reference/priors.html#arguments>
#'
#' @param K \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{K} parameter. (Default: \code{normal(1, 0.5)}).
#' @param K0 \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{K0} parameter. (Default: \code{normal(0, 0.5)}).
#' @param rate \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{rate} parameter. (Default: \code{normal(1, 1)} with and a lower bound
#'   of \code{0}).
#' @param lambda \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{lambda} parameter. (Default: \code{normal(0.5, 0.5)}).
#' @param nu \code{\link[brms]{brmsprior}} or \code{numeric}. Prior for the
#'   \code{nu} parameter.  (Default: normal(1, 1) with a lower bound of
#'   \code{0}).
#' @param ... additional \code{\link[brms]{brmsprior}} objects.
#'
#' @returns \code{\link[brms]{brmsprior}} \code{data.frame}
#'
#' @examples
#'\dontrun{
#' priors <- growth_richards_prior(
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
