#' Create a Prior for the Sigmoid Growth Model
#'
#' @description Creates a for the priors for the sigmoid growth model. If the
#' function arguments `K`, `K0`, `rate`, `lambda`, or `NULL`, normal
#' distribution priors will be set.
#'
#' If you would like to set a parameter as a constant, enter a numeric  value
#' for the function argument.
#'
#' For other distribution options, reference [brms::prior] and [Prior Choice
#' Recommendations
#' ](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations)
#'
#' @param K [brms::brmsprior()] or `numeric`. Prior for the
#'   `K` parameter. (Default: `normal(1, 0.5)`)
#' @param K0 [brms::brmsprior()] or `numeric`. Prior for the
#'   `K0` parameter. (Default: `normal(0, 0.5)`)
#' @param rate [brms::brmsprior()] or `numeric`. Prior for the
#'   `rate` parameter. (Default: `normal(1, 1)` with and a lower bound
#'   of `0`).
#' @param lambda [brms::brmsprior()] or `numeric`. Prior for the
#'   `lambda` parameter. (Default: `normal(0.5, 0.5)`).
#' @param ... additional [brms::brmsprior()] objects.
#'
#' @returns [brms::brmsprior()] `data.frame`
#'
#' @examples
#'\dontrun{
#' priors <- growth_sigmoid_prior(
#'   K = brms::prior(prior = normal(100, 20), nlpar = "K"),
#'   lambda = 1)
#'}
#' @export
growth_sigmoid_prior <- function(
    K = brms::prior(prior = normal(1, 0.5), nlpar = "K"),
    K0 = brms::prior(prior = normal(0, 0.5), nlpar = "K0"),
    rate = brms::prior(prior = normal(2, 0.5), nlpar = "rate", lb = 0),
    lambda = brms::prior(prior = normal(0.5, 0.5), nlpar = "lambda"),
    ...) {

  c(
    prepare_prior(prior = K, nlpar = "K"),
    prepare_prior(prior = K0, nlpar = "K0"),
    prepare_prior(prior = rate, nlpar = "rate"),
    prepare_prior(prior = lambda, nlpar = "lambda"))
}

#' Create a Prior for the Richards Growth Model
#'
#' @description Creates a for the priors for the Richards growth model. If the
#' function arguments `K`, `K0`, `rate`, `lambda`, or `nu` are `NULL`, normal
#' distribution priors will be set.
#'
#' If you would like to set a parameter as a constant, enter a numeric
#' value for the function argument.
#'
#' For other distribution options, reference [brms::prior] and [Prior Choice
#' Recommendations
#' ](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations)
#'
#' @param K [brms::brmsprior()] or `numeric`. Prior for the
#'   `K` parameter. (Default: `normal(1, 0.5)`).
#' @param K0 [brms::brmsprior()] or `numeric`. Prior for the
#'   `K0` parameter. (Default: `normal(0, 0.5)`).
#' @param rate [brms::brmsprior()] or `numeric`. Prior for the
#'   `rate` parameter. (Default: `normal(1, 1)` with and a lower bound
#'   of `0`).
#' @param lambda [brms::brmsprior()] or `numeric`. Prior for the
#'   `lambda` parameter. (Default: `normal(0.5, 0.5)`).
#' @param nu [brms::brmsprior()] or `numeric`. Prior for the
#'   `nu` parameter.  (Default: normal(1, 1) with a lower bound of
#'   `0`).
#' @param ... additional [brms::brmsprior()] objects.
#'
#' @returns [brms::brmsprior()] `data.frame`
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
