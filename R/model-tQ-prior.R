#' Create a Prior for the tQ Enzyme Kinetics Model
#'
#' @description For both the `kcat` and `kM` parameters, the default priors are
#' `gamma(4, 1)`. We use the gamma distribution because it is naturally lower
#' bounded by `0`. The first parameter is the shape, `alpha=4`, and the second
#' is the rate, `beta=1`. The mean of gamma distributions is `alpha/beta` and
#' the variance is `alpha/beta^2`.
#'
#' @param kcat prior for `kcat` parameter. Given a numeric value, it will be
#'   used as a constant and not estimated.
#' @param kM prior for `kM` parameter. Given a numeric value, it will be used as
#'   a constant and not estimated.
#'
#' @seealso [tQ_model], [tQ_formula], [tQ_init], and [tQ_stanvar]
#'
#' @export
tQ_prior <- function(
    kcat = brms::prior_string(prior = "gamma(4, 1)", lb = 0, nlpar = "kcat"),
    kM = brms::prior_string(prior = "gamma(4, 1)", lb = 0, nlpar = "kM")) {

  c(
    prepare_prior(kcat, nlpar = "kcat"),
    prepare_prior(kM, nlpar = "kM"))
}
