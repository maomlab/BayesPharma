#' Define priors for the tQ enzyme kinetics model
#'
#' Default priors are gamma(4, 1) for both `kcat` and
#' `kM`. We use the gamma distribution because it is naturally
#' lower bounded by 0. The first parameter is the shape,
#' `alpha=4`, and the second is the rate, `beta=1`. The mean
#' of gamma distributions is `alpha/beta` and the variance is
#' `alpha/beta^2`.
#'
#' @param kcat prior for `kcat` parameter (Default: NULL). Given
#'     a numeric value, it will be used as a constant and not
#'     estimated.
#' @param kM prior for `kM` parameter (Default: NULL). Given a
#'     numeric value, it will be used as a constant and not estimated.
#'
#'
#' @seealso [tQ_model], [tQ_formula], [tQ_init], or [tQ_stanvar]
#'
#' @export
tQ_prior <- function(
    kcat = NULL,
    kM = NULL) {

  if (is.null(kcat)) {
    kcat_prior <- brms::prior_string(
      prior = "gamma(4, 1)",
      lb = 0,
      nlpar = "kcat")
  } else if (is.numeric(kcat)) {
    kcat_prior <- brms::prior_string(
      prior = paste0("constant(", kcat, ")"),
      nlpar = "kcat")
  } else {
    kcat_prior <- kcat
  }

  if (is.null(kM)) {
    kM_prior <- brms::prior_string(
      prior = "gamma(4, 1)",
      lb = 0,
      nlpar = "kM")
  } else if (is.numeric(kM)) {
    kM_prior <- brms::prior_string(
      prior = paste0("constant(", kM, ")"),
      nlpar = "kM")
  } else {
    kM_prior <- kM
  }

  c(kcat_prior, kM_prior)
}
