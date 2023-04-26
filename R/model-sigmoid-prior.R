#' Default priors for the sigmoid agonist model
#'
#' @description Creates a `data.frame` of priors for the sigmoid agonist
#'     model
#'
#' If the function arguments `ec50`, `hill`, `top`,
#' `bottom` are `NULL`, default normal distribution priors
#' will be set.
#'
#' If you would like to set a parameter as a constant, enter a numeric
#' value for the function argument.
#'
#' For other distribution options, reference
#' <http://mc-stan.org/rstanarm/reference/priors.html#arguments>
#'
#' @param ec50 `brmsprior` or numeric. Prior for the ec50
#'     parameter.  Default: normal(-6, 2.5) where the mean -7
#'     corresponds to a concentration of 1e-6 or 1 μM. Setting ec50 to
#'     a numeric value constrains it to a constant value.
#' @param hill `brmsprior` or numeric. Prior for the hill
#'     parameter. Default: normal(1, 1) with and a lower bound of
#'     -0.1.
#' @param top `brmsprior` or numeric. Prior for the top
#'     parameter.  Default: normal(1, 0.5). Setting top to a numeric
#'     value constrains it to a constant value.
#' @param bottom `brmsprior` or numeric. Prior for the top
#'     parameter.  Default: normal(0, 0.5). Setting bottom to a
#'     numeric value constrains it to a constant value.
#' @param ... additional `brmsprior` objects.
#' @returns `brmsprior` `data.frame`
#'
#' @examples
#'\dontrun{
#' # Consider an activator that has a max response around 50%, EC50 is estimated
#' # to be around 1 nM, and minimum response is known to be 0.
#' priors <- sigmoid_agonist_prior(
#'   ec50 = brms::prior(prior = normal(-9, 0.5), nlpar = "ec50"),
#'   top = brms::prior(prior = normal(0.5, 0.2), nlpar = "top"),
#'   bottom = 0)
#'}
#' @export
sigmoid_agonist_prior <- function(
  ec50 = brms::prior(prior = normal(-6, 2.5), nlpar = "ec50"),
  hill = brms::prior(prior = normal(1, 1), nlpar = "hill", lb = -0.01),
  top = brms::prior(prior = normal(1, 0.5), nlpar = "top"),
  bottom = brms::prior(prior = normal(0, 0.5), nlpar = "bottom"),
  ...) {

    c(
        prepare_prior(prior = ec50, nlpar = "ec50"),
        prepare_prior(prior = hill, nlpar = "hill"),
        prepare_prior(prior = top, nlpar = "top"),
        prepare_prior(prior = bottom, nlpar = "bottom"))
}

#' Default priors for the sigmoid antagonist model
#'
#' @description Creates a `data.frame` of priors for the sigmoid antagonist
#'     model
#'
#' If the function arguments ic50, hill, top, bottom = NULL, default
#' normal distribution priors will be set.
#'
#' If you would like to set a parameter as a constant, enter a numeric value for
#' the function argument.
#'
#' For other distribution options, reference
#' <http://mc-stan.org/rstanarm/reference/priors.html#arguments>
#'
#' @param ic50 [brms::brmsprior()] or `numeric`. Prior for the
#'   `ic50` parameter. (Default: `normal(-6, 2.5)` where the mean
#'   `-6` corresponds to a concentration of 1e-6 or 1 μM. Setting
#'   `ic50` to a numeric value constrains it to a constant value.
#' @param hill [brms::brmsprior()] or `numeric`. Prior for the
#'   `hill` parameter. (Default: `normal(-1, 1)` upper bounded by
#'   `0.1`).
#' @param top [brms::brmsprior()] or `numeric`. Prior for the
#'   `top` parameter. (Default: `normal(1, 0.5)`. Setting `top`
#'   to a numeric value constrains it to a constant value.
#' @param bottom [brms::brmsprior()] or `numeric`. Prior for the
#'   bottom parameter. (Default: `normal(0, 0.5)`. Setting `bottom` to
#'   a numeric value constrains it to a constant value.
#' @param ... additional [brms::brmsprior()] objects.
#' @returns [brms::brmsprior()] `data.frame`
#'
#' @examples
#'\dontrun{
#' # Consider an activator that has a max response around 50%, EC50 is estimated
#' # to be around 1 μM, and minimum response is known to be 0.
#' priors <- sigmoid_antagonist_prior(
#'   ic50 = brms::prior(prior = normal(-6, 0.5), nlpar = "ic50"),
#'   top = brms::prior(prior = normal(0.5, 0.5), nlpar = "top"),
#'   bottom = 0)
#'}
#' @export
sigmoid_antagonist_prior <- function(
    ic50 = brms::prior(prior = normal(-6, 2.5), nlpar = "ic50"),
    hill = brms::prior(prior = normal(-1, 1), nlpar = "hill", ub = 0.01),
    top = brms::prior(prior = normal(1, 0.5), nlpar = "top"),
    bottom = brms::prior(prior = normal(0, 0.5), nlpar = "bottom"),
    ...) {

  c(
    prepare_prior(prior = ic50, nlpar = "ic50"),
    prepare_prior(prior = hill, nlpar = "hill"),
    prepare_prior(prior = top, nlpar = "top"),
    prepare_prior(prior = bottom, nlpar = "bottom"))
}
