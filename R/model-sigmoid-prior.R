#' Default priors for the sigmoid agoinst model
#'
#' @description Creates a data.frame of priors for the sigmoid agonist model
#'
#' If the function arguments ec50, hill, top, bottom = NULL, default
#' normal distribution priors will be set.
#'
#' If you would like to set a parameter as a constant, enter a numeric value for
#' the function argument.
#'
#' For other distribution options, reference
#' <http://mc-stan.org/rstanarm/reference/priors.html#arguments>
#'
#' @param ec50 `brmsprior` or numeric. Prior for the ec50 parameter.
#'   Default: normal(-7, 2.5) where the mean -7 corresponds to a
#'   concentration of 1e-7 or 100 nM. Setting ec50 to a numeric value constrains
#'   it to a constant value.
#' @param hill `brmsprior` or numeric. Prior for the hill parameter. Default:
#'   normal(1, 1) with and a lower bound of -0.1.
#' @param top `brmsprior` or numeric. Prior for the top parameter.
#'   Default: normal(100, 25). Setting top to a numeric value constrains it to
#'   a constant value.
#' @param bottom  `brmsprior` or numeric. Prior for the top parameter.
#'   Default: normal(100, 25). Setting bottom to a numeric value constrains it
#'   to a constant value.
#' @param ... additional `brmsprior` objects.
#' @return `brmsprior` `data.frame`
#'
#' @examples
#'\dontrun{
#' # Consider an activator that has a max response around 50%, EC50 is estimated
#' # to be around 1 uM, and minimum response is known to be 0.
#' priors <- sigmoid_agonist_prior(
#'   ec50 = brms::prior(normal(-6, 0.5), nlpar = "ec50"),
#'   inhibitor = FALSE,
#'   top = brms::prior(normal(50, 2.5), nlpar = "top"),
#'   bottom = 0)
#'}
#' @export
sigmoid_agonist_prior <- function(
  ec50 = brms::prior(normal(-7, 2.5), nlpar = "ec50"),
  hill = brms::prior(prior = normal(1, 1), nlpar = "hill", ub = 0.01),
  top = brms::prior(normal(100, 25), nlpar = "top"),
  bottom = brms::prior(prior = normal(0, 25), nlpar = "bottom"),
  ...) {
  
  c(
    prepare_prior(ec50, nlpar = "ec50"),
    prepare_prior(hill, nlpar = "hill"),
    prepare_prior(top, nlpar = "top"),
    prepare_prior(bottom, nlpar = "bottom"))
}

#' Default priors for the sigmoid antagonist model
#'
#' @description Creates a data.frame of priors for the sigmoid antagonist model
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
#' @param ic50 `brmsprior` or numeric. Prior for the ec50 parameter.
#'   Default: normal(-7, 2.5) where the mean -7 corresponds to a
#'   concentration of 1e-7 or 100 nM. Setting ic50 to a numeric value constrains
#'   it to a constant value.
#' @param hill `brmsprior` or numeric. Prior for the hill parameter. Default:
#'   normal(-1, 1) upper bounded by 0.1.
#' @param top `brmsprior` or numeric. Prior for the top parameter.
#'   Default: normal(100, 25). Setting top to a numeric value constrains it to
#'   a constant value.
#' @param bottom  `brmsprior` or numeric. Prior for the top parameter.
#'   Default: normal(100, 25). Setting bottom to a numeric value constrains it to
#'   a constant value.
#' @param ... additional `brmsprior` objects.
#' @return `brmsprior` `data.frame`
#'
#' @examples
#'\dontrun{
#' # Consider an activator that has a max response around 50%, EC50 is estimated
#' # to be around 1 uM, and minimum response is known to be 0.
#' priors <- sigmoid_antagoinst_prior(
#'   ic50 = brms::prior(normal(-6, 0.5), nlpar = "ic50"),
#'   top = brms::prior(normal(50, 2.5), nlpar = "top"),
#'   bottom = 0)
#'}
#' @export
sigmoid_antagoinst_prior <- function(
    ic50 = brms::prior(normal(-7, 2.5), nlpar = "ic50"),
    hill = brms::prior(prior = normal(-1, 1), nlpar = "hill", lb = -0.01),
    top = brms::prior(normal(100, 25), nlpar = "top"),
    bottom = brms::prior(prior = normal(0, 25), nlpar = "bottom"),
    ...) {
 
  c(
    prepare_prior(ec50, nlpar = "ec50"),
    prepare_prior(hill, nlpar = "hill"),
    prepare_prior(top, nlpar = "top"),
    prepare_prior(bottom, nlpar = "bottom"))
}
