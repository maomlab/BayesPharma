#' Default priors for the sigmoid model
#'
#' @description Creates a data.frame of priors for the sigmoid model
#'
#' If the function arguments ec50, hill, top, bottom = NULL, default
#' normal distribution priors will be set.
#'
#' If inhibitor = TRUE, hill is positive. If inhibitor = FALSE, hill is
#' negative.
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
#'   normal(-1, 1) upper bounded by 0.1 if inhibitor is TRUE and normal(1, 1)
#'   with and a lower bound of -0.1. The dependence on inhibitor is designed
#'   to make the model identifiable, otherwise swapping top and bottom and
#'   reversing the sign of hill will give the same model.
#' @param inhibitor TRUE/FALSE value. This determines if a positive or negative
#'    slope is used for the hill prior. (default = TRUE)
#' @param top `brmsprior` or numeric. Prior for the top parameter.
#'   Default: normal(100, 25). Setting top to a numeric value constrains it to
#'   a constant value.
#' @param bottom  `brmsprior` or numeric. Prior for the top parameter.
#'   Default: normal(100, 25). Setting ec50 to a numeric value constrains it to
#'   a constant value.
#' @param ... additional `brmsprior` objects.
#' @return `brmsprior` `data.frame`
#'
#' @examples
#'\dontrun{
#' # Consider an activator that has a max response around 50%, EC50 is estimated
#' # to be around 1 uM, and minimum response is known to be 0.
#' priors <- sigmoid_prior(
#'   ec50 = brms::prior(normal(-6, 0.5), nlpar = "ec50"),
#'   inhibitor = FALSE,
#'   top = brms::prior(normal(50, 2.5), nlpar = "top"),
#'   bottom = 0)
#'}
#' @export
sigmoid_prior <- function(
  ec50 = brms::prior(normal(-7, 2.5), nlpar = "ec50"),
  hill = NULL,
  inhibitor = TRUE,
  top = brms::prior(normal(100, 25), nlpar = "top"),
  bottom = brms::prior(prior = normal(0, 25), nlpar = "bottom"),
  ...) {

  # To make the model identifiable, force inhibitor the slope to be either
  # positive or negative
  if(is.null(hill)){
    if(inhibitor) {
      hill <- brms::prior(
        prior = normal(-1, 1),
        nlpar = "hill",
        lb = -0.01)
    } else {
      hill <- brms::prior(
        prior = normal(1, 1),
        nlpar = "hill",
        ub = 0.01)
    }
  }
  
  c(
    prepare_prior(ec50, nlpar = "ec50"),
    prepare_prior(hill, nlpar = "hill"),
    prepare_prior(top, nlpar = "top"),
    prepare_prior(bottom, nlpar = "bottom"))

}
