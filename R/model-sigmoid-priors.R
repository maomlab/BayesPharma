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
#' http://mc-stan.org/rstanarm/reference/priors.html#arguments.
#'
#' @param ec50 NULL, numeric units, or brms::prior. NULL will provide a weakly
#'   informative prior with mean of 100 nM. Setting ec50 to a numeric value will
#'   set the ec50 prior to a constant value of the given numeric value. A prior
#'   of choice can be set using brms::prior(). (default = NULL)
#' @param hill NULL, numeric value, or brms::prior. NULL will provide a weakly
#'   informative prior. Setting hill to a numeric value will set the hill prior
#'   to a constant value of the given numeric value. A prior of choice can be
#'   set using brms::prior(). (default = NULL)
#' @param inhibitor TRUE/FALSE value. This determines if a positive or negative
#'    slope is used for the hill prior. (default = TRUE)
#' @param top NULL, numeric value, or `brms::prior`. NULL will provide a weakly
#'   informative prior. Setting top to a numeric value will set the top prior to
#'   a constant value of the given numeric value. A prior of choice can be set
#'   using `brms::prior()`. (default = NULL)
#' @param bottom  NULL, numeric value, or `brms::prior`. NULL will provide a
#' weakly
#'   informative prior. Setting bottom to a numeric value will set the bottom
#'   prior to a constant value of the given numeric value. A prior of choice can
#'   be set using `brms::prior()`. (default = NULL)
#' @return brmsprior data.frame
#'
#' @examples
#'\dontrun{
#' Consider an activator that has a max response around 50%, EC50 is estimated
#' to be around 1 uM, and minimum response is known to be 0.
#' 
#' priors <- dr_priors(
#'   ec50 = brms::prior(normal(-6, 0.5), nlpar = "ec50"),
#'   hill = NULL,
#'   inhibitor = FALSE,
#'   top = brms::prior(normal(50, 2.5), nlpar = "top"),
#'   bottom = 0)
#'}
#' @export

dr_priors <- function(
  ec50 = NULL,
  hill = NULL,
  inhibitor = TRUE,
  top = NULL,
  bottom = NULL,
  ...) {

  if (is.null(ec50)) {
    ec50_prior <- brms::prior(
      prior = normal(-7, 2.5),
      nlpar = "ec50")

  } else if (is.numeric(ec50)) {
    ec50_prior <- brms::prior_string(
      prior = paste0("constant(", ec50, ")"),
      nlpar = "ec50")

  } else {
    ec50_prior <- ec50
    assertthat::assert_that(is(ec50_prior, "brmsprior"))
    assertthat::assert_that(ec50_prior$nlpar == "ec50")
  }
  if (is.null(hill) && inhibitor == FALSE) {
    hill_prior <- brms::prior(
      prior = normal(1, 1),
      nlpar = "hill",
      lb = -0.01)

  } else if (is.null(hill) && inhibitor == TRUE) {
    hill_prior <- brms::prior(
      prior = normal(-1, 1),
      nlpar = "hill",
      ub = 0.01)

  } else if (is.numeric(hill)) {
    hill_prior <- brms::prior_string(
      prior = paste0("constant(", hill, ")"),
      nlpar = "hill")

  } else {
    hill_prior <- hill
    assertthat::assert_that(is(hill_prior, "brmsprior"))
    assertthat::assert_that(hill_prior$nlpar == "hill")
  }

  if (is.null(top)) {
    top_prior <- brms::prior(
      prior = normal(100, 25),
      nlpar = "top")

  } else if (is.numeric(top)) {
    top_prior <- brms::prior_string(
      prior = paste0("constant(", top, ")"),
      nlpar = "top")

  } else {
    top_prior <- top
    assertthat::assert_that(is(top_prior, "brmsprior"))
    assertthat::assert_that(top_prior$nlpar == "top")
    
  }

  if (is.null(bottom)) {
    bottom_prior <- brms::prior(
      prior = normal(0, 25),
      nlpar = "bottom")

  } else if (is.numeric(bottom)) {
    bottom_prior <- brms::prior_string(
      prior = paste0("constant(", bottom, ")"),
      nlpar = "bottom")

  } else {
    bottom_prior <- bottom
    assertthat::assert_that(is(bottom_prior, "brmsprior"))
    assertthat::assert_that(bottom_prior$nlpar == "bottom")
  }

  priors <- c(ec50_prior, hill_prior, top_prior, bottom_prior, ...)
  return(priors)
}
