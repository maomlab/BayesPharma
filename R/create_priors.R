
#' data.frame of pre-determined priors of parameters to be predicted by the
#' model
#'
#' If the function arguments ec50, hill, top, bottom = NULL, pre-set
#' normal distribution priors will be set.
#' If agonist = TRUE, hill is positive. If agonist = FALSE, hill is negative.
#' If you would like to set a parameter as a constant, enter a numeric value for
#' the function argument.
#' If you would like to set your own distribution, for example, in the function
#' arguments type 'ec50 = brms::prior(distribution(mu,sigma), nlpar = 'ec50')'.
#' For other distribution options, reference
#' http://mc-stan.org/rstanarm/reference/priors.html#arguments.
#'
#'@param ec50 NULL, numeric value, or brms::prior. NULL will provide a weakly
#'informative prior. Setting ec50 to a numeric value will set the ec50 prior to
#'a constant value of the given numeric value. A prior of choice can be set
#'using brms::prior(). (default = NULL)
#'@param hill NULL, numeric value, or brms::prior. NULL will provide a weakly
#'informative prior. Setting hill to a numeric value will set the hill prior to
#'a constant value of the given numeric value. A prior of choice can be set
#'using brms::prior(). (default = NULL)
#'@param agonist TRUE/FALSE value. This determines if a positive or negative
#' slope is used for the hill prior. (default = TRUE)
#'@param top NULL, numeric value, or brms::prior. NULL will provide a weakly
#'informative prior. Setting top to a numeric value will set the top prior to
#'a constant value of the given numeric value. A prior of choice can be set
#'using brms::prior(). (default = NULL)
#'@param bottom  NULL, numeric value, or brms::prior. NULL will provide a weakly
#'informative prior. Setting bottom to a numeric value will set the bottom prior
#'to a constant value of the given numeric value. A prior of choice can be set
#'using brms::prior(). (default = NULL)
#'@return brmsprior data.frame
#'
#'@export

dr_priors <- function(ec50 = NULL,
                      hill = NULL,
                      agonist = TRUE,
                      top = NULL,
                      bottom = NULL) {

  cat("If the function arguments ec50, hill, top, bottom = NULL, pre-set
  normal distribution priors will be set. If agonist = TRUE, hill is positive.
  If agonist = FALSE, hill is negative.
  If you would like to set a parameter as a constant, enter a numeric value for
  the function argument.
  If you would like to set your own distribution, for example, in the function
  arguments type 'ec50 = brms::prior(distribution(mu,sigma), nlpar = 'ec50')'.
  For other distribution options, reference
  http://mc-stan.org/rstanarm/reference/priors.html#arguments.
      ")


  if (is.null(ec50)) {
    ec50_prior <- brms::prior(normal(-7,5), nlpar = 'ec50')
    print("ec50 prior set to a normal distribution with a mean of -7 and a standard deviation of 5.")

  } else if (is.numeric(ec50)) {
    ec50_prior <- brms::prior_string(paste0("constant(",ec50,")"), nlpar = 'ec50')
    print(paste0("ec50 prior set to a constant value of ", ec50))
  } else {
    ec50_prior <- ec50
    print(paste0("ec50 prior is set to ", ec50))

  }
  if (is.null(hill) && agonist == TRUE) {
    hill_prior <- brms::prior(normal(1,2),
                              nlpar = 'hill',
                              lb = -0.01)
    print("hill prior set to a normal distribution with a mean of 1 and a standard deviation of 2 with a lower bound of -0.01.")

  } else if (is.null(hill) && agonist == FALSE) {
    hill_prior <- brms::prior(normal(-1,2),
                              nlpar = 'hill',
                              ub = 0.01)
    print("hill prior set to a normal distribution with a mean of -1 and a standard deviation of 2 with an upper bound of 0.01.")

  } else if (is.numeric(hill)) {
    hill_prior <- brms::prior_string(paste0("constant(",hill,")"), nlpar = 'hill')
    print(paste0("hill prior set to constant value of ", hill))

  } else {
    hill_prior <- hill
    print(paste0("hill prior set to ", hill))
  }

  if (is.null(top)) {
    top_prior <- brms::prior(normal(100,25), nlpar = "top")
    print("top prior set to a normal distribution with a mean of 100 and a standard deviation of 25.")

  } else if (is.numeric(top)) {
    top_prior <- brms::prior_string(paste0("constant(",top,")"), nlpar = 'top')
    print(paste0("top prior set to a constant value of ", top))

  } else {
    top_prior <- top
    print(paste0("top prior set to ", top))
  }

  if (is.null(bottom)) {
    bottom_prior <- brms::prior(normal(0,25), nlpar = "bottom")
    print("bottom prior set to a normal distribution with a mean of 0 and a standard deviation of 25")

  } else if (is.numeric(bottom)) {
    bottom_prior <- brms::prior_string(paste0("constant(",bottom,")"), nlpar = 'bottom')
    print(paste0("bottom prior set to a constant value of ", bottom))

  } else {
    bottom_prior <- bottom
    print(paste0("bottom prior set to ", bottom))
  }

  priors <- c(ec50_prior, hill_prior, top_prior, bottom_prior)
  return(priors)
}
