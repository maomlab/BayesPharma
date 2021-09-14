
#' data.frame of pre-determined priors of parameters to be predicted by the
#' model
#'
#'@param ec50 TRUE/FALSE value. If TRUE then a prior will be made for this
#' parameter and to be predicted by the model. (default = TRUE)
#'@param hill TRUE/FALSE value. If TRUE then a prior will be made for this
#' parameter and to be predicted by the model. (default = TRUE)
#'@param agonist TRUE/FALSE value. This determines if a positive or negative
#' slope is used for the hill prior. (default = TRUE)
#'@param top TRUE/FALSE value. If TRUE then a prior will be made for this
#' parameter and to be predicted by the model. (default = TRUE)
#'@param bottom TRUE/FALSE value. If TRUE then a prior will be made for this
#' parameter and to be predicted by the model. (default = TRUE)
#'@return brmsprior data.frame
#'
#'@export

make_priors <- function(ec50 = TRUE,
                           hill = TRUE,
                           agonist = TRUE,
                           top = TRUE,
                           bottom = TRUE) {

  cat("If the function arguments ec50, hill, top, bottom = TRUE, pre-set
  normal distribution priors will be set. If ec50, hill, top, bottom = FALSE,
  pre-set constant priors will be set. If agonist = TRUE, hill is positive. If
  agonist = FALSE, hill is negative.
  If you would like to set your own distribution, for example in the function
  arguments type 'ec50 = brms::prior(distribution(mu,sigma), nlpar = 'ec50')'.
  If you would like to set a parameter as a constant, for example in the
  function arguments type 'ec50 = brms::prior(constant(n), nlpar = 'ec50')'.
  For other distribution options, reference
  http://mc-stan.org/rstanarm/reference/priors.html#arguments .")


  if (ec50 == TRUE) {
    ec50_prior <- brms::prior(normal(-7,5), nlpar = 'ec50')
    cat("ec50 prior set to a normal distribution with a mean of -7 and a
          standard deviation of 5.")

  } else if (ec50 == FALSE) {
    ec50_prior <- brms::prior(constant(-7.5), nlpar = 'ec50')
    cat("a constant value of -7.5 is set for the ec50 prior.")

  } else {
    ec50_prior <- ec50

  }
  if (hill == TRUE && agonist == TRUE) {
    hill_prior <- brms::prior(normal(1,2),
                              nlpar = 'hill',
                              lb = -0.01)
    cat("hill prior set to a normal distribution with a mean of 1 and a
          standard deviation of 2.")

  } else if (hill == TRUE && agonist == FALSE) {
    hill_prior <- brms::prior(normal(-1,2),
                                        nlpar = 'hill',
                                        ub = 0.01)
    cat("hill prior set to a normal distribution with a mean of -1 and a
          standard deviation of 2.")

  } else if (hill == FALSE && agonist == TRUE) {
    hill_prior <- brms::prior(constant(1), nlpar = "hill")
    cat("hill prior set to constant value of 1")

  } else if (hill == FALSE && agonist == FALSE) {
    hill_prior <- brms::prior(constant(-1), nlpar = "hill")
    cat("hill prior set to constant value of -1")

  } else {
    hill_prior <- hill
  }

  if (top == TRUE) {
    top_prior <- brms::prior(normal(100,25), nlpar = "top")
    cat("top prior set to a normal distribution with a mean of 100 and a
          standard deviation of 25.")

  } else if (top == FALSE) {
    top_prior <- brms::prior(constant(100), nlpar = "top")
    cat("top prior set to a constant value of 100.")

  } else {
    top_prior <- top
  }

  if (bottom == TRUE) {
    bottom_prior <- brms::prior(normal(0,25), nlpar = "bottom")

  } else if (bottom == FALSE) {
    bottom_prior <- brms::prior(constant(0), nlpar = "bottom")

  } else {
    bottom_prior <- bottom
  }

  priors <- c(ec50_prior, hill_prior, top_prior, bottom_prior)
  return(priors)
}
