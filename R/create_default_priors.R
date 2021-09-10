
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

default_priors <- function(ec50 = TRUE,
                           hill = TRUE,
                           agonist = TRUE,
                           top = TRUE,
                           bottom = TRUE) {


  if (ec50 == TRUE) {
    ec50_prior <- brms::prior(normal(-7,5), nlpar = 'ec50')

  } else {
    ec50_prior <- NULL
    print("ec50 is a constant or ec50 does not equal 'ec50' or 'ic50' ")

  }
  if (hill == TRUE && agonist == TRUE) {
    hill_prior <- brms::prior(normal(1,2),
                              nlpar = 'hill',
                              lb = -0.01)

  } else if (hill == TRUE && agonist == FALSE) {
    hill_prior <- c(brms::prior(normal(-1,2),
                                        nlpar = 'hill',
                                        ub = 0.01))

  } else {
    hill_prior <- NULL
    print("hill is a fixed value, hill does not equal 'hill',
          or agonist and inhibitor are not equal to TRUE or FALSE")
  }

  if (top == TRUE) {
    top_prior <- brms::prior(normal(100,25), nlpar = "top")

  } else {
    top_prior <- NULL
    print("top is fixed value or top does not equal 'top' ")
  }

  if (bottom == TRUE) {
    bottom_prior <- brms::prior(normal(0,25), nlpar = "bottom")

  } else {
    bottom_prior <- NULL
    print("bottom is fixed value or bottom does not equal 'bottom' ")
  }

  priors <- c(ec50_prior, hill_prior, top_prior, bottom_prior)
  return(priors)
}
