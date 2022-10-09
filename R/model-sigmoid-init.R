#' Create initialization for the sigmoid model 
#'
#' @description Creating initial values for the sigmoid model parameters that
#'   can be passed to the `sigmoid_model`
#'
#' @param ec50 numeric or numeric returning function units log_dose
#'   (default = -9)
#' @param hill TRUE, FALSE, or numeric units, TRUE/FALSE sets hill to an
#'   initial value of 1/-1 or numeric units can be assigned to hill
#'   (default = TRUE).
#' @param inhibitor TRUE/FALSE value that determines if hill is a positive or
#'   negative slope. If TRUE, the initial condition will be hill = -1. If FALSE,
#'   hill = 1 (default = TRUE).
#' @param top numeric units of top (default = 100).
#' @param bottom numeric units of bottom (default = 0).
#' @param chains numeric units of Markov chains that will be used in brmsfit
#'   (default = 4).
#' @return list of lists.
#'
#' @examples
#'\dontrun{
#' #Consider an activator that has a max response around 50%, EC50 is estimated
#' #to be around 1 uM, minimum response is known to be 0, and 4 MCMC chains will
#' #be used by the model.
#' init <- BayesPharma::sigmoid_init(
#'   ec50 = -6,
#'   inhibitor = FALSE,
#'   top = 50)
#'}
#'@export
sigmoid_init <- function(
    ec50 = -9,
    hill = TRUE,
    inhibitor = TRUE,
    top = 100,
    bottom = 0,
    chains = 4) {

  ec50_init <- ec50

  if (inhibitor == FALSE && hill == TRUE || hill == FALSE) {
    hill_init <- 1
    cat("hill is a positive slope.\n")
  } else if (inhibitor == TRUE && hill == TRUE || hill == FALSE) {
    cat("hill is a negative slope.\n")
    hill_init <- -1
  } else{
    hill_init <- hill
  }

  top_init <- top
  bottom_init <- bottom

  init_list <- list(
    b_ec50 = ec50_init,
    b_hill = hill_init,
    b_top = top_init,
    b_bottom = bottom_init)

  init <- rep(list(init_list), chains)
  return(init)
}
