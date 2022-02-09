#' Create a list of initial values of the model parameters
#'
#' @description Creating a list of lists containing initial values for the model parameters
#'   that will be used as an argument in the dr_model.
#'
#' @usage
#'    dr_inits(
#'       ec50 = -9,
#'       hill = TRUE,
#'       inhibitor = TRUE,
#'       top = 100,
#'       bottom = 0,
#'       chains = 4)
#'
#' @param ec50 numeric units of ec50 (default = -9).
#' @param hill TRUE, FALSE, or numeric units, TRUE/FALSE sets hill to an
#'   initial value of 1/-1 or numeric units can be assigned to hill (default = TRUE).
#' @param inhibitor TRUE/FALSE value that determines if hill is a positive or
#'   negative slope. If TRUE, the initial condition will be hill = -1. If FALSE,
#'   hill = 1 (default = TRUE).
#' @param top numeric units of top (default = 100).
#' @param bottom numeric units of bottom (default = 0).
#' @param chain numeric units of Markov chains that will be used in brmsfit (default = 4).
#' @return list of lists.
#'
#' @examples
#' Consider an activator that has a max response around 50%, EC50 is estimated to be
#' around 1uM, minimum response is known to be 0, and 4 MCMC chains will be used by the model.
#'
#' inits <- BayesPharma::dr_inits(ec50 = -6,
#'                                hill = TRUE,
#'                                inhibitor = FALSE,
#'                                top = 50,
#'                                bottom = 0,
#'                                chains = 4)
#'
#'@export

dr_inits <- function(ec50 = -9,
                     hill = TRUE,
                     inhibitor = TRUE,
                     top = 100,
                     bottom = 0,
                     chains = 4) {

  ec50_init <- ec50

  if (inhibitor == FALSE && hill == TRUE || hill == FALSE) {
    hill_init <- 1
    print("hill is a positive slope.")
  } else if (inhibitor == TRUE && hill == TRUE || hill == FALSE) {
    print("hill is a negative slope.")
    hill_init <- -1
  } else{
    hill_init <- hill
  }

    top_init <- top

    bottom_init <- bottom

  init_list <- list(ec50 = ec50_init,
                    hill = hill_init,
                    top = top_init,
                    bottom = bottom_init)

  inits <- rep(list(init_list), chains)

  # i <- 0
  # while (i < chains) {
  #   inits <- append(inits, list(init_list))
  #   i <- i + 1
  #   }

  return(inits)

}
