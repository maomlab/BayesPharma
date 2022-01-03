#' Create a list of initial values of the model parameters
#'
#' @description Creating a list of lists containing initial values for the model parameters
#'   that will be used as an argument in the dr_model.
#'
#' @usage
#'
#' @param ec50 numeric unit of ec50 (default = -9)
#' @param hill TRUE, FALSE, or numeric value, TRUE/FALSE sets hill to an
#'   initial value of 1/-1 or a numeric value can be assigned to hill.
#'   numeric unit of hill (default = TRUE)
#' @param inhibitor TRUE/FALSE value that determines if hill is a positive or
#'   negative slope. If TRUE, the initial condition will be hill = -1. If FALSE,
#'   hill = 1 (default = TRUE)
#' @param top numeric unit of top (default = 100)
#' @param bottom numeric unit of bottom (default = 0)
#' @param chain the number of Markov chains that will be used in brmsfit.(default = 4)
#' @return list of lists
#'
#'@export

dr_inits <- function(ec50 = -9,
                     hill = TRUE,
                     inhibitor = TRUE,
                     top = 100,
                     bottom = 0,
                     chains = 4) {

  ec50_init <- ec50

  if (hill == TRUE && inhibitor == FALSE) {
    hill_init <- 1
    print("hill is a positive slope.")
  } else if (hill == TRUE && inhibitor == TRUE) {
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

  inits <- list()

  i <- 0
  while (i < chains) {
    inits <- append(inits, list(init_list))
    i <- i + 1
    }

  return(inits)

}
