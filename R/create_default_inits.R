#'A list of pre-determined initial values for the parameters
#'
#'@param ec50 numeric value. Initial value for ec50 used in brmsfit.
#'(default = -9)
#'@param hill TRUE, FALSE, or numeric value, TRUE/FALSE sets hill to an
#'initial value of 1/-1 or a numeric value can be assigned to hill. Initial
#'value for hill used in brmsfit.(default = TRUE)
#'@param agonist TRUE/FALSE value that determines if hill is a positive or
#' negative slope. If TRUE, the initial condition will be hill = 1. If FALSE,
#' hill = -1. (default = TRUE)
#'@param top numeric value. Initial value for top used in brmsfit.
#'(default = 100)
#'@param bottom numeric value. Initial value for bottom used in brmsfit.
#'(default = 0)
#'@param chain the number of chains that will be used for in brmsfit.
#'(default = 4)
#'@return list of lists
#'
#'@export

dr_inits <- function(ec50 = -9,
                     hill = TRUE,
                     agonist = TRUE,
                     top = 100,
                     bottom = 0,
                     chains = 4) {

  ec50_init <- ec50

  if (hill == TRUE && agonist == TRUE){
    hill_init <- 1
    print("hill is a positive slope.")
  } else if (hill == TRUE && agonist == FALSE) {
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
  # inits <- list(init_list, init_list, init_list, init_list)
  i = 0
  while( i < chains){
    inits <- append(inits, list(init_list))
    i = i + 1
    }

  return(inits)

}
