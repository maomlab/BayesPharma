#'A list of pre-determined initial values for the parameters
#'
#'@param ec50 TRUE/FALSE value. If TRUE, an initial value will be added to the
#' list and is to be predicted by the model and ec50 = -9. (default = TRUE)
#'@param hill TRUE/FALSE value. If TRUE, an initial value will be added to the
#' list and is to be predicted by the model and hill = 1 if agonist = TRUE or
#' hill = -1 if agonist = FALSE, (default = TRUE)
#'@param agonist TRUE/FALSE value that determines if hill is a positive or
#' negative slope. If TRUE, the initial condition will be hill = 1.
#' (default = TRUE)
#'@param top TRUE/FALSE value. If TRUE, an initial value will be added to the
#' list and is to be predicted by the model and top = 100, (default = TRUE)
#'@param bottom TRUE/FALSE value. If TRUE, an initial value will be added to the
#' list and is to be predicted by the model and bottom = 0, (default = TRUE)
#'@return list
#'
#'@export

default_inits <- function(ec50 = TRUE,
                          hill = TRUE,
                          agonist = TRUE,
                          top = TRUE,
                          bottom = TRUE,
                          chains = 4) {

  if (ec50 == TRUE){
    ec50_init <-  -9
  } else {
    ec50_init <- NULL
    print("ec50 is a fixed parameter. If not, set ec50 = TRUE.")
  }
  if (hill == TRUE && agonist == TRUE){
    hill_init <- 1
    print("hill is a positive slope.")
  } else if (hill == TRUE && agonist == FALSE) {
    print("hill is a negative slope.")
    hill_init <- -1
  } else{
    hill_init <- NULL
    print("hill is a fixed parameter. If not, set hill = TRUE.")
  }
  if (top == TRUE){
    top_init <- 100
  } else {
    top_init <- NULL
    print("top is a fixed parameter. If not, set top = TRUE.")
  }
  if (bottom == TRUE){
    bottom_init <- 0
  } else {
    bottom_init <- NULL
    print("bottom is a fixed parameter. If not, set bottom = TRUE.")
  }
  init_list <- list(ec50 = ec50_init,
                    hill = hill_init,
                    top = top_init,
                    bottom = bottom_init)
  # inits <- list()
  inits <- list(init_list, init_list, init_list, init_list)
  # i = 0
  # while( i <= 4){
  #   inits <- list(inits, init_list)
  #   i = i + 1
  # }
  return(inits)

}
