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
  init_list <- list()
  if (ec50 == TRUE){
    init_list <- list(init_list, ec50 = -9)
  } else {
    print("ec50 is a fixed parameter. If not, set ec50 = TRUE.")
  }
  if (hill == TRUE && agonist == TRUE){
    init_list <- list(init_list, hill = 1)
    print("hill is a positive slope.")
  } else if (hill == TRUE && agonist == FALSE) {
    print("hill is anegative slope.")
    init_list <- list(init_list, hill = -1)
  } else{
      print("hill is a fixed parameter. If not, set hill = TRUE.")
  }
  if (top == TRUE){
    init_list <- list(init_list, top = 100)
  } else {
    print("top is a fixed parameter. If not, set top = TRUE.")
  }
  if (bottom == TRUE){
    init_list <- list(init_list, bottom = 0)
  } else {
    print("bottom is a fixed parameter. If not, set bottom = TRUE.")
  }

  inits <- list()
  for( i in chains){
    inits <- list(inits, inits_list)
  }

}
