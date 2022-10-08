#' Initialize parameter values for the the tQ enyzme kinetic model
#' @export
tQ_init <- function(
    kcat = 4,
    kM = 4,
    chains = 4) {

  init_list <- list(
    kcat = kcat,
    kM = kM)

  inits <- list()

  i <- 0
  while (i < chains) {
    inits <- append(inits, list(init_list))
    i <- i + 1
  }

  return(inits)
}
