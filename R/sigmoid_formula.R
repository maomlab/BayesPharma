
#' Sigmoid function
#'
#' @export


sigmoid <- Vectorize(function(ec50, hill, top, bottom, log_dose){
  bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))
})
