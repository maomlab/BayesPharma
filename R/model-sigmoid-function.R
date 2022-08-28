#' Sigmoid Function
#'
#' @description A vectorized function of the sigmoid dose-response curve
#'   equation. This may be need if stan backend is cmdstanr
#'
#' @param ec50 numeric.
#' @param hill numeric.
#' @param top numeric.
#' @param bottom numeric.
#' @param log_dose numeric.
#' @return numeric.
#'
#' @export


sigmoid <- Vectorize(
  function(ec50, hill, top, bottom, log_dose) {
    bottom + (top - bottom) / (1 + 10^((ec50 - log_dose) * hill))
})
