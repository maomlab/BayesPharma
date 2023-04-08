#' Create initialization for the Richards growth model
#'
#' @description Creating initial values for Richards growth model
#'     parameters that can be passed to the
#'     \code{richards_growth_model}.
#'
#' @param K numeric or numeric returning function (default = 1).
#' @param K0 numeric or numeric returning function (default = 0).
#' @param rate numeric or numeric returning function (default = 1).
#' @param lambda numeric units of top (default = 0.5).
#' @param nu numeric units of bottom (default = 2).
#' @returns input for brms::brm(init = ...)
#'
#' @examples
#'\dontrun{
#' init <- BayesPharma::growth_richards_init(
#'   A = 100,
#'   nu = 24)
#'}
#'@export
growth_richards_init <- function(
    K = 1,
    K0 = 0,
    rate = 1,
    lambda = 0.5,
    nu = 2) {

  function() {
    list(
      b_K = prepare_init(K),
      b_K0 = prepare_init(K0),
      b_rate = prepare_init(rate),
      b_lambda = prepare_init(lambda),
      b_nu = prepare_init(nu))
  }
}
