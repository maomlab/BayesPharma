#' Initialize Parameter Values for the tQ Enzyme Kinetic Model
#'
#' @param kcat `numeric` initial value for `kcat` parameter in the tQ model
#' @param kM `numeric` initial value for the `kM` parameter in the tQ model
#' @param ... additional parameter initialization. Each named argument should be
#'   a function that returns a `numeric` or `array` depending on the dimension
#'   of the parameter, see [rstan_default_init()] to use the \pkg{rstan} default
#'   init.
#'
#' @returns input for `[tQ_model](init = ...)` parameter.
#'
#' @seealso [tQ_model], [tQ_formula], and [tQ_prior]
#'
#' @export
tQ_init <- function(
    kcat = 4,
    kM = 4,
    ...) {

  function() {
    list(
      b_kcat = prepare_init(kcat),
      b_kM = prepare_init(kM),
      ...)
  }
}
