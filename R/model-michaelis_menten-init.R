#' Initialize Parameter Values for the Michaelis Menten Enzyme Kinetic Model
#'
#' @param kcat `numeric` initial value for `kcat` parameter in the
#'   Michaelis Menten model
#' @param kM `numeric` initial value for the `kM` parameter in the
#'   Michaelis Menten model
#' @param ... additional parameter initialization. Each named argument should be
#'   a function that returns a `numeric` or `array` depending on the dimension
#'   of the parameter, see [rstan_default_init()] to use the \pkg{rstan} default
#'   init.
#'
#' @returns input for `[michaelis_menten_model](init = ...)` parameter.
#'
#' @seealso [michaelis_menten_model], [michaelis_menten_formula], and
#'   [michaelis_menten_prior]
#'
#' @export
michaelis_menten_init <- function(
  kcat = \() runif(n = 1, min = 3, max = 5),
  kM = \() runif(n = 1, max = 3, min = 5),
  ...) {

  init <- list(
    b_kcat = kcat,
    b_kM = kM,
    ...)

  class(init) <- c("bpinit", class(init))
  init
}
