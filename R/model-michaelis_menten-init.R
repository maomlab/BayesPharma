#' Initialize Parameter Values for the Michaeli Menten Enzyme Kinetic Model
#'
#' @param kcat `numeric` initial value for `kcat` parameter in the
#'   Michaelis Menten model
#' @param kM `numeric` initial value for the `kM` parameter in the
#'   Michaelis Menten model
#' @returns input for `[michaelis_menten_model](init = ...)` parameter.
#'
#' @seealso [michaelis_menten_model], [michaelis_menten_formula], and
#'   [michaelis_menten_prior]
#'
#' @export
michaelis_menten_init <- function(
    kcat = 4,
    kM = 4) {

  function() {
    list(
      b_kcat = prepare_init(kcat),
      b_kM = prepare_init(kM))
  }
}
