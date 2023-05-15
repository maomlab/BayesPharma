#' Initialize Parameter Values for the tQ Enzyme Kinetic Model
#'
#' @param kcat `numeric` initial value for `kcat` parameter in the tQ model
#' @param kM `numeric` initial value for the `kM` parameter in the tQ model
#' @returns input for `[tQ_model](init = ...)` parameter.
#'
#' @seealso [tQ_model], [tQ_formula], and [tQ_prior]
#'
#' @export
tQ_init <- function(
    kcat = 4,
    kM = 4) {

  function() {
    list(
      b_kcat = prepare_init(kcat),
      b_kM = prepare_init(kM))
  }
}
