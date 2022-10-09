#' Initialize parameter values for the tQ enyzme kinetic model
#' 
#' @param kcat numeric initial value for `kcat` parameter in the tQ model
#'   (Default 4)
#' @param kM numeric initial value for the `kM` parameter in the tQ model
#'   (default 4)
#' @return input for `BayesPharma::model_tQ(init = ...)` parameter.
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
