#' Create initialization for the agonist sigmoid model
#'
#' @description Creating initial values for an agonist sigmoid model parameters
#'   that can be passed to the `sigmoid_agoninst_model`.
#'
#' @param ic50 numeric or numeric returning function units log_dose
#'   (default = -9)
#' @param hill numeric or numeric returning function with units
#'   response/log_dose (default = 1)
#' @param top numeric units of top (default = 100).
#' @param bottom numeric units of bottom (default = 0).
#' @return input for brms::brm(init = ...)
#'
#' @examples
#'\dontrun{
#' #Consider an activator that has a max response around 50%, EC50 is estimated
#' #to be around 1 uM, minimum response is known to be 0.
#' init <- BayesPharma::sigmoid_agonist_init(
#'   ec50 = -6,
#'   top = 50)
#'}
#'@export
sigmoid_agonist_init <- function(
    ec50 = -9,
    hill = 1,
    top = 100,
    bottom = 0) {

  function() {
    list(
      b_ec50 = prepare_init(ec50),
      b_hill = prepare_init(hill),
      b_top = prepare_init(top),
      b_bottom = prepare_init(bottom))
  }
}

#' Create initialization for the antagonist sigmoid model
#'
#' @description Creating initial values for the sigmoid model parameters that
#'   can be passed to the `sigmoid_antagonist_model`
#'
#' @param ic50 numeric or numeric returning function units log_dose
#'   (default = -9)
#' @param hill numeric or numeric returning function with units
#'   response/log_dose (default = -1)
#' @param top numeric units of top (default = 100).
#' @param bottom numeric units of bottom (default = 0).
#' @return input for brms::brm(init = ...)
#'
#' @examples
#'\dontrun{
#' #Consider an inhibitor that has a min response around 50%, IC50 is estimated
#' #to be around 1 uM, maximum response is known to be 1000,
#' init <- BayesPharma::sigmoid_antagonist_init(
#'   ec50 = -6,
#'   bottom = 50)
#'}
#'@export
sigmoid_antagonist_init <- function(
    ic50 = -9,
    hill = -1,
    top = 100,
    bottom = 0) {
  
  function() {
    list(
      b_ic50 = prepare_init(ic50),
      b_hill = prepare_init(hill),
      b_top = prepare_init(top),
      b_bottom = prepare_init(bottom))
  }
}
