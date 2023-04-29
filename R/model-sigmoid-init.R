#' Initialize Parameter Values for an Agonist Sigmoid Model
#'
#' @description Creating initial values for an agonist sigmoid model parameters
#'   that can be passed to the [sigmoid_model()] along with the
#'   [sigmoid_agonist_formula()] and [sigmoid_agonist_prior()].
#'
#' @param ec50 `numeric` or `numeric` returning `function` units of the
#'   treatment. If the treatment is `log_dose`, the default value of `-6`
#'  corresponds `1e-6` molar = `1` μM
#' @param hill `numeric` or `numeric` returning `function` with units
#'   `response/log_dose`
#' @param top `numeric` or `numeric` returning `function` with units of the
#'   response
#' @param bottom `numeric` or `numeric` returning `function` with units of the
#'   response
#' @returns input for `[brms::brm](init = ...)`
#'
#' @seealso [sigma_agonist_formula()], [sigma_agonist_prior()], and
#'   [sigmoid_model()]
#'
#' @examples
#'\dontrun{
#' #Consider an activator that has a max response around 50%, EC50 is estimated
#' #to be around 1 nM, minimum response is known to be 0.
#' init <- BayesPharma::sigmoid_agonist_init(
#'   ec50 = -9,
#'   top = 0.5)
#'}
#'@export
sigmoid_agonist_init <- function(
    ec50 = -6,
    hill = 1,
    top = 1,
    bottom = 0) {

  function() {
    list(
      b_ec50 = prepare_init(ec50),
      b_hill = prepare_init(hill),
      b_top = prepare_init(top),
      b_bottom = prepare_init(bottom))
  }
}

#' Initialize Parameter Values for an Antagonist Sigmoid Model
#'
#' @description Creating initial values for an agonist sigmoid model parameters
#'   that can be passed to the [sigmoid_model()] along with the
#'   [sigmoid_antagonist_formula()] and [sigmoid_antagonist_prior()].
#'
#' @param ic50 `numeric` or `numeric` returning `function` units of the
#'   treatment. If the treatment is `log_dose`, the default value of `-6`
#'  corresponds `1e-6` molar = `1` μM
#' @param hill `numeric` or `numeric` returning `function` with units
#'   response/log_dose
#' @param top `numeric` or `numeric` returning `function` in units of the
#'   response
#' @param bottom `numeric` or `numeric` returning `funciton` in units of the
#'   response
#' @returns input for `[brm][brms::brm](init = ...)`
#'
#' @seealso [sigma_antagonist_formula()], [sigma_antagonist_prior()], and
#'   [sigmoid_model()]
#'
#' @examples
#'\dontrun{
#' #Consider an inhibitor that has a min response around 50%, IC50 is estimated
#' #to be around 1 nM, maximum response is known to be around 1,
#' init <- BayesPharma::sigmoid_antagonist_init(
#'   ec50 = -9,
#'   bottom = 0.5)
#'}
#'@export
sigmoid_antagonist_init <- function(
    ic50 = -6,
    hill = -1,
    top = 1,
    bottom = 0) {

  function() {
    list(
      b_ic50 = prepare_init(ic50),
      b_hill = prepare_init(hill),
      b_top = prepare_init(top),
      b_bottom = prepare_init(bottom))
  }
}
