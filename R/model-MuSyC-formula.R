#' Create a formula for the MuSyC synergy model
#'
#' @description setup a defaulMuSyC synergy model formula to predict
#'   the `E0`, `C1`, `E1`, `s1`, `C2`, `E2`, `s2`, `log10alpha`, and `E3alpha`
#'   parameters.
#'
#' @param predictors Additional formula objects to specify predictors of
#'   non-linear parameters. i.e. what perturbations/experimental differences
#'   should be modeled separately? (Default: 1) should a random effect be taken
#'   into consideration? i.e. cell number, plate number, etc.
#' @param ... additional arguments passed to `brms::brmsformula`
#' 
#' @return brmsformula
#'
#' @examples
#'\dontrun{
#'   # Data has a string column `drug_id` with drug identifiers
#'   # Fit a separate model for each drug
#'   BayesPharma::MuSyC_formula(predictors = 0 + drug_id)
#'
#'   # Data has a string column `plate_id` with plate identifiers
#'   # Estimate the change in response for each plate relative to a global
#'   # baseline.
#'   BayesPharma::MuSyC_formula(predictors = plate_id)
#'
#'   # data has columns `drug_id` and `plate_id`
#'   # fit a multilevel model where the drug effect depends on the plate
#'   BayesPharma::MuSyC_formula(predictors = 0 + (drug_id|plate_id))
#'}
#'
#' @export
MuSyC_formula <- function(
    predictors = 1,
    ...) {

  if (!is.null(predictors)) {
    predictor_eq <- rlang::new_formula(
      lhs = quote(
        logE0 +
        logC1 + logE1 + h1 +
        logC2 + logE2 + h2 +
        logE3 + logalpha),
      rhs = rlang::enexpr(predictors))
  } else {
    predictor_eq <- NULL
  }

  brms::brmsformula(
    response ~ MuSyC(
      logd1 - logd1scale, logd2 - logd2scale,
      logE0,
      logC1, logE1, h1,
      logC2, logE2, h2,
      logE3, logalpha),
    predictor_eq,
    nl = TRUE,
    ...)
}
