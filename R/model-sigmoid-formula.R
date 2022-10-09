#' Create a sigmoid formula for the brms model
#'
#' @description set-up a sigmoid dose response model formula to define a
#'   non-linear model or multilevel non-linear model for `ec50`, `hill`, `top`,
#'   and, `bottom` for use in Bayesian_model and in the BRMS package.
#'
#' @param predictors Additional formula objects to specify predictors of
#'   non-linear parameters. i.e. what perturbations/experimental differences
#'   should be modeled separately? (Default: 1) should a random effect be taken
#'   into consideration? i.e. cell number, plate number, etc.
#' @param ... additional arguments to `brms::brmsformula`
#' 
#' @return brmsformula
#'
#' @examples
#'\dontrun{
#'   # Data has a string column `drug_id` with drug identifiers
#'   # Fit a separate model for each drug
#'   BayesPharma::sigmoid_formula(predictors = 0 + drug_id)
#'
#'   # Data has a string column `plate_id` with plate identifiers
#'   # Estimate the change in response for each plate relative to a global
#'   # baseline.
#'   BayesPharma::sigmoid_formula(predictors = plate_id)
#'
#'   # data has columns  `drug_id` and `plate_id`
#'   # fit a multilevel model where the drug effect depends on the plate
#'   BayesPharma::sigmoid_formula(predictors = 0 + (drug_id|plate_id))
#'}
#'
#'@export
sigmoid_formula <- function(
    predictors = 1,
    ...) {

  predictor_eq <- rlang::new_formula(
    lhs = quote(ec50 + hill + top + bottom),
    rhs = rlang::enexpr(predictors))

  # The sigmoid function is defined in BayesPharma::sigmoid_stanvar
  sigmoid_formula <- brms::brmsformula(
    response ~ sigmoid(ec50, hill, top, bottom, log_dose),
    predictor_eq,
    nl = TRUE,
    ...)

  return(sigmoid_formula)
}
