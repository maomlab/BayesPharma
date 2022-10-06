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
#' @return brmsformula
#'
#' @examples
#'\dontrun{
#'   Consider observations made using 4 different drugs and the column header
#'   containing the labels for the 4 different drugs is `predictors`.
#'   dr_formula(predictors = 0 + predictors)
#'
#'   Consider that the cell_ID was recorded and the noise from the cell_ID is to
#'   be accounted for. dr_formula(predictors = 0 + predictors + (1|cell_ID))
#'}
#'
#'@export
dr_formula <- function(
    predictors = 1,
    ...) {
  
  predictor_eq <- rlang::new_formula(
    lhs = quote(ec50 + hill + top + bottom),
    rhs = rlang::enexpr(predictors))
  
  sigmoid_formula <- brms::brmsformula(
    response ~ sigmoid(ec50, hill, top, bottom, log_dose),
    predictor_eq,
    nl = TRUE,
    ...)
  
  return(sigmoid_formula)
}