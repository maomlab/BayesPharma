#' Formula for a curve with a constant response
#'
#' @description set-up an equation for formula objects with a constant response.
#'
#' @param predictors Additional formula objects to specify predictors of
#'   non-linear parameters. i.e. what perturbations/experimental differences
#'   should be modeled separately? (Default: 1) should a random effect be taken
#'   into consideration? i.e. cell number, plate number, etc.
#' @return brmsformula
#'
#' @examples
#'\dontrun{
#'   constant_formula(predictors = 0 + predictors)
#'}
#' @export
constant_formula <- function(
    predictors = 1,
    ...) {

  constant_eq <- rlang::new_formula(
    lhs = quote(response),
    rhs = rlang::enexpr(predictors))

  formula <- brms::brmsformula(constant_eq, ...)

  return(formula)
}
