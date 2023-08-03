#' Formula for a Curve with a Constant Response
#'
#' @description
#'   Set-up an equation for formula objects with a constant response.
#'
#' @param response_variable `character` variable representing the response to
#'   treatment
#' @param response_units character the units of the response
#' @param predictors Additional formula objects to specify predictors of
#'   non-linear parameters. i.e. what perturbations/experimental differences
#'   should be modeled separately? (Default: 1) should a random effect be taken
#'   into consideration? i.e. cell number, plate number, etc.
#' @param ... additional arguments to [brms::brmsformula()]
#'
#' @returns bpformula [brms::brmsformula()]
#'
#' @examples
#'\dontrun{
#'   constant_formula(predictors = 0 + predictors)
#'}
#' @export
constant_formula <- function(
  response_variable = "response",
  response_units = NULL,
  predictors = 1,
  ...) {

  constant_eq <- rlang::new_formula(
    lhs = rlang::ensym(response_variable),
    rhs = rlang::enexpr(predictors))

  model_formula <- brms::brmsformula(constant_eq, ...)
  class(model_formula) <- c("bpformula", class(model_formula))
  model_formula
}
