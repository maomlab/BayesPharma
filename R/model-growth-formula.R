#' Create a Richards growth formula for the brms model
#'
#' @description set-up a Richards growth model formula to define
#'     a non-linear model or multilevel non-linear model for
#'     \code{K}, \code{K0}, \code{rate}, \code{lambda}, and \code{nu} for
#'     use in \code{growth_richards_model} and in the BayesPharma
#'     package.
#'
#'     This parameterization follows (Zwietering, 1990) and grofit,
#'
#' @param treatment_variable character variable representing time as a treatment
#'     (Default: 'time')
#' @param treatment_units character the units of the time variable. (Default:
#'     "seconds")
#' @param response_variable character variable representing the response to
#'     treatment. (Default: 'response')
#' @param response_units character the units of the response. (Default: NULL)
#' @param predictors Additional formula objects to specify predictors
#'     of non-linear parameters. i.e. what perturbations/experimental
#'     differences should be modeled separately? (Default: 1) should a
#'     random effect be taken into consideration? i.e. cell number,
#'     plate number, etc.
#' @param ... additional arguments to \code{brms::brmsformula}
#'
#' @returns a \code{bpformula}, which is a subclass of
#'     \code{brms::brmsformula} and can be passed to
#'     \code{BayesPharma::growth_richards_model}.
#' @seealso
#'     [brms::brmsformula()], which this function wraps.
#'     [BayesPharma::growth_richards_model()] into which the result of this
#'     function can be passed.
#'
#' @examples
#'\dontrun{
#'   # Data has a string column drug_id with drug identifiers
#'   # Fit a separate model for each drug
#'   BayesPharma::growth_richards_formula(predictors = 0 + drug_id)
#'
#'   # Data has a string column plate_id with plate identifiers
#'   # Estimate the change in response for each plate relative to a global
#'   # baseline.
#'   BayesPharma::growth_richards_formula(predictors = plate_id)
#'
#'   # data has columns drug_id and plate_id
#'   # fit a multilevel model where the drug effect depends on the plate
#'   BayesPharma::growth_richards_formula(predictors = 0 + (drug_id|plate_id))
#'}
#'
#'@export
growth_richards_formula <- function(
    treatment_variable = "time",
    treatment_units = "hours",
    response_variable = "response",
    response_units = NULL,
    predictors = 1,
    ...) {

  # The growth_richards function is defined in
  # BayesPharma::growth_richards_stanvar
  response_eq <- as.formula(
    paste0(
      response_variable, " ~ ",
      "growth_richards(K, K0, rate, lambda, nu, ", treatment_variable, ")"))

  predictor_eq <- rlang::new_formula(
    lhs = quote(K + K0 + rate + lambda + nu),
    rhs = rlang::enexpr(predictors))

  # The growth_richards function is defined in
  # BayesPharma::growth_richards_stanvar
  model_formula <- brms::brmsformula(
    response_eq,
    predictor_eq,
    nl = TRUE,
    ...)

  model_formula$bayes_pharma_info <- list(
    formula_type = "growth_richards",
    treatment_variable = treatment_variable,
    treatment_units = treatment_units,
    response_variable = response_variable,
    response_units = response_units)

  class(model_formula) <- c("bpformula", class(model_formula))
  model_formula
}
