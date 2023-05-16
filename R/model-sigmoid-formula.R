#' Create an Agonist Sigmoid Formula for the Sigmoid Model
#'
#' @description set-up a sigmoid dose response model formula to define a
#'   non-linear model or multilevel non-linear model for `ec50`, `hill`, `top`,
#'   and, `bottom` for use in the [sigmoid_model] .
#'
#' @param treatment_variable `character` variable representing the treatment.
#' @param treatment_units `character` the units of the treatment. The default is
#'   log base 10 of the molar concentration.
#' @param response_variable `character` variable representing the response to
#'   treatment.
#' @param response_units `character` the units of the response.
#' @param predictors `character` or expression of predictors
#'   of non-linear parameters. i.e. what perturbations/experimental
#'   differences should be modeled separately?
#' @param ... additional arguments to [brms::brmsformula()]
#'
#' @returns a `bpformula`, which is a subclass of [brms::brmsformula] and can be
#'   passed to [sigmoid_model()].
#'
#' @seealso [brms::brmsformula], which this function wraps. [sigmoid_model()]
#'   into which the result of this function can be passed.
#'
#' @examples
#'\dontrun{
#'   # Data has a string column drug_id with drug identifiers
#'   # Fit a separate model for each drug
#'   BayesPharma::sigmoid_agonist_formula(predictors = 0 + drug_id)
#'
#'   # Data has a string column plate_id with plate identifiers
#'   # Estimate the change in response for each plate relative to a global
#'   # baseline.
#'   BayesPharma::sigmoid_agonist_formula(predictors = plate_id)
#'
#'   # data has columns drug_id and plate_id
#'   # fit a multilevel model where the drug effect depends on the plate
#'   BayesPharma::sigmoid_agonist_formula(predictors = 0 + (drug_id|plate_id))
#'}
#'
#'@export
sigmoid_agonist_formula <- function(
    treatment_variable = "log_dose",
    treatment_units = "Log[Molar]",
    response_variable = "response",
    response_units = NULL,
    predictors = 1,
    ...) {
  # The sigmoid function is defined in BayesPharma::sigmoid_stanvar
  response_eq <- stats::as.formula(
    paste0(
      response_variable, " ~ ",
      "sigmoid(ec50, hill, top, bottom, ", treatment_variable, ")"))

  predictor_eq <- rlang::new_formula(
    lhs = quote(ec50 + hill + top + bottom),
    rhs = rlang::enexpr(predictors))

  model_formula <- brms::brmsformula(
    response_eq,
    predictor_eq,
    nl = TRUE,
    ...)

  model_formula$bayes_pharma_info <- list(
    formula_type = "sigmoid_agonist",
    treatment_variable = treatment_variable,
    treatment_units = treatment_units,
    response_variable = response_variable,
    response_units = response_units)

  class(model_formula) <- c("bpformula", class(model_formula))
  model_formula
}

#' Create a Formula for the Sigmoid Antagonist Model
#'
#' @description set-up an antagonist sigmoid dose response model formula to
#'   define a non-linear model or multilevel non-linear model for `ic50`,
#'   `hill`, `top`, and, `bottom` for use in the [sigmoid_model()]
#'
#' @param treatment_variable `character` variable representing the treatment.
#' @param treatment_units `character` the units of the treatment. The default is
#'   log base 10 of the molar concentration.
#' @param response_variable `character` variable representing the response to
#'   treatment.
#' @param response_units `character` the units of the response.
#' @param predictors `character` or expression of predictors of non-linear
#'   parameters. i.e. what perturbations/experimental differences should be
#'   modeled separately?
#' @param ... additional arguments to [brms::brmsformula()]
#'
#' @returns a `bpformula`, which is a subclass of [brms::brmsformula] and can
#'   be passed to [sigmoid_model()].
#'
#' @seealso [brms::brmsformula], which this function wraps. [sigmoid_model()]
#'   into which the result of this function can be passed.
#' @examples
#'\dontrun{
#'   # Data has a string column drug_id with drug identifiers
#'   # Fit a separate model for each drug
#'   BayesPharma::sigmoid_antagonist_formula(predictors = 0 + drug_id)
#'
#'   # Data has a string column plate_id with plate identifiers
#'   # Estimate the change in response for each plate relative to a global
#'   # baseline.
#'   BayesPharma::sigmoid_antagonist_formula(predictors = plate_id)
#'
#'   # data has columns drug_id and plate_id
#'   # fit a multilevel model where the drug effect depends on the plate
#'   BayesPharma::sigmoid_antagonist_formula(
#'     predictors = 0 + (drug_id|plate_id))
#'}
#'
#'@export
sigmoid_antagonist_formula <- function(
    treatment_variable = "log_dose",
    treatment_units = "Log[Molar]",
    response_variable = "response",
    response_units = NULL,
    predictors = 1,
    ...) {

  # The sigmoid function is defined in BayesPharma::sigmoid_stanvar
  response_eq <- stats::as.formula(
    paste0(
      response_variable, " ~ ",
      "sigmoid(ic50, hill, top, bottom, ", treatment_variable, ")"))

  predictor_eq <- rlang::new_formula(
    lhs = quote(ic50 + hill + top + bottom),
    rhs = rlang::enexpr(predictors))

  model_formula <- brms::brmsformula(
    response_eq,
    predictor_eq,
    nl = TRUE,
    ...)

  model_formula$bayes_pharma_info <- list(
    formula_type = "sigmoid_antagonist",
    treatment_variable = treatment_variable,
    treatment_units = treatment_units,
    response_variable = response_variable,
    response_units = response_units)

  class(model_formula) <- c("bpformula", class(model_formula))
  model_formula
}
