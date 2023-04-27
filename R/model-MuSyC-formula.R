#' Create a formula for the MuSyC synergy model
#'
#' @description setup a default `MuSyC` synergy model formula to
#'     predict the `E0`, `C1`, `E1`, `s1`,
#'     `C2`, `E2`, `s2`, `log10alpha`, and
#'     `E3alpha` parameters.
#'
#' @param treatment_1_variable character variable representing the treatment.
#'     (Default: 'logd1')
#' @param treatment_1_units character the units of the treatment. The default is
#'     log base 10 of the molar concentration. (Default: "Log[Molar]")
#' @param treatment_2_variable character variable representing the treatment.
#'     (Default: 'logd2')
#' @param treatment_1_units character the units of the treatment. The default is
#'     log base 10 of the molar concentration. (Default: "Log[Molar]")
#' @param response_variable character variable representing the response to
#'     treatment. (Default: 'response')
#' @param response_units character the units of the response. (Default: NULL)
#' @param predictors Additional formula objects to specify predictors
#'     of non-linear parameters. i.e. what perturbations/experimental
#'     differences should be modeled separately? (Default: 1) should a
#'     random effect be taken into consideration? i.e. cell number,
#'     plate number, etc.
#' @param ... additional arguments passed to [brms::brmsformula()]
#'
#' @returns [brms::brmsformula()]
#'
#' @examples
#'\dontrun{
#'   # Data has a string column drug_id with drug identifiers
#'   # Fit a separate model for each drug
#'   BayesPharma::MuSyC_formula(predictors = 0 + drug_id)
#'
#'   # Data has a string column plate_id with plate identifiers
#'   # Estimate the change in response for each plate relative to a global
#'   # baseline.
#'   BayesPharma::MuSyC_formula(predictors = plate_id)
#'
#'   # data has columns drug_id and plate_id
#'   # fit a multilevel model where the drug effect depends on the plate
#'   BayesPharma::MuSyC_formula(predictors = 0 + (drug_id|plate_id))
#'}
#'
#' @export
MuSyC_formula <- function(
    treatment_1_variable = "logd1",
    treatment_1_units = "Log[Molar]",
    treatment_2_variable = "logd2",
    treatment_2_units = "Log[Molar]",
    response_variable = "response",
    response_units = NULL,
    predictors = 1,
    ...) {

  response_eq <- as.formula(
    paste0(
      response_variable, " ~ ",
      "MuSyC(",
      treatment_1_variable, " - logd1scale, ",
      treatment_2_variable, " - logd2scale, ",
      "logE0, ",
      "logC1, logE1, h1, ",
      "logC2, logE2, h2, ",
      "logE3, logalpha)"))


  predictor_eq <- rlang::new_formula(
    lhs = quote(
      logE0 +
      logC1 + logE1 + h1 +
      logC2 + logE2 + h2 +
      logE3 + logalpha),
    rhs = rlang::enexpr(predictors))

  model_formula <- brms::brmsformula(
    response_eq,
    predictor_eq,
    nl = TRUE,
    ...)

  model_formula$bayes_pharma_info <- list(
    formula_type = "MuSyC",
    treatment_1_variable = treatment_1_variable,
    treatment_1_units = treatment_1_units,
    treatment_2_variable = treatment_2_variable,
    treatment_2_units = treatment_2_units,
    response_variable = response_variable,
    response_units = response_units)

  class(model_formula) <- c("bpformula", class(model_formula))
  model_formula

}
