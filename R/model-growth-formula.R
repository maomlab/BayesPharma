#' Create a Formula for the Sigmoid Growth Model
#'
#' @description Set-up a sigmoid growth model formula to for use in
#'   [growth_sigmoid_model()]. The functional form is
#'
#'     response ~ growth_sigmoid(K, K0, rate, lambda, time)
#'
#'   The parameterization follows (Zwietering, 1990) and \pkg{grofit}:
#'
#'     K      = **carrying capacity**, `K = response(time = Inf)`. The
#'              \pkg{grofit} package calls this parameter `A`. `K` has the same
#'              units as the `response`.
#'     K0     = **initial population size** `K0 = response(time = 0)`. The
#'              \pkg{grofit} package assumes `K0=0`. `K0` has the same units as
#'              the `response`.
#'     rate   = **maximum growth rate** `rate = max[d(response)/d(time)]`. The
#'              \pkg{grofit} package calls this `mu`. `rate` has the units of
#'              `response/time`
#'     lambda = **duration of the lag-phase** the time point at which the
#'              tangent through the growth curve when it achieves the maximum
#'              growth rate crosses the initial population size `K0`. (see
#'              Figure 2 in (Kahm et al., 2010)).
#'
#' See the vignettes(topic = "derive_growth_model", package = "BayesPharma") for
#' more details.
#'
#' @param treatment_variable `character` variable representing time as a
#'   treatment
#' @param treatment_units `character` the units of the time variable
#' @param response_variable `character` variable representing the response to
#'   treatment
#' @param response_units `character` the units of the response
#' @param predictors Additional formula objects to specify predictors
#'   of non-linear parameters. i.e. what perturbations/experimental
#'   differences should be modeled separately? (Default: 1) should a
#'   random effect be taken into consideration? i.e. cell number,
#'   plate number, etc.
#' @param ... additional arguments to [brms::brmsformula()]
#'
#' @returns a `bpformula`, which is a subclass of
#'   [brms::brmsformula] and can be passed to
#'   [growth_sigmoid_model()].
#'
#' @seealso
#'     [brms::brmsformula()], which this function wraps.
#'     [growth_sigmoid_model]()] into which the result of this
#'     function can be passed. Related to [grofit::logistic]
#'
#' @examples
#' \dontrun{
#'   # Data has a string column drug_id with drug identifiers
#'   # Fit a separate model for each drug
#'   BayesPharma::growth_sigmoid_formula(predictors = 0 + drug_id)
#'
#'   # Data has a string column plate_id with plate identifiers
#'   # Estimate the change in response for each plate relative to a global
#'   # baseline.
#'   BayesPharma::growth_sigmoid_formula(predictors = plate_id)
#'
#'   # data has columns drug_id and plate_id
#'   # fit a multilevel model where the drug effect depends on the plate
#'   BayesPharma::growth_sigmoid_formula(predictors = 0 + (drug_id|plate_id))
#' }
#'
#' @references
#' Zwietering M. H., Jongenburger I., Rombouts F. M., van 't Riet K., (1990)
#' Modeling of the Bacterial Growth Curve. Appl. Environ. Microbiol., 56(6),
#' 1875-1881 https://doi.org/10.1128/aem.56.6.1875-1881.1990
#'
#' Kahm, M., Hasenbrink, G., Lichtenberg-Fraté, H., Ludwig, J., & Kschischo, M.
#' (2010). grofit: Fitting Biological Growth Curves with R. J. Stat. Softw.,
#' 33(7), 1–21. https://doi.org/10.18637/jss.v033.i07
#'
#'@export
growth_sigmoid_formula <- function(
  treatment_variable = "time",
  treatment_units = "hours",
  response_variable = "response",
  response_units = NULL,
  predictors = 1,
  ...) {

  # The growth_sigmoid function is defined in
  # BayesPharma::growth_sigmoid_stanvar
  response_eq <- stats::as.formula(
    paste0(
      response_variable, " ~ ",
      "growth_sigmoid(K, K0, rate, lambda, ", treatment_variable, ")"))

  predictor_eq <- rlang::new_formula(
    lhs = quote(K + K0 + rate + lambda),
    rhs = rlang::enexpr(predictors))

  model_formula <- brms::brmsformula(
    response_eq,
    predictor_eq,
    nl = TRUE,
    ...)

  model_formula$bayes_pharma_info <- list(
    formula_type = "growth_sigmoid",
    treatment_variable = treatment_variable,
    treatment_units = treatment_units,
    response_variable = response_variable,
    response_units = response_units)

  class(model_formula) <- c("bpformula", class(model_formula))
  model_formula
}



#' Create a Formula for the Richards Growth Model
#'
#' @description set-up a Richards growth model formula to for use in
#'   `growth_richards_model` and in the [BayesPharma] package. The functional
#'   form is
#'
#'     response ~ richards_growth(K, K0, rate, lambda, nu, time)
#'
#'   The parameterization follows (Zwietering, 1990) and \pkg{grofit}:
#'
#'
#'     K      = **carrying capacity**, `K = response(time = Inf)`. The
#'              \pkg{grofit} package calls this parameter `A`. `K` has the same
#'              units as the `response`.
#'     K0     = **initial population size** `K0 = response(time = 0)`. The
#'              \pkg{grofit} package assumes `K0=0`. `K0` has the same units as
#'              the `response`.
#'     rate   = **maximum growth rate** `rate = max[d(response)/d(time)]`. The
#'              \pkg{grofit} package calls this `mu`. `rate` has the units of
#'              `response/time`
#'     lambda = **duration of the lag-phase** the time point at which the
#'              tangent through the growth curve when it achieves the maximum
#'              growth rate crosses the initial population size `K0`. (see
#'              Figure 2 in (Kahm et al., 2010)).
#'     nu     = **growth asymmetry** before and after the inflection
#'              point.
#'
#' See the vignettes(topic = "derive_growth_model", package = "BayesPharma") for
#' more details.
#'
#' @param treatment_variable `character` variable representing time as a
#'   treatment
#' @param treatment_units `character` the units of the time variable
#' @param response_variable `character` variable representing the response to
#'   treatment
#' @param response_units `character` the units of the response
#' @param predictors Additional formula objects to specify predictors
#'   of non-linear parameters. i.e. what perturbations/experimental differences
#'   should be modeled separately? (Default: 1) should a random effect be taken
#'   into consideration? i.e. cell number, plate number, etc.
#' @param ... additional arguments to [brms::brmsformula()]
#'
#' @returns a `bpformula`, which is a subclass of
#'   [brms::brmsformula()] and can be passed to
#'   [growth_richards_model()].
#' @seealso
#'   [brms::brmsformula()], which this function wraps.
#'   [growth_richards_model()] into which the result of this
#'   function can be passed.
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
#' @references
#' Zwietering M. H., Jongenburger I., Rombouts F. M., van 't Riet K., (1990)
#' Modeling of the Bacterial Growth Curve. Appl. Environ. Microbiol., 56(6),
#' 1875-1881 https://doi.org/10.1128/aem.56.6.1875-1881.1990
#'
#' Kahm, M., Hasenbrink, G., Lichtenberg-Fraté, H., Ludwig, J., & Kschischo, M.
#' (2010). grofit: Fitting Biological Growth Curves with R. J. Stat. Softw.,
#' 33(7), 1–21. https://doi.org/10.18637/jss.v033.i07
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
  response_eq <- stats::as.formula(
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
