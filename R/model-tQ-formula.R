#' Define formula for the tQ enzyme kinetics model
#'
#' @param series_index_variable `character` variable indexing which
#'   measurements are part of a common time series.
#' @param treatment_variable `character` variable representing time as the
#'   treatment.
#' @param treatment_units `character` the units of the treatment variable.
#' @param ET_variable `character` variable representing the total enzyme
#'   concentration.
#' @param ET_units `character` variable representing the units of the
#'   enzyme concentration.
#' @param ST_variable `character` variable representing the total substrate
#'   concentration.
#' @param ST_units `character` variable representing the units of the
#'   substrate concentration.
#' @param response_variable `character` variable representing the product.
#' @param response_units `character` the units of the product.
#' @param predictors `expression` of predictors for `kcat` and
#'   `kM` parameters
#' @param ... additional arguments passed to [brms::brmsformula()]
#'
#' @returns a `bpformula`, which is a subclass of
#'   [brms::brmsformula] and can be passed to
#'   [tQ_model()].
#'
#' @seealso [tQ_model], [tQ_prior], [tQ_init], [tQ_stanvar] and
#'   [brms::brmsformula], which this function wraps.
#'
#' @export
tQ_formula <- function(
    series_index_variable = "series_index",
    treatment_variable = "time",
    treatment_units = "seconds",
    ET_variable = "ET",
    ET_units = "mg/ml",
    ST_variable = "ST",
    ST_units = "mg/ml",
    response_variable = "P",
    response_units = "mg/ml",
    predictors = 1,
    ...) {

  # The tQ function is defined in BayesPharma::tQ_stanvar
  response_eq <- as.formula(
    paste0(
      response_variable, " ~ tQ(",
      series_index_variable, ", ",
      treatment_variable, ", ",
      "kcat, kM, ",
      ET_variable, ", ",
      ST_variable, ")"))

  predictor_eq <- rlang::new_formula(
    lhs = quote(kcat + kM),
    rhs = rlang::enexpr(predictors))

  model_formula <- brms::brmsformula(
    response_eq,
    predictor_eq,
    nl = TRUE,
    loop = FALSE,
    ...)

  model_formula$bayes_pharma_info <- list(
    formula_type = "tQ",
    series_index_variable = series_index_variable,
    treatment_variable = treatment_variable,
    treatment_units = treatment_units,
    ET_variable = ET_variable,
    ET_units = ET_units,
    ST_variable = ST_variable,
    ST_units = ST_units,
    response_variable = response_variable,
    response_units = response_units)

  class(model_formula) <- c("bpformula", class(model_formula))
  model_formula
}
