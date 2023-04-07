#' Define formula for the tQ enzyme kinetics model
#'
#' @param series_index_variable character variable indexing which measurements
#'     are part of a common time series.
#' @param treatment_variable character variable representing the treatment.
#'     (Default: 'log_dose')
#' @param treatment_units character the units of the treatment. The default is
#'     log base 10 of the molar concentration. (Default: "Log[Molar]")
#' @param ET_variable character variable representing the enzyme concentration
#'     (Default: 'ET')
#' @param ET_units character variable representing the units of the enzyme
#'     concentration. (Default: mg/ml)
#' @param ST_variable character variable representing the substrate
#'     concentration at time 0. (Default: 'ET')
#' @param ST_units character variable representing the units of the substrate
#'     concentration. (Default: mg/ml)
#' @param response_variable character variable representing the product.
#'     (Default: 'P')
#' @param response_units character the units of the product. (Default: mg/ml)
#'
#' @param predictors predictors for \code{kcat} and \code{kM}.
#' @param ... additional arguments passed to \code{brms::brmsformula}
#'
#' @returns a \code{bpformula}, which is a subclass of
#'     \code{brms::brmsformula} and can be passed to
#'     \code{BayesPharma::tQ_model}.
#' @seealso
#'     [brms::brmsformula()], which this function wraps.
#'     [BayesPharma::tQ_model()] into which the result of this
#'     function can be passed.
#'
#' @export
tQ_formula <- function(
    series_index_variable = "series_index",
    time_variable = "time",
    time_units = "seconds",
    ET_variable = "ET",
    ET_units = "mg/ml",
    ST_variable = "ST",
    ST_units = "mg/ml",
    response_variable = "P",
    response_units = "mg/ml",
    predictors = 1,
    ...) {
  
  # The sigmoid function is defined in BayesPharma::sigmoid_stanvar
  response_eq <- as.formula(
    paste0(
      response_variable, " ~ tQ(",
        series_index_variable, ", ",
        time_variable, ", ",
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
    time_variable = time_variable,
    time_units = time_units,
    ET_variable = ET_variable,
    ET_units = ET_units,
    ST_variable = ST_variable,
    ST_units = ST_units,
    response_variable = response_variable,
    response_units = response_units)
  
  class(model_formula) <- c("bpformula", class(model_formula))
  model_formula  
  
}
