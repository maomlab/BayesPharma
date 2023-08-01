#' Define a Formula for the Michaelis Menten Enzyme Kinetics Model
#'
#' @description set-up a Michaelis Menten enzyme kinetics model formula to
#'   define a an ordinary differential equation model that with `kcat`, `kM`
#'   model parameters for use in the [michaelis_menten_model].
#'
#' @param series_index_variable `character` variable indexing which
#'   measurements are part of a common time series
#' @param treatment_variable `character` variable representing time as the
#'   treatment
#' @param treatment_units `character` the units of the treatment variable
#' @param ET_variable `character` variable representing the total enzyme
#'   concentration
#' @param ET_units `character` variable representing the units of the
#'   enzyme concentration
#' @param ST_variable `character` variable representing the total substrate
#'   concentration
#' @param ST_units `character` variable representing the units of the
#'   substrate concentration
#' @param response_variable `character` variable representing the `product`
#' @param response_units `character` the units of the `product`
#' @param predictors `expression` of predictors for `kcat` and `kM` parameters
#' @param ... additional arguments passed to [brms::brmsformula()]
#'
#' @returns a `bpformula`, which is a subclass of [brms::brmsformula] and can be
#'   passed to [michaelis_menten_model()].
#'
#' @seealso [michaelis_menten_model], [michaelis_menten_prior],
#'   [michaelis_menten_init], [michaelis_menten_stanvar] and
#'   [brms::brmsformula], which this function wraps.
#'
#' @references
#' Choi, B., Rempala, G.A. & Kim, J.K. Beyond the Michaelis-Menten equation:
#' Accurate and efficient estimation of enzyme kinetic parameters. Sci Rep 7,
#' 17018 (2017). https://doi.org/10.1038/s41598-017-17072-z
#'
#' @export
michaelis_menten_formula <- function(
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

  # The michaelis_menten function is defined in
  # BayesPharma::michaelis_menten_stanvar
  response_eq <- stats::as.formula(
    paste0(
      response_variable, " ~ michaelis_menten_multiple(",
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
    formula_type = "michaelis_menten",
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
