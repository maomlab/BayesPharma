#' Define formula for the tQ enzyme kinetics model
#'
#' @param predictors predictors for `kcat` and `kM`.
#'
#'
#' @export
tQ_formula <- function(
    predictors = 1,
    ...) {

  predictor_eq <- rlang::new_formula(
    lhs = quote(kcat + kM),
    rhs = rlang::enexpr(predictors))

  brms::brmsformula(
    P ~ tQ_multiple(
      series_index, time, kcat, kM, ET, ST),
    predictor_eq,
    nl = TRUE,
    loop = FALSE,
    ...)
}
