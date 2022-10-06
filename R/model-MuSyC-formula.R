
#' Create a formula for the MuSyC synergy model
#'
#' @description setup a defaulMuSyC synergy model formula to predict
#'   the `E0`, `C1`, `E1`, `s1`, `C2`, `E2`, `s2`, `log10alpha`, and `E3alpha`
#'   parameters.
#'
#' @param predictors Additional formula objects to specify predictors of
#'   non-linear parameters. i.e. what perturbations/experimental differences
#'   should be modeled separately? (Default: 1) should a random effect be taken
#'   into consideration? i.e. cell number, plate number, etc.
#' @return brmsformula
#'
#' @examples
#'\dontrun{
#'   Consider observations made using 4 different drugs and the column header
#'   containing the labels for the 4 different drugs is `predictors`.
#'   dr_formula(predictors = 0 + predictors)
#'
#'   Consider that the cell_ID was recorded and the noise from the cell_ID is to
#'   be accounted for. dr_formula(predictors = 0 + predictors + (1|cell_ID))
#'}
#'
#' @export
MuSyC_formula <- function(
    predictors = 1,
    ...) {
  
  if (!is.null(predictors)) {
    predictor_eq <- rlang::new_formula(
      lhs = quote(
        logE0 +
        logC1 + logE1 + h1 +
        logC2 + logE2 + h2 +
        logE3 + logalpha),
      rhs = rlang::enexpr(predictors))
  } else {
    predictor_eq <- NULL
  }
  
  brms::brmsformula(
    response ~ MuSyC(
      logd1, logd2,
      logE0,
      logC1, logE1, h1,
      logC2, logE2, h2,
      logE3, logalpha),
    brms::nlf(logd1 ~ log(dose1 / d1_scale_factor)),
    brms::nlf(logd2 ~ log(dose2 / d2_scale_factor)),
    predictors_eq,
    nl = TRUE,
    ...)
}
