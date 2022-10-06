#' Generate MuSyC Ed scores using a robust functional form
#' It should give the same results as the simple one, but
#' be more numerically stable
#' @export
MuSyC_model_generate <- function(
    logd1,
    logd2,
    logE0,
    h1, logC1, logE1,
    h2, logC2, logE2,
    logE3,
    logalpha) {
  numerator_parts <- c(
    h1 * logC1 + h2 * logC2 + logE0,
    h1 * logd1 + h2 * logC2 + logE1,
    h1 * logC1 + h2 * logd2 + logE2,
    h1 * logd1 + h2 * logd2 + logE3 + logalpha)
  numerator_max <- max(numerator_parts)
  log_numerator <- numerator_max +
    log(sum(exp(numerator_parts - numerator_max)))
  denominator_parts <- c(
    h1 * logC1 + h2 * logC2,
    h1 * logd1 + h2 * logC2,
    h1 * logC1 + h2 * logd2,
    h1 * logd1 + h2 * logd2 + logalpha)
  denominator_max <- max(denominator_parts)
  log_denominator <- denominator_max +
    log(sum(exp(denominator_parts - denominator_max)))
  exp(log_numerator - log_denominator)
}
