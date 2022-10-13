#' Generate MuSyC Ed scores using a robust functional form
#' It should give the same results as the simple one, but
#' be more numerically stable
#'
#' @param logd1 log dose for treatment 1
#' @param logd2 log dose for treatment 2
#'
#' @param logE0 numeric. Full log response with no treatment.
#' @param logE1 numeric. Full log response for treatment 1 and the
#'     dose of treatment 2 is zero.
#' @param logC1 numeric. IC50 for treatment 1 and dose of treatment 2
#'     is zero.
#' @param h1 numeric. Slope of treatment 1 at it's IC50 and dose of
#'     treatment 2 is zero in exponential form. Use
#'     \code{MuSyC_si_to_hi} to convert from slope form.
#' @param logE2 numeric. Full log response for treatment 2 and the
#'     dose of treatment 1 is zero.
#' @param logC2 numeric. IC50 for treatment 2 and dose of treatment 1
#'     is zero.
#' @param h2 numeric. Slope of treatment 2 at it's IC50 and dose of
#'     treatment 1 is zero in exponential form. Use
#'     \code{MuSyC_si_to_hi} to convert from slope form.
#' @param logE3 numeric. Full response for treatment 1 and treatment
#'     2.
#' @param logalpha numeric. Log synergistic potency parameter with
#'     greater than 1 is synergistic, less than 1 is antagonistic.
#'
#'
#' @export
MuSyC <- function(
    logd1,
    logd2,
    logE0,
    logE1, logC1, h1,
    logE2, logC2, h2,
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
