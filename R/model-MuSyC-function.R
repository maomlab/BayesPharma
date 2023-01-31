#' Generate MuSyC Ed scores using a robust functional form
#'
#'
#' @param logd1 \code{numeric}. log dose for treatment 1
#' @param logd2 \code{numeric}. log dose for treatment 2
#'
#' @param logE0 \code{numeric}. Full log response with no treatment.
#' @param logE1 \code{numeric}. Full log response for treatment 1 and the dose
#'     of treatment 2 is zero.
#' @param logC1 \code{numeric}. IC50 for treatment 1 and dose of treatment 2
#'     is zero.
#' @param h1 \code{numeric}. Slope of treatment 1 at its IC50 and dose of
#'     treatment 2 is zero in exponential form. Use \code{MuSyC_si_to_hi} to
#'     convert from slope form.
#' @param logE2 \code{numeric}. Full log response for treatment 2 and the dose
#'     of treatment 1 is zero.
#' @param logC2 \code{numeric}. IC50 for treatment 2 and dose of treatment 1 is
#'     zero.
#' @param h2 \code{numeric}. Slope of treatment 2 at it's IC50 and dose of
#'     treatment 1 is zero in exponential form. Use \code{MuSyC_si_to_hi} to
#'     convert from slope form.
#' @param logE3 \code{numeric}. Full response for treatment 1 and treatment 2.
#' @param logalpha \code{numeric}. Log synergistic potency parameter with
#'      greater than 1 is synergistic, less than 1 is antagonistic.
#'
#' @returns \{numeric} for the synergistic response to treatment 1 and
#'      treatment 2 at doses \code{logd1} and \code{logd2} respectively.
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
