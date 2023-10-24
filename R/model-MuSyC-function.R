#' Generate MuSyC Ed Scores Using a Robust Functional Form
#'
#'
#' @param logd1 `numeric` log dose for treatment 1
#' @param logd2 `numeric` log dose for treatment 2
#'
#' @param logE0 `numeric` Full log response with no treatment.
#' @param logE1 `numeric` Full log response for treatment 1 and the dose
#'     of treatment 2 is zero.
#' @param logC1 `numeric` AC50 for treatment 1 and dose of treatment 2
#'     is zero.
#' @param h1 `numeric` Slope of treatment 1 at its AC50 and dose of
#'     treatment 2 is zero in exponential form. Use `MuSyC_si_to_hi` to
#'     convert from slope form.
#' @param logE2 `numeric` Full log response for treatment 2 and the dose
#'     of treatment 1 is zero.
#' @param logC2 `numeric` AC50 for treatment 2 and dose of treatment 1 is
#'     zero.
#' @param h2 `numeric` Slope of treatment 2 at its AC50 and dose of
#'     treatment 1 is zero in exponential form. Use `MuSyC_si_to_hi` to
#'     convert from slope form.
#' @param logE3 `numeric` Full response for treatment 1 and treatment 2.
#' @param logalpha `numeric` Log synergistic potency parameter with
#'      greater than 1 is synergistic, less than 1 is antagonistic.
#'
#' @returns `numeric` for the synergistic response to treatment 1 and
#'      treatment 2 at doses `logd1` and `logd2` respectively.
#'
#' @seealso [MuSyC_model]
#'
#' @references
#'   Meyer, D.J., Wooten, D.J., Paudel B.B., Bauer, J., Hardeman, K.N.,
#'   Westover, D., Lovly, C.M., Harris, L.A., Tyson D.R., Quaranta, V.,
#'   Quantifying Drug Combination Synergy along Potency and Efficacy Axes, Cell
#'   Syst. 8, 2 (2019). https://doi.org/10.1016/j.cels.2019.01.003
#'
#'   Wooten, D.J., Meyer, C.T., Lubbock, A.L.R. et al. MuSyC is a consensus
#'   framework that unifies multi-drug synergy metrics for combinatorial drug
#'   discovery. Nat Commun 12, 4607 (2021).
#'   https://doi.org/10.1038/s41467-021-24789-z
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

  numerator_parts <- h1 * logC1 + h2 * logC2 + logE0
  denominator_parts <- h1 * logC1 + h2 * logC2
  
  if (logd1 > -Inf) {
    numerator_parts <- c(numerator_parts, h1 * logd1 + h2 * logC2 + logE1)
    denominator_parts <- c(denominator_parts, h1 * logd1 + h2 * logC2)
  } else {
    numerator_parts <- c(numerator_parts, -Inf)
    denominator_parts <- c(denominator_parts, -Inf)
  }

  if (logd2 > -Inf) {
    numerator_parts <- c(numerator_parts, h1 * logC1 + h2 * logd2 + logE2)
    denominator_parts <- c(denominator_parts, h1 * logC1 + h2 * logd2)
  } else {
    numerator_parts <- c(numerator_parts, -Inf)
    denominator_parts <- c(denominator_parts, -Inf)
  }  
  
  if ((logd1 > -Inf) & (logd2 > -Inf)) {
    numerator_parts <- c(
      numerator_parts, h1 * logd1 + h2 * logd2 + logE3 + logalpha)
    denominator_parts <- c(
      denominator_parts, h1 * logd1 + h2 * logd2 + logalpha)
  } else {
    numerator_parts <- c(numerator_parts, -Inf)
    denominator_parts <- c(denominator_parts, -Inf)
  } 
  
  stable_logsumexp <- function(parts){
    stabilizer <- parts |>
      max() |>
      is.finite() |>
      ifelse(max(parts), 0)
    stabilizer + log(sum(exp(parts - stabilizer)))
  }
  exp(stable_logsumexp(numerator_parts) - stable_logsumexp(denominator_parts))
}
