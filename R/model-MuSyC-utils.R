#' Convert From Slope to Exponent Parameterization for Drug i in the MuSyC Model
#'
#' @description This can be used for setting priors and interpreting parameter
#'   estimates
#'
#' @param si `numeric` value of slope of drug i at its AC50 and doses of
#'   all other drugs are zero
#' @param Ci `numeric` value of the AC50 of drug i
#' @param E0 `numeric` value of the response with no treatments
#' @param Ei `numeric` value of the response of infinite drug i and no
#'   other treatments
#'
#' @returns hi `numeric` value of the exponent in the MuSyC equation for
#'   drug i
#'
#' @details Claim: When `d1=0` and `d2=C2` then the gradient of the response
#'   with respect to `d2` is the `s2`, symbolically `d(Ed)/d(d2) = s2` where
#'   `s2 = h2 * (E0 + E2) / (4 * C2)` then
#'
#'     d(Ed)/d(d2)
#'       = d/d(d2)
#'         (C1^h1 * C2^h2 * E0 + C1^h1 * d2^h2 * E2) /
#'         (C1^h1 * C2^h2      + C1^h1 * d2^h2)
#'
#'   Cancel the `C1^h1` terms:
#'
#'       =  d/d(d2)
#'          (C2^h2 * E0 + d2^h2 * E2) /
#'          (C2^h2      + d2^h2)
#'
#'   Distribute the derivative across the terms in the numerator
#'
#'       =  E0 * C2^h2 * (d/d(d2) 1     / (C2^h2 + d2^h2)) +
#'          E2         * (d/d(d2) d2^h2 / (C2^h2 + d2^h2))
#'
#'       =  E0 * C2^h2 * (h2 * d2^(h2-1) / (C2^h2 + d2^h2)^2) +
#'          E2 * (C2^h2 * h2 * d2^(h2-1) / (C2^h2 + d2^h2)^2)
#'
#'       =  (E0 + E2) * C2^h2 * h2 * d2^(h2-1)/(C2^h2 + d2^h2)^2
#'
#'   Evaluate at `d2 = C2`:
#'
#'       =  (E0 + E2) * h2 * C2^(2*h2-1) / (4*C2^(2*h2)))
#'       =  h2 * (E0 + E2) / (4 * C2)
#'
#' @seealso [MuSyC_hi_to_si]
#'
#'@export
MuSyC_si_to_hi <- function(si, Ci, E0, Ei) {
  si * 4 * Ci / (E0 + Ei)
}

#' Convert Exponent to Slope Parameterization for Drug i in MuSyC Model
#'
#' @description This can be used for setting priors and interpreting parameter
#'   estimates see [MuSyC_si_to_hi()] for details
#'
#' @param hi `numeric` value of the exponent in the MuSyC equation for drug
#'   i
#' @param Ci `numeric` value of the AC50 of drug i
#' @param E0 `numeric` value of the response with no treatments
#' @param Ei `numeric` value of the response of infinite drug i and no
#'   other treatments
#' @returns si slope of drug i at it's AC50 and doses of all other drugs are
#'   zero
#'
#' @seealso [MuSyC_si_to_hi]
#'
#'@export
MuSyC_hi_to_si <- function(hi, Ci, E0, Ei) {
  hi * (E0 + Ei) / (4 * Ci)
}
