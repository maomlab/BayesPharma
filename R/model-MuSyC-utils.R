#' Convert from slope parametrization to the exponent parametrization for drug i
#'
#' This can be used for setting priors and interpreting parameter estimates
#'
#' @param si slope of drug i at it's IC50 and doses of all other drugs are zero
#' @param Ci the IC50 of drug i
#' @param E0 the reponse with no treatments
#' @param Ei the reponse of inifinite drug i and no other treatments
#' @return hi the exponent in the MuSyC equation for drug i
#'
#'
#' Claim: When d1=0 and d2=C2 then d(Ed)/d(d2) = s2
#'        where s2 = h2 * (E0 + E2) / (4 * C2)
#'
#' d(Ed)/d(d2)
#'   =  d/d(d2)
#'      (C1^h1 * C2^h2 * E0 + C1^h1 * d2^h2 * E2) /
#'      (C1^h1 * C2^h2      + C1^h1 * d2^h2)
#'
#'Cancle the C1^h1 terms:
#'   =  d/d(d2)
#'      (C2^h2 * E0 + d2^h2 * E2) /
#'      (C2^h2      + d2^h2)
#'
#'
#' distribute the derivative across the terms in the numerator
#'   =  E0 * C2^h2 * [d/d(d2) 1     / (C2^h2 + d2^h2)] +
#'      E2         * [d/d(d2) d2^h2 / (C2^h2 + d2^h2)]
#'
#'   =  E0 * C2^h2 * [h2 * d2^(h2-1) / (C2^h2 + d2^h2)^2] +
#'      E2 * [C2^h2 * h2 * d2^(h2-1) / (C2^h2 + d2^h2)^2]
#'
#'   =  (E0 + E2) * C2^h2 * h2 * d2^(h2-1)/(C2^h2 + d2^h2)^2
#'
#' Evaluate at d2 = C2:
#'   =  (E0 + E2) * h2 * C2^(2*h2-1) / [4*C2^(2*h2))]
#'   =  h2 * (E0 + E2) / (4 * C2)
#'
#'
#'@export
MuSyC_si_to_hi <- function(si, Ci, E0, Ei) {
  si * 4 * Ci / (E0 + Ei)
}

#' Convert slope parametrization to exponent parametrization for drug i
#'
#' This can be used for setting priors and interpreting parameter estimates
#' see MuSyC_si_to_hi for details
#'
#' @param si slope of drug i at it's IC50 and doses of all other drugs are zero
#' @param Ci the IC50 of drug i
#' @param E0 the response with no treatments
#' @param Ei the response of infinite drug i and no other treatments
#' @return hi the exponent in the MuSyC equation for drug i
#'
#'@export
MuSyC_hi_to_si <- function(hi, Ci, E0, Ei) {
  hi * (E0 + Ei) / (4 * Ci)
}
