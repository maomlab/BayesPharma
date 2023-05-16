#' Stan Code for the MuSyC Function.
#'
#' @note stanvar script of a MuSyC equation.
#'
#' @export
MuSyC_stanvar <- function() {
  brms::stanvar(
    scode = paste(
      "  real MuSyC(",
      "    real logd1, real logd2,",
      "    real logE0,",
      "    real logC1, real logE1, real h1,",
      "    real logC2, real logE2, real h2,",
      "    real logE3, real logalpha) {",
      "      vector[4] numerator_parts;",
      "      vector[4] denominator_parts;",
      "      numerator_parts[1] = h1*logC1 + h2*logC2 + logE0;",
      "      denominator_parts[1] = h1*logC1 + h2*logC2;",
      "      if( logd1 > negative_infinity() ) {",
      "        numerator_parts[2] = h1*logd1 + h2*logC2 + logE1;",
      "        denominator_parts[2] = h1*logd1 + h2*logC2;",
      "      } else {",
      "        numerator_parts[2] = negative_infinity();",
      "        denominator_parts[2] = negative_infinity();",
      "      }",
      "      if( logd2 > negative_infinity() ) {",
      "        numerator_parts[3] = h1*logC1 + h2*logd2 + logE2;",
      "        denominator_parts[3] = h1*logC1 + h2*logd2;",
      "      } else {",
      "        numerator_parts[3] = negative_infinity();",
      "        denominator_parts[3] = negative_infinity();",
      "      }",
      "      if(",
      "        (logd1 > negative_infinity()) && ",
      "        (logd2 > negative_infinity())) {",
      "        numerator_parts[4] = h1*logd1 + h2*logd2 + logE3 + logalpha;",
      "        denominator_parts[4] = h1*logd1 + h2*logd2 + logalpha;",
      "      } else {",
      "        numerator_parts[4] = negative_infinity();",
      "        denominator_parts[4] = negative_infinity();",
      "      }",
      "      return exp(log_sum_exp(numerator_parts) -",
      "         log_sum_exp(denominator_parts));",
      "  }", sep = "\n"),
    block = "functions")
}

#' Stan Code for the MuSyC Generated Quantities
#'
#' @export
MuSyC_genquant_stanvar <- function() {
  brms::stanvar(
    scode = paste(
      "  real E0 = exp(b_logE0[1]);",
      "  real E1 = exp(b_logE1[1]);",
      "  real E2 = exp(b_logE2[1]);",
      "  real E3 = exp(b_logE3[1]);",
      "  real s1 = b_h1[1] * (E0 + E1) / (4 * exp(b_logC1[1]));",
      "  real s2 = b_h2[1] * (E0 + E2) / (4 * exp(b_logC2[1]));",
      "  real C1 = exp(b_logC1[1]);",
      "  real C2 = exp(b_logC2[1]);",
      "  real alpha = exp(b_logalpha[1]);",
      sep = "\n"),
    block = "genquant",
    position = "end")
}