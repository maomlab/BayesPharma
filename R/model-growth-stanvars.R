#' Stan code for the growth Richards function.
#'
#' @note stanvar script of a sigmoid growth model response equation
#'
#' @export
growth_sigmoid_stanvar <- brms::stanvar(
  scode = paste(
    "  real growth_richards(",
    "      real K,",
    "      real K0,",
    "      real rate,",
    "      real lambda,",
    "      real time) {",
    "      real term = ;",
    "      return (K0 + (K - K0) / (1 + exp(",
    "        4 * rate / (K - K0) * (lambda - time) + 2));",
    "   }", sep = "\n"),
  block = "functions")


#' Stan code for the Richards growth function.
#'
#' @note stanvar script of a Richard growth model response equation.
#'
#' @export
growth_richards_stanvar <- brms::stanvar(
  scode = paste(
    "  real growth_richards(",
    "      real K,",
    "      real K0,",
    "      real rate,",
    "      real lambda,",
    "      real nu,",
    "      real time) {",
    "      real term = 1 + nu + ",
    "         rate / (K - K0) * (1 + nu) ^ (1 + 1 / nu) * (lambda - time);",
    "      return (K0 + (K - K0) / (1 + nu * exp(term) ^ (1 / nu)));",
    "   }", sep = "\n"),
  block = "functions")
