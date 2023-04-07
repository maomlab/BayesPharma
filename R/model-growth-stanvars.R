#' Stan code for the growth Richards function.
#'
#' @note stanvar script of a growth Richards dose response equation.
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