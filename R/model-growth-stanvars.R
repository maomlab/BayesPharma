#' Stan Code for the Growth Richards Function.
#'
#' @note stanvar script of a sigmoid growth model response equation
#'
#' @examples
#' \dontrun{
#' brms::brm(
#'   data = data.frame(
#'     response = ...,
#'     time = ...),
#'   formula = brms::brmsformula(
#'     response ~ growth_sigmoid(K, K0, rate, lambda, time),
#'     K + K0 + rate + lambda ~ 1,
#'     nl = TRUE),
#'   prior = ...,
#'   init =  ...,
#'   stanvars = BayesPharma::growth_sigmoid_stanvar())
#' }
#'
#' @export
growth_sigmoid_stanvar <- function() {
  brms::stanvar(
    scode = paste(
      "  real growth_sigmoid(",
      "      real K,",
      "      real K0,",
      "      real rate,",
      "      real lambda,",
      "      real time) {",
      "      return (K0 + K / (1 + exp(",
      "        4 * rate / K * (lambda - time) + 2)));",
      "   }", sep = "\n"),
    block = "functions")
}

#' Stan Code for the Richards Growth Function.
#'
#' @note stanvar script of a Richard growth model response equation.
#'
#' @examples
#' \dontrun{
#' brms::brm(
#'   data = data.frame(
#'     response = ...,
#'     time = ...),
#'   formula = brms::brmsformula(
#'     response ~ growth_richards(K, K0, rate, lambda, nu, time),
#'     K + K0 + rate + lambda + nu ~ 1,
#'     nl = TRUE),
#'   prior = ...,
#'   init =  ...,
#'   stanvars = BayesPharma::growth_sigmoid_stanvar())
#' }
#'
#' @export
growth_richards_stanvar <- function() {
  brms::stanvar(
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
}
