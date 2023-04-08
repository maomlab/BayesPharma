#' Richards growth function
#'
#' Functional form for the Richards growth model.
#'
#' @param K numeric, the carrying capacity
#' @param K0 numeric, the baseline response
#' @param rate numeric, maximum growth rate
#' @param lambda numeric, time line through the maximum growth rate crosses zero
#' @param nu numeric
#' @param time numeric, time point at which to evaluate the response.
#' @returns numeric, response given the time and parameters
#'
#'
#' Y(time) =
#'   K0 + (K - K0)/(
#'     1 + nu * exp(
#'       1 + nu + rate/(K - K0) * (1 + nu)^(1 + 1/nu) *
#'       (lambda - time))) ^ (1/nu)
#'
#' @examples
#' \dontrun{
#'
#'  # Generate Richards growth curve
#'  data <- data.frame(
#'    time = seq(0, 2, length.out = 101)) |>
#'      dplyr::mutate(
#'        response = stats::rnorm(
#'          n = length(time),
#'          mean = BayesPharma::growth_richards(
#'            K = 1,
#'            K0 = 0,
#'            rate = 2,
#'            lambda = 0.5,
#'            nu = 2,
#'            time = time),
#'        sd = .2))
#' }
#'
#' @export
growth_richards <- Vectorize(
  function(K, K0, rate, lambda, nu, time) {
    K0 + (K - K0) / (
      1 + nu * exp(
        1 + nu + rate / (K - K0) * (1 + nu) ^ (1 + 1 / nu) *
        (lambda - time))) ^ (1 / nu)
  })
