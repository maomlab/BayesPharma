#' Sigmoid growth function
#'
#' Functional form for the sigmoid growth model, related to the Richards growth
#' model by setting nu = 1.
#'
#' @param K \code{numeric}, the carrying capacity
#' @param K0 \code{numeric}, the baseline response
#' @param rate \code{numeric}, maximum growth rate
#' @param lambda \code{numeric}, time line through the maximum growth rate
#'   crosses zero
#' @param time \code{numeric}, time point at which to evaluate the response.
#'
#' @returns \code{numeric}, response given the time and parameters
#'
#' @details The Richards growth model is given by
#' 
#'   response(time) =
#'     K0 + (K - K0)/(1 + nu * exp(
#'     1 + nu + rate/(K - K0) * (1 + nu)^(1 + 1/nu) * (lambda - time))) ^ (1/nu)
#'
#' Setting nu = 1, and simplifying gives
#'   
#'   response(time) =
#'     K0 + (K - K0)/(1 + 1 * exp(
#'     1 + 1 + rate/(K - K0) * (1 + 1)^(1 + 1/1) * (lambda - time))) ^ (1/1)
#'    
#'   response(time) =
#'     K0 + (K - K0)/(1 + exp(
#'     2 + rate/(K - K0) * (2)^(2) * (lambda - time))) ^ (1/1)
#'
#'   response(time) =
#'     K0 + (K - K0)/(1 + exp(2 + rate/(K - K0) * 4 * (lambda - time)))
#'     
#'   response(time) =
#'     K0 + (K - K0)/(1 + exp(4 * rate/(K - K0) * (lambda - time) + 2))
#'
#' The sigmoid growth curve is related to the sigmoid agoinst model by setting
#' top = K, bottom = K0, hill = 4 * rate / (K - K0) / log10(e), AC50 = lambda +
#' 2 * log10(e) / hill, and log_dose = time, where e is exp(1):
#' 
#'   response(time) = 
#'     bottom + (top - bottom)/(1 + exp(hill/log10(e) * (lambda - time) + 2))
#'     
#'   response(time) = 
#'     bottom + (top - bottom)/(1 + exp(hill/log10(e) * (AC50 - time))) 
#'     
#'   response(log_dose) = 
#'     bottom + (top - bottom)/(1 + 10^((ac50 - log_dose) * hill))
#'
#' @examples
#' \dontrun{
#'
#'  # Generate Sigmoid growth curve
#'  data <- data.frame(
#'    time = seq(0, 2, length.out = 101)) |>
#'      dplyr::mutate(
#'        response = stats::rnorm(
#'          n = length(time),
#'          mean = BayesPharma::growth_sigmoid(
#'            K = 1,
#'            K0 = 0,
#'            rate = 2,
#'            lambda = 0.5,
#'            time = time),
#'        sd = .2))
#' }
#'
#' @export
growth_sigmoid <- Vectorize(
  function(K, K0, rate, lambda, time) {
    K0 + (K - K0) / (1 + exp(4 * rate/(K - K0) * (lambda - time) + 2))
  })

#' Richards growth function
#'
#' Functional form for the Richards growth model.
#'
#' @param K \code{numeric}, the carrying capacity
#' @param K0 \code{numeric}, the baseline response
#' @param rate \code{numeric}, maximum growth rate
#' @param lambda \code{numeric}, time line through the maximum growth rate
#'   crosses zero
#' @param nu \code{numeric}, asymmetry in growth before and after the inflection
#'   point
#' @param time \code{numeric}, time point at which to evaluate the response.
#'
#' @returns numeric, response given the time and parameters
#'
#'   response(time) =
#'     K0 + (K - K0)/(
#'       1 + nu * exp(
#'         1 + nu + rate/(K - K0) * (1 + nu)^(1 + 1/nu) *
#'         (lambda - time))) ^ (1/nu)
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
