#' Sigmoid growth function
#'
#' @description Functional form for the sigmoid growth model, related to the
#' Richards growth model by setting `nu = 1`.
#'
#'
#'   The parameterization follows (Zwietering, 1990) and \pkg{grofit}:
#'
#'     K      = **carrying capacity**, `K = response(time = Inf)`. The
#'              \pkg{grofit} package calls this parameter `A`. `K` has the same
#'              units as the `response`.
#'     K0     = **initial population size** `K0 = response(time = 0)`. The
#'              \pkg{grofit} package assumes `K0=0`. `K0` has the same units as
#'              the `response`.
#'     rate   = **maximum growth rate** `rate = max[d(response)/d(time)]`. The
#'              \pkg{grofit} package calls this `mu`. `rate` has the units of
#'              `response/time`
#'     lambda = **duration of the lag-phase** the time point at which the
#'              tangent through the growth curve when it achieves the maximum
#'              growth rate crosses the initial population size `K0`. (see
#'              Figure 2 in (Kahm et al., 2010)).
#'
#' See the vignettes(topic = "derive_growth_model", package = "BayesPharma")
#'
#'
#' @param K `numeric`, the carrying capacity
#' @param K0 `numeric`, initial population size
#' @param rate `numeric`, maximum growth rate
#' @param lambda `numeric`, duration of the lag-phase
#' @param time `numeric`, time point at which to evaluate the response
#'
#' @returns `numeric`, response given the time and parameters
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
#' @references
#' Zwietering M. H., Jongenburger I., Rombouts F. M., van 't Riet K., (1990)
#' Modeling of the Bacterial Growth Curve. Appl. Environ. Microbiol., 56(6),
#' 1875-1881 https://doi.org/10.1128/aem.56.6.1875-1881.1990
#'
#' Kahm, M., Hasenbrink, G., Lichtenberg-Fraté, H., Ludwig, J., & Kschischo, M.
#' (2010). grofit: Fitting Biological Growth Curves with R. J. Stat. Softw.,
#' 33(7), 1–21. https://doi.org/10.18637/jss.v033.i07
#'
#' @export
growth_sigmoid <- Vectorize(
  function(K, K0, rate, lambda, time) {
    K0 + (K - K0) / (1 + exp(4 * rate / (K - K0) * (lambda - time) + 2))
  })

#' Richards growth function
#'
#' @description Functional form for the Richards growth model.
#'
#'     response(time) =
#'       K0 + (K - K0)/(
#'         1 + nu * exp(
#'           1 + nu + rate/(K - K0) * (1 + nu)^(1 + 1/nu) *
#'           (lambda - time))) ^ (1/nu)
#'
#'   The parameterization follows (Zwietering, 1990) and \pkg{grofit}:
#'
#'     K      = **carrying capacity**, `K = response(time = Inf)`. The
#'              \pkg{grofit} package calls this parameter `A`. `K` has the same
#'              units as the `response`.
#'     K0     = **initial population size** `K0 = response(time = 0)`. The
#'              \pkg{grofit} package assumes `K0=0`. `K0` has the same units as
#'              the `response`.
#'     rate   = **maximum growth rate** `rate = max[d(response)/d(time)]`. The
#'              \pkg{grofit} package calls this `mu`. `rate` has the units of
#'              `response/time`
#'     lambda = **duration of the lag-phase** the time point at which the
#'              tangent through the growth curve when it achieves the maximum
#'              growth rate crosses the initial population size `K0`. (see
#'              Figure 2 in (Kahm et al., 2010)).
#'     nu     = **growth asymmetry** before and after the inflection
#'
#' @param K `numeric` the carrying capacity
#' @param K0 `numeric` the baseline response
#' @param rate `numeric` the maximum growth rate
#' @param lambda `numeric` duration of the lag-phase
#' @param nu `numeric` growth asymmetry before and after the inflection point
#' @param time `numeric` time point at which to evaluate the response
#'
#' @returns `numeric` response given the time and parameters

#'
#' @examples
#' \dontrun{
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
#'
#' @references
#' Zwietering M. H., Jongenburger I., Rombouts F. M., van 't Riet K., (1990)
#' Modeling of the Bacterial Growth Curve. Appl. Environ. Microbiol., 56(6),
#' 1875-1881 https://doi.org/10.1128/aem.56.6.1875-1881.1990
#'
#' Kahm, M., Hasenbrink, G., Lichtenberg-Fraté, H., Ludwig, J., & Kschischo, M.
#' (2010). grofit: Fitting Biological Growth Curves with R. J. Stat. Softw.,
#' 33(7), 1–21. https://doi.org/10.18637/jss.v033.i07
#'
#' @export
growth_richards <- Vectorize(
  function(K, K0, rate, lambda, nu, time) {
    K0 + (K - K0) / (
      1 + nu * exp(
        1 + nu + rate / (K - K0) * (1 + nu) ^ (1 + 1 / nu) *
        (lambda - time))) ^ (1 / nu)
  })
