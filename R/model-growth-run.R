#' Run Bayesian Richards Growth Model
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   <https://paul-buerkner.github.io/brms/reference/brm.html>
#'   or
#'   <https://rdrr.io/cran/rstan/man/stan.html>
#'
#' @param data \code{data.frame} of experimental data.  must contain columns
#'     \code{response} and any predictors specified in the formula.
#' @param formula \code{brmsformula} object. To create a growth model formula,
#'     (default: \code{BayesPharma::growth_formula()}.
#' @param prior brmspriors data.frame for A, nu, mu, and lambda.
#'     Use one of the priors functions provided to create priors to
#'     use here.  (default:
#'     \code{BayesPharma::growth_richards_prior()}
#' @param init initial values of the parameters being modeled (default
#'     = \code{BayesPharma::growth_richards_init()}
#' @param iter number of iterations the model runs. Increasing iter
#'     can help with model convergence (default: 8000).
#' @param control a named list of parameters to control the sampler's
#'     behavior.  Adding \code{max_treedepth} and giving a greater
#'     value than 10 can improve model convergence (default:
#'     \code{list(adapt_delta = 0.99)}).
#' @param stanvar_function Stan code for the model (default:
#'     \code{BayesPharma::sigmoid_stanvar})
#' @param expose_functions boolean. Expose the BayesPharma functions for the
#'   model [default: TRUE].
#' @param ... additional arguments passed to \code{brms::brm}
#'
#' @returns \code{brmsfit} object
#'
#' @examples
#'\dontrun{
#'   BayesPharma::growth_model(
#'     data = data,
#'     formula = BayesPharma::growth_richards_formula(predictors = 0 + drug))
#'}
#' @export
growth_richards_model <- function(
    data,
    formula = growth_richards_formula(),
    prior = growth_richards_prior(),
    init = growth_richards_init(),
    iter = 8000,
    control = list(adapt_delta = 0.99),
    stanvar_function = growth_richards_stanvar,
    expose_functions = TRUE,
    ...) {

  if (!methods::is(formula, "bpformula")) {
    warning(
      "formula must be a 'bpformula'. You can use the ",
      "'BayesPharma::growth_richards_formula(...)' prior function")
  }

  if (!(formula$bayes_pharma_info[["time_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The time variable ",
        "'", formula$bayes_pharma_info[["time_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }

  if (!(formula$bayes_pharma_info[["response_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The response variable ",
        "'", formula$bayes_pharma_info[["response_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }

  if (!methods::is(prior, "brmsprior")) {
    warning(
      "prior must be a 'brmsprior'. Use either the ",
      "'BayesPharma::growth_richards_prior(...)'")
  }

  model <- brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    iter = iter,
    control = control,
    stanvars = stanvar_function,
    ...)

  model$bayes_pharma <- list(model_type = "growth_richards")
  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharam_info))

  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  model
}
