#' Run Bayesian Sigmoid Agonist Model
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   <https://paul-buerkner.github.io/brms/reference/brm.html>
#'   or
#'   <https://rdrr.io/cran/rstan/man/stan.html>
#'
#' @param data data.frame of experimental data.
#'   must contain columns `response` and any predictors specified in
#'   the formula.
#' @param formula brmsformula object. To create a dose-response brmsformula,
#'   (default: `BayesPharma::sigmoid_agonist_formula()`.
#' @param prior brmspriors data.frame for ec50, hill, top, and bottom.
#'   Use one of the priors functions provided to create priors to use here.
#'   (default: `BayesPharma::sigmoid_agonist_prior()`
#' @param init initial values of the parameters being modeled (default =
#'   `BayesPharma::sigmoid_agonist_init()`
#' @param iter number of iterations the model runs. Increasing iter can help
#'   with model convergence (default: 8000).
#' @param control a named list of parameters to control the sampler's behavior.
#'   Adding `max_treedepth` and giving a greater value than 10 can improve model
#'   convergence (default: `list(adapt_delta = 0.99)`).
#' @param stanvar_function stan code for the model (default:
#'   BayesPharma::sigmoid_stanvar)
#' @param ... additional arguments passed to `brms::brm`
#'
#' @return `brmsfit` object
#'
#' @examples
#'\dontrun{
#'   sigmoid_agoinst_model(data,
#'    formula = sigmoid_agonist_formula(predictors = 0 + drug))
#'}
#' @export
sigmoid_agonist_model <- function(
  data,
  formula = sigmoid_agonist_formula(),
  prior = sigmoid_agonist_prior(),
  init = sigmoid_agonist_init(),
  iter = 8000,
  control = list(adapt_delta = 0.99),
  stanvar_function = sigmoid_stanvar,
  ...) {

  if (!methods::is(formula, "brmsformula")) {
    warning(
      "formula must be a `brmsformula`. Use either the ",
      "'BayesPharma::sigmoid_agonist_formula(...)'")
  }

  if (!methods::is(prior, "brmsprior")) {
    warning(
      "prior must be a 'brmsprior'. Use either the ",
      "'BayesPharma::sigmoid_agonist_prior(...)'")
  }


  if (!("response" %in% names(data))) {
    warning(
      "There needs to be a column 'response' in the input 'data' data.frame\n")
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

  model$bayes_pharma <- list(model_type = "sigmoid_agonist")
  model

}


#' Run Bayesian Sigmoid Antagonist Model
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   <https://paul-buerkner.github.io/brms/reference/brm.html>
#'   or
#'   <https://rdrr.io/cran/rstan/man/stan.html>
#'
#' @param data data.frame of experimental data.
#'   must contain columns `response` and any predictors specified in
#'   the formula.
#' @param formula brmsformula object. To create a dose-response brmsformula,
#'   (default: `BayesPharma::sigmoid__formula()`.
#' @param prior brmspriors data.frame for ec50, hill, top, and bottom.
#'   Use one of the priors functions provided to create priors to use here.
#'   (default: `BayesPharma::sigmoid_antagonist_prior()`
#' @param init initial values of the parameters being modeled (default =
#'   `BayesPharma::sigmoid_antagonist_init()`
#' @param iter number of iterations the model runs. Increasing iter can help
#'   with model convergence (default: 8000).
#' @param control a named list of parameters to control the sampler's behavior.
#'   Adding `max_treedepth` and giving a greater value than 10 can improve model
#'   convergence (default: `list(adapt_delta = 0.99)`).
#' @param stanvar_function stan code for the model (default:
#'   BayesPharma::sigmoid_stanvar)
#' @param ... additional arguments passed to `brms::brm`
#'
#' @return `brmsfit` object
#'
#' @examples
#'\dontrun{
#'   sigmoid_agoinst_model(data,
#'    formula = sigmoid_antagonist_formula(predictors = 0 + drug))
#'}
#' @export
sigmoid_antagonist_model <- function(
    data,
    formula = sigmoid_antagonist_formula(),
    prior = sigmoid_antagonist_prior(),
    init = sigmoid_antagonist_init(),
    iter = 8000,
    control = list(adapt_delta = 0.99),
    stanvar_function = sigmoid_stanvar,
    ...) {
  
  if (!methods::is(formula, "brmsformula")) {
    warning(
      "formula must be a `brmsformula`. Use either the ",
      "'BayesPharma::sigmoid_antagonist_formula(...)' prior functions")
  }
  
  if (!methods::is(prior, "brmsprior")) {
    warning(
      "prior must be a 'brmsprior'. Use either the ",
      "'BayesPharma::sigmoid_antagonist_prior(...)' functions.")
  }
  
  
  if (!("response" %in% names(data))) {
    warning(
      "There needs to be a column 'response' in the input 'data' data.frame\n")
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
  
  model$bayes_pharma <- list(model_type = "sigmoid_antagonist")
  model
  
}

