#' Run Bayesian Regression Model using Stan
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   <https://paul-buerkner.github.io/brms/reference/brm.html>
#'   or
#'   <https://rdrr.io/cran/rstan/man/stan.html>
#'
#' @param data data.frame of experimental data.
#'   must contain columns \code{response} and any predictors specified in
#'   the formula.
#' @param formula brmsformula object. To create a dose-response brmsformula,
#'   use the \code{sigmoid_formula} function.
#' @param prior brmspriors data.frame for ec50, hill, top, and bottom.
#'   Use one of the priors functions provided to create priors to use here.
#' @param init list of lists, numeric value, or "random" for the initial values
#'   of the parameters being modeled (default = 0).
#' @param iter number of iterations the model runs. Increasing iter can help
#'   with model convergence (default: 8000).
#' @param control a named list of parameters to control the sampler's behavior.
#'   Adding `max_treedepth` and giving a greater value than 10 can improve model
#'   convergence (default: `list(adapt_delta = 0.99)`).
#' @param stanvar_function stan code for the model.
#' @param ... additional arguments passed to `brms::brm`
#' 
#' @return `brmsfit` object
#'
#' @examples
#'\dontrun{
#'   sigmoid_model(data,
#'    formula = sigmoid_formula(predictors = 0 + drug))
#'}
#' @export
sigmoid_model <- function(
   data,
   formula = sigmoid_formula(),
   prior = sigmoid_prior(),
   init = sigmoid_init(),
   iter = 8000,
   control = list(adapt_delta = 0.99),
   stanvar_function = sigmoid_stanvar,
   ...) {

  if (is.null(prior)) {
    warning("priors for each parameter is required. Use prior functions provided
             to get default priors.")
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
  
  model$bayes_pharma <- list(model_type = "sigmoid")
  model
  
}
