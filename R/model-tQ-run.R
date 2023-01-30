#' Model for the tQ enzyme kinetics model
#'
#' @param data data.frame of experimental data.
#'   must contain columns \code{response} and any predictors specified in
#'   the formula.
#' @param formula brmsformula object. To create a dose-response brmsformula,
#'   use the \code{tQ_formula} function.
#' @param prior brmspriors data.frame for kcat, and kM.
#'   Use one of the priors functions provided to create priors to use here.
#' @param init list of lists, numeric value, or "random" for the initial values
#'   of the parameters being modeled (default = 0).
#' @param iter number of iterations the model runs. Increasing iter can help
#'   with model convergence (default = 8000).
#' @param control a named list of parameters to control the sampler's behavior.
#'   Adding max_treedepth and giving a greater value than 10 can improve model
#'   convergence (default = list(adapt_delta = 0.99)).
#' @param ... additional arguments passed to \code{brms::brm}.
#'
#' @return brmsfit model
#'
#' @export
tQ_model <- function(
    data,
    formula = tQ_formula(),
    prior = tQ_prior(),
    init = tQ_init(),
    iter = 8000,
    control = list(adapt_delta = 0.99),
    ...) {
    
  model <- brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    iter = iter,
    control = control,
    stanvars = tQ_stanvar,
    ...)

  model$bayes_pharma <- list(model_type = "tQ")
  model
}
