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
#' @param expose_functions boolean. Expose the BayesPharma functions for the
#'   model [default: TRUE].
#' @param ... additional arguments passed to \code{brms::brm}.
#'
#' @returns brmsfit model
#'
#' @export
tQ_model <- function(
    data,
    formula = tQ_formula(),
    prior = tQ_prior(),
    init = tQ_init(),
    iter = 8000,
    control = list(adapt_delta = 0.99),
    expose_functions = TRUE,
    ...) {

  
  if (!(
    formula$bayes_pharma_info[["series_index_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The series index ",
        "'",formula$bayes_pharma_info[["series_index_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }
  
  if (!(formula$bayes_pharma_info[["time_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The time variable ",
        "'",formula$bayes_pharma_info[["time_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }  
  
  if (!(formula$bayes_pharma_info[["ET_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The enzyme concentration variable ",
        "'",formula$bayes_pharma_info[["ET_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }    
  
  if (!(formula$bayes_pharma_info[["ST_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The substrate concentration variable ",
        "'",formula$bayes_pharma_info[["ST_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }  
  
  if (!(formula$bayes_pharma_info[["response_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The response variable ",
        "'",formula$bayes_pharma_info[["response_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }  
  
  model <- brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    iter = iter,
    control = control,
    stanvars = tQ_stanvar,
    ...)

  model$bayes_pharma_info <- list(model_type = "tQ")
  
  if("bayes_pharam_info" %in% names(formula)){
    model$bayes_pharma_info <- c(
      model$bayes_pharma_info,
      formula_info = formula$bayes_pharam_info)
  }

  # this is needed e.g. for brms::loo_compare()
  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  model
}
