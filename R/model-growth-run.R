#' Run the Bayesian Sigmoid Growth Model
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   [brms::brms()] and the [stan](https://rdrr.io/cran/rstan/man/stan.html)
#'   documentation.
#'
#' @param data `data.frame` of experimental data. Must contain column
#'   representing the treatment, response and predictors specified in the
#'   formula.
#' @param formula `bpformula` object. To create a growth model formula, use
#'   [growth_sigmoid_formula()]
#' @param prior `brmspriors` `data.frame` for `K`, `K0`, `rate`, and `lambda`.
#'   To create a prior, use [growth_sigmoid_prior()].
#' @param init initial values of the parameters being modeled. To create an
#'   init, use [growth_sigmoid_init()]
#' @param iter `numeric` value for the number of iterations the model runs.
#'   Increasing `iter` can help with model convergence
#' @param control a named `list` of parameters to control the sampler's
#'   behavior. Adding `max_treedepth` and giving a greater value than
#'   `10` can improve model convergence
#' @param stanvar_function Stan code for the model
#' @param expose_functions `logical`. Expose the stan functions for the
#'   model. This is needed e.g. to run [brms::loo_compare()]
#' @param ... additional arguments passed to [brms::brm()]
#'
#' @returns `bpfit` a wrapper for [brms::brmsfit()]
#'
#' @seealso Helper functions: [growth_sigmoid_formula], and
#'   [growth_sigmoid_prior], [growth_sigmoid_init] and [brms::brmsfit]
#'
#' @examples
#'\dontrun{
#'   BayesPharma::growth_sigmoid_model(
#'     data = data,
#'     formula = BayesPharma::growth_richards_formula(predictors = 0 + drug))
#'}
#' @export
growth_sigmoid_model <- function(
  data,
  formula = growth_richards_formula(),
  prior = growth_richards_prior(),
  init = growth_richards_init(),
  iter = 8000,
  control = list(adapt_delta = 0.99),
  stanvar_function = growth_sigmoid_stanvar(),
  expose_functions = TRUE,
  ...) {

  if (!inherits(formula, "bpformula")) {
    warning(
      "formula must be a 'bpformula'. You can use the ",
      "'BayesPharma::growth_sigmoid_formula(...)' prior function")
  }

  if (!(formula$bayes_pharma_info[["treatment_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The time variable ",
        "'", formula$bayes_pharma_info[["treatment_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }

  if (!(formula$bayes_pharma_info[["response_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The response variable ",
        "'", formula$bayes_pharma_info[["response_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }

  if (!inherits(prior, "brmsprior")) {
    warning(
      "prior must be a 'brmsprior'. Use either the ",
      "'BayesPharma::growth_sigmoid_prior(...)'")
  }

  init <- eval_init(
    init,
    sdata = brms::make_standata(
      formula = formula,
      data = data,
      prior = prior,
      ...),
    chains = args$chains)

  model <- brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    iter = iter,
    control = control,
    stanvars = stanvar_function,
    ...)

  model$bayes_pharma <- list(model_type = "growth_sigmoid")
  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharma_info))

  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  class(model) <- c("bpfit", class(model))
  model
}


#' Run Bayesian Richards Growth Model
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   [brm] and the [stan](https://rdrr.io/cran/rstan/man/stan.html)
#'   documentation.
#'
#' @param data `data.frame` of experimental data. Must contain column
#'   representing the treatment, response and predictors specified in the
#'   formula.
#' @param formula `bpformula` object. To create a growth model formula, use
#'   [growth_richards_formula()]
#' @param prior `brmspriors` `data.frame` for `K`, `K0`,
#'   `rate`, `lambda`, and `nu`. To create a prior, use
#'   [growth_richards_prior()]
#' @param init initial values of the parameters being modeled. To create an
#'   init, use [growth_richards_init()]
#' @param iter `numeric` value for the number of iterations the model runs.
#'   Increasing `iter` can help with model convergence
#' @param control a named `list` of parameters to control the sampler's
#'   behavior. Adding `max_treedepth` and giving a greater value than
#'   `10` can improve model convergence
#' @param stanvar_function Stan code for the model
#' @param expose_functions `logical`. Expose the stan functions for the
#'   model. This is needed e.g. to run [brms::loo_compare()]
#' @param ... additional arguments passed to [brms::brm()]
#'
#' @returns `bpfit` which wraps [brms::brmsfit()]
#'
#' @seealso Helper functions: [growth_richards_formula],
#'   [growth_richards_prior], and [growth_richards_init], and [brms::brmsfit]
#'
#'
#' @examples
#'\dontrun{
#'   BayesPharma::growth_sigmoid_model(
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
  stanvar_function = growth_richards_stanvar(),
  expose_functions = TRUE,
  ...) {

  args <- list(...)

  if (!inherits(formula, "bpformula")) {
    warning(
      "formula must be a 'bpformula'. You can use the ",
      "'BayesPharma::growth_richards_formula(...)' prior function")
  }

  if (!(formula$bayes_pharma_info[["treatment_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The time variable ",
        "'", formula$bayes_pharma_info[["treatment_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }

  if (!(formula$bayes_pharma_info[["response_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The response variable ",
        "'", formula$bayes_pharma_info[["response_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }

  if (!inherits(prior, "brmsprior")) {
    warning(
      "prior must be a 'brmsprior'. Use either the ",
      "'BayesPharma::growth_richards_prior(...)'")
  }

  init <- eval_init(
    init,
    sdata = brms::make_standata(
      formula = formula,
      data = data,
      prior = prior,
      ...),
    chains = args$chains)

  model <- brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    iter = iter,
    control = control,
    stanvars = stanvar_function,
    ...)

  model$bayes_pharma_info <- list(model_type = "growth_richards")
  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharma_info))

  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  class(model) <- c("bpfit", class(model))
  model
}
