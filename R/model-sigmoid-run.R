#' Fit a Bayesian sigmoid agonist model
#'
#' @description Fits the sigmoid agonist model. The functional form is
#'  \preformatted{
#'    <response> ~ sigmoid(ec50, hill, top, bottom, <treatment>)}
#'  where
#'  \preformatted{
#'    sigmoid = bottom + (top - bottom)/(1 + 10^((ec50 - <treatment>) * hill))}
#'  By default the observed data (and therefore should be columns in the input
#'  data data.frame) are
#'  \itemize{
#'    \item{**<treatment>**: `log_dose`, the `log10` of the dose
#'      as a molar concentration}
#'    \item{**<response>**: `response`, with unspecified units}
#'  }
#'      
#'  and the modeled parameters are
#'  \itemize{
#'    \item{**ec50**: the dose where the response reaches half maximal
#'      activity}
#'    \item{**hill**: the hill coefficient controlling the slope at the
#'      `ec50`}, by convention the slope of an agonist is positive
#'    \item{**top**: the response when <treatment> => `Inf`}
#'    \item{**bottom**: the response when <treatment> => `-Inf`}
#'  }
#'  To configure the model you can use the following helper functions:
#'  \itemize{
#'    \item{[sigmiod_agonist_formula()]: define the `treatment`
#'      and `response` variables, and predictors for the model parameters}
#'    \item{[sigmoid_agonist_prior()]: define the prior for the model
#'      parameters}
#'    \item{[sigmoid_agonist_init()]: define initial values for the
#'      model parameters}
#'  }
#'
#' @param data `data.frame` of observed data. It must contain columns
#'   for the treatment, response and any additional predictors specified in the
#'   formula. See [sigmoid_agonist_formula()] for more details.
#' @param formula `bpformula` for the sigmoid agonist model
#'   (Default: [sigmoid_agonist_formula()])
#' @param prior `brmsprior` defining a distribution for the model
#'   parameters `ec50`, `hill`, `top`, and `bottom`.
#'   Use one of the priors functions provided to create priors to
#'   use here.  (default: [sigmoid_agonist_prior()]
#' @param init `function` returning a named `list` of functions
#'   returning arrays giving initial values of the parameters being modeled for
#'   each chain. (Default: [sigmoid_agonist_init()]
#' @param iter `number` of iterations the model runs. Increasing
#'   `iter` can help with model convergence. (Default: `8000`)
#' @param control a named `list` of parameters to control the sampler's
#'   behavior.  Adding `max_treedepth` and giving a greater
#'   value than `10` can improve model convergence (default:
#'   `list(adapt_delta = 0.99)`).
#' @param stanvar_function stan code for the model (default:
#'   [sigmoid_stanvar()]
#' @param expose_functions `logical`. Expose the sigmoid function used in
#'   the model. This is needed e.g. for [brms::loo_compare()]
#' @param ... additional arguments passed to [brms::brm()]
#'
#' @returns `bpfit` object, which is a wrapper around a [brms::brmsfit] object.
#'
#' @examples
#' \dontrun{
#'   BayesPharma::sigmoid_agonist_model(
#'     data = data,
#'     formula = BayesPharma::sigmoid_agonist_formula(predictors = 0 + drug))
#' }
#' @export
sigmoid_agonist_model <- function(
  data,
  formula = sigmoid_agonist_formula(),
  prior = sigmoid_agonist_prior(),
  init = sigmoid_agonist_init(),
  iter = 8000,
  control = list(adapt_delta = 0.99),
  stanvar_function = sigmoid_stanvar,
  expose_functions = TRUE,
  ...) {

  if (!inherits(formula, "bpformula")) {
    warning(
      "formula must be a 'bpformula'. You can use the ",
      "'BayesPharma::sigmoid_agonist_formula(...)'")
  }

  if (!(formula$bayes_pharma_info[["treatment_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The treatment variable ",
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
      "prior must be a 'brmsprior'. You can use the ",
      "'BayesPharma::sigmoid_agonist_prior(...)'")
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

  model$bayes_pharma_info <- list(model_type = "sigmoid_agonist")

  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharma_info))

  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  class(model) <- c("bpfit", class(model))
  model
}





#' Run Bayesian Sigmoid Antagonist Model
#'
#' @description Fits the sigmoid antagonist model. The functional form is
#'  \preformatted{
#'    <response> ~ sigmoid(ic50, hill, top, bottom, <treatment>)}
#'  where
#'  \preformatted{
#'    sigmoid = bottom + (top - bottom)/(1 + 10^((ic50 - <treatment>) * hill))}
#'  By default the observed data are
#'  \itemize{
#'    \item{**<treatment>**: `log_dose`, the `log10` of the dose
#'      as a molar concentration}
#'    \item{**<response>**: `response`, with unspecified units}
#'  }
#'  and the modeled parameters are
#'  \itemize{
#'    \item{**ic50**: the dose where the response reaches half maximal
#'      activity}
#'    \item{**hill**: the hill coefficient controlling the slope at the
#'      `ic50`}, by convention the slope of an antagonist is negative.
#'    \item{**top**: the response when <treatment> => `-Inf`}
#'    \item{**bottom**: the response when <treatment> => `Inf`}
#'  }
#'  To configure the model you can use the following helper functions:
#'  \itemize{
#'    \item{[sigmiod_antagonist_formula()]: define the
#'      `treatment` and `response` variables, and predictors for the
#'      model parameters}
#'    \item{[sigmoid_antagonist_prior()]: define the prior for the
#'      model parameters}
#'    \item{[sigmoid_antagonist_init()]: define initial values for the
#'      model parameters}
#'  }
#'
#' @param data `data.frame` of experimental data. It must contain columns
#'   for the treatment, response and any additional predictors specified in the
#'   formula. See [sigmoid_antagonist_formula()] for more details.
#' @param formula `bpformula` for the sigmoid antagonist model
#'   (Default: [sigmoid_antagonist_formula()])
#' @param prior `brmsprior` defining a distribution for the model
#'   parameters `ic50`, `hill`, `top`, and `bottom`.
#'   Use one of the priors functions provided to create priors to
#'   use here.  (default: [sigmoid_antagonist_prior()]
#' @param init `function` returning a named `list` of functions
#'   returning arrays giving initial values of the parameters being modeled for
#'   each chain. (Default: [sigmoid_antagonist_init()]
#' @param iter `number` of iterations the model runs. Increasing
#'   `iter` can help with model convergence. (Default: `8000`)
#' @param control a named `list` of parameters to control the sampler's
#'   behavior.  Adding `max_treedepth` and giving a greater
#'   value than `10` can improve model convergence (default:
#'   `list(adapt_delta = 0.99)`).
#' @param stanvar_function stan code for the model (default:
#'   [sigmoid_stanvar()])
#' @param expose_functions `logical`. Expose the BayesPharma functions for
#'   the model. This is needed e.g. for [brms::loo_compare()]
#'   (Default: `TRUE`]
#' @param ... additional arguments passed to [brms::brm()]
#'
#' @returns `bpfit` object, which is a wrapper around a
#'   [brms::brmsfit-class()].
#'
#' @examples
#'\dontrun{
#'   BayesPharma::sigmoid_antagonist_model(
#'     data = data,
#'     formula = BayesPharma::sigmoid_antagonist_formula(predictors = 0 + drug))
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
    expose_functions = TRUE,
    ...) {

  if (!inherits(formula, "bpformula")) {
    warning(
      "formula must be a 'bpformula'. You can use the ",
      "'BayesPharma::sigmoid_antagonist_formula(...)' prior function")
  }

  if (!(formula$bayes_pharma_info[["treatment_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The treatment variable ",
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
      "prior must be a 'brmsprior'. You can use the ",
      "'BayesPharma::sigmoid_antagonist_prior(...)' function.")
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

  model$bayes_pharma_info <- list(
    model_type = "sigmoid_antagonist")

  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharma_info))

  # this is needed e.g. for brms::loo_compare()
  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  class(model) <- c("bpfit", class(model))
  model
}
