#' Fit a Bayesian sigmoid model
#'
#' @description Fits the sigmoid model. The functional form is
#'  \preformatted{
#'    <response> ~ sigmoid(ac50, hill, top, bottom, <treatment>)}
#'  where
#'  \preformatted{
#'    sigmoid = bottom + (top - bottom)/(1 + 10^((ac50 - <treatment>) * hill))}
#'  By default the observed data (and therefore should be columns in the input
#'  data `data.frame`) are
#'  \itemize{
#'    \item{**<treatment>**: `log_dose`, the `log10` of the dose as a molar
#'      concentration}
#'    \item{**<response>**: `response`, with unspecified units}
#'  }
#'  and the modeled parameters are
#'  \itemize{
#'    \item{**ac50**: the dose where the response reaches half maximal
#'      activity}
#'    \item{**hill**: the hill coefficient controlling the slope at the `ac50`,
#'      by convention the slope of an agonist is positive}
#'    \item{**top**: the response when <treatment> => `Inf`}
#'    \item{**bottom**: the response when <treatment> => `-Inf`}
#'  }
#'  To configure the model you can use the following helper functions for
#'  agonist models
#'  \itemize{
#'    \item{[sigmoid_agonist_formula()]: define how the `response` relates to
#'      the `treatment` and the parameters with `ac50`->`ec50`, and how any
#'      covariates predict the parameters}
#'    \item{[sigmoid_agonist_prior()]: define the prior for the model
#'      parameters, with the slope constrained to be positive}
#'    \item{[sigmoid_agonist_init()]: define initial values for the
#'      model parameters to match the default [sigmoid_agonist_prior()]}
#'  }
#'  the following helper functions for an antagonist model
#'  \itemize{
#'    \item{[sigmoid_antagonist_formula()]: define how the `response` relates to
#'      the `treatment` and the parameters with `ac50`->`ic50`, and how any
#'      covariates predict the parameters}
#'    \item{[sigmoid_antagonist_prior()]: define the prior for the
#'      model parameters, with the slope being constrained be negative}
#'    \item{[sigmoid_antagonist_init()]: define initial values for the
#'      model parameters to match the default [sigmoid_antagonist_prior()]}
#'  }
#'
#' @param data `data.frame` of observed data. It must contain columns
#'   for the treatment, response and any additional predictors specified in the
#'   formula. See [sigmoid_agonist_formula()] for more details.
#' @param formula `bpformula` formula for the sigmoid model (see above).
#' @param prior `brmsprior` defining a distribution for the model
#'   parameters defined in the formula. (see above).
#' @param init `function` to initialize [brms::brm] model for for the parameters
#'   (see above)
#' @param iter `numeric` of iterations the model runs. Increasing
#'   `iter` can help with model convergence (see [brms::brm])
#' @param control a named `list` of arguments to control the sampler's
#'   behavior.  Adding `max_treedepth` and giving a greater
#'   value than `10` can improve model convergence (see [brms::brm])
#' @param stanvar_function stan code for the model.
#' @param expose_functions `logical`. Expose the sigmoid function used in
#'   the model. This is needed e.g. for [brms::loo_compare()]
#' @param ... additional arguments passed to [brms::brm()]
#'
#' @returns `bpfit` object, which is a wrapper around a [brms::brmsfit] object.
#'
#' @examples
#' \dontrun{
#'   BayesPharma::sigmoid_model(
#'     data = data,
#'     formula = BayesPharma::sigmoid_antagoinst_formula(predictors = 0 + drug),
#'     prior = BayesPharma::sigmoid_antagonist_prior(),
#'     init = BayesPharma::sigmoid_antagonist_init())
#' }
#' @export
sigmoid_model <- function(
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
      "'BayesPharma::sigmoid_agonist_formula(...)' or ",
      "'BayesPharma::sigmoid_antagoinst_formula(...)'")
  }

  if (!(formula$bayes_pharma_info[["treatment_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The treatment variable ",
        "'", formula$bayes_pharma_info[["treatment_variable"]], "' defined in ",
        " the formula needs to be a column of the input 'data' data.frame\n"))
  }

  if (!(formula$bayes_pharma_info[["response_variable"]] %in% names(data))) {
    warning(
      paste0(
        "The response variable ",
        "'", formula$bayes_pharma_info[["response_variable"]], "' defined in ", 
        "the formula needs to be a column of the input 'data' data.frame\n"))
  }

  if (!inherits(prior, "brmsprior")) {
    warning(
      "prior must be a 'brmsprior'. You can use the ",
      "'BayesPharma::sigmoid_agonist_prior(...)' or ",
      "'BayesPharma::sigmoid_antagoinst_prior(...)")
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

  model$bayes_pharma_info <- list(model_type = "sigmoid")

  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharma_info))

  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  class(model) <- c("bpfit", class(model))
  model
}