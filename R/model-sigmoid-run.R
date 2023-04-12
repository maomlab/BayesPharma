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
#'    \item{\strong{<treatment>}: \code{log_dose}, the \code{log10} of the dose
#'      as a molar concentration}
#'    \item{\strong{<response>}: \code{response}, with unspecified units}
#'  }
#'      
#'  and the modeled parameters are
#'  \itemize{
#'    \item{\strong{ec50}: the dose where the response reaches half maximal
#'      activity}
#'    \item{\strong{hill}: the hill coefficient controlling the slope at the
#'      \code{ec50}}, by convention the slope of an agonist is positive
#'    \item{\strong{top}: the response when <treatment> => \code{Inf}}
#'    \item{\strong{bottom}: the response when <treatment> => \code{-Inf}}
#'  }
#'  To configure the model you can use the following helper functions:
#'  \itemize{
#'    \item{\code{\link{sigmiod_agonist_formula}}: define the \code{treatment}
#'      and \code{response} variables, and predictors for the model parameters}
#'    \item{\code{\link{sigmoid_agonist_prior}}: define the prior for the model
#'      parameters}
#'    \item{\code{\link{sigmoid_agonist_init}}: define initial values for the
#'      model parameters}
#'  }
#'
#' @param data \code{data.frame} of observed data. It must contain columns
#'   for the treatment, response and any additional predictors specified in the
#'   formula. See \code{\link{sigmoid_agonist_formula}} for more details.
#' @param formula \code{bpformula} for the sigmoid agonist model
#'   (Default: \code{\link{sigmoid_agonist_formula}()})
#' @param prior \code{brmsprior} defining a distribution for the model
#'   parameters \code{ec50}, \code{hill}, \code{top}, and \code{bottom}.
#'   Use one of the priors functions provided to create priors to
#'   use here.  (default: \code{\link{sigmoid_agonist_prior}()}
#' @param init \code{function} returning a named \code{list} of functions
#'   returning arrays giving initial values of the parameters being modeled for
#'   each chain. (Default: \code{\link{sigmoid_agonist_init}()}
#' @param iter \code{number} of iterations the model runs. Increasing
#'   \code{iter} can help with model convergence. (Default: \code{8000})
#' @param control a named \code{list} of parameters to control the sampler's
#'   behavior.  Adding \code{max_treedepth} and giving a greater
#'   value than \code{10} can improve model convergence (default:
#'   \code{list(adapt_delta = 0.99)}).
#' @param stanvar_function stan code for the model (default:
#'   \code{BayesPharma::sigmoid_stanvar})
#' @param expose_functions \code{logical}. Expose the sigmoid function used in
#'   the model. This is needed e.g. for \code{\link[brms]{loo_compare}}
#' @param ... additional arguments passed to \code{\link[brms]{brm}}
#'
#' @returns \code{bpfit} object, which is a wrapper around a
#'   \code{\link[brms]{brmsfit-class}}.
#'
#' @examples
#'\dontrun{
#'   BayesPharma::sigmoid_agonist_model(
#'     data = data,
#'     formula = BayesPharma::sigmoid_agonist_formula(predictors = 0 + drug))
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
  expose_functions = TRUE,
  ...) {

  if (!methods::is(formula, "bpformula")) {
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

  if (!methods::is(prior, "brmsprior")) {
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

  model$bayes_pharma <- list(model_type = "sigmoid_agonist")

  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharam_info))

  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

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
#'    \item{\strong{<treatment>}: \code{log_dose}, the \code{log10} of the dose
#'      as a molar concentration}
#'    \item{\strong{<response>}: \code{response}, with unspecified units}
#'  }
#'  and the modeled parameters are
#'  \itemize{
#'    \item{\strong{ic50}: the dose where the response reaches half maximal
#'      activity}
#'    \item{\strong{hill}: the hill coefficient controlling the slope at the
#'      \code{ic50}}, by convention the slope of an antagonist is negative.
#'    \item{\strong{top}: the response when <treatment> => \code{-Inf}}
#'    \item{\strong{bottom}: the response when <treatment> => \code{Inf}}
#'  }
#'  To configure the model you can use the following helper functions:
#'  \itemize{
#'    \item{\code{\link{sigmiod_antagonist_formula}}: define the
#'      \code{treatment} and \code{response} variables, and predictors for the
#'      model parameters}
#'    \item{\code{\link{sigmoid_antagonist_prior}}: define the prior for the
#'      model parameters}
#'    \item{\code{\link{sigmoid_antagonist_init}}: define initial values for the
#'      model parameters}
#'  }
#'
#' @param data \code{data.frame} of experimental data. It must contain columns
#'   for the treatment, response and any additional predictors specified in the
#'   formula. See \code{\link{sigmoid_antagonist_formula}} for more details.
#' @param formula \code{bpformula} for the sigmoid antagonist model
#'   (Default: \code{\link{sigmoid_antagonist_formula}()})
#' @param prior \code{brmsprior} defining a distribution for the model
#'   parameters \code{ic50}, \code{hill}, \code{top}, and \code{bottom}.
#'   Use one of the priors functions provided to create priors to
#'   use here.  (default: \code{\link{sigmoid_antagonist_prior}()}
#' @param init \code{function} returning a named \code{list} of functions
#'   returning arrays giving initial values of the parameters being modeled for
#'   each chain. (Default: \code{\link{sigmoid_antagonist_init}()}
#' @param iter \code{number} of iterations the model runs. Increasing
#'   \code{iter} can help with model convergence. (Default: \code{8000})
#' @param control a named \code{list} of parameters to control the sampler's
#'   behavior.  Adding \code{max_treedepth} and giving a greater
#'   value than \code{10} can improve model convergence (default:
#'   \code{list(adapt_delta = 0.99)}).
#' @param stanvar_function stan code for the model (default:
#'   \code{\link{sigmoid_stanvar}})
#' @param expose_functions \code{logical}. Expose the BayesPharma functions for
#'   the model. This is needed e.g. for \code{\link[brms]{loo_compare}}
#'   (Default: \code{TRUE}]
#' @param ... additional arguments passed to \code{\link[brms]{brm}}
#'
#' @returns \code{bpfit} object, which is a wrapper around a
#'   \code{\link[brms]{brmsfit-class}}.
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

  if (!methods::is(formula, "bpformula")) {
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

  if (!methods::is(prior, "brmsprior")) {
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
    list(formula_info = formula$bayes_pharam_info))

  # this is needed e.g. for brms::loo_compare()
  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  class(model) <- c("bpfit", class(model))

  model
}
