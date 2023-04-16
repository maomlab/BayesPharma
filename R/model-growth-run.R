#' Run Bayesian sigmoid growth model
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   <https://paul-buerkner.github.io/brms/reference/brm.html>
#'   or <https://rdrr.io/cran/rstan/man/stan.html>
#'
#' @param data \code{data.frame} of experimental data. Must contain column
#'   representing the treatment, response and predictors specified in the
#'   formula.
#' @param formula \code{bpformula} object. To create a growth model formula, use
#'   \code{BayesPharma::growth_sigmoid_formula} (default:
#'   \code{BayesPharma::growth_sigmoid_formula()}.
#' @param prior \code{brmspriors} \code{data.frame} for \code{K}, \code{K0},
#'   \code{rate}, and \code{lambda}. To create a prior, use
#'   \code{BayesPharma::growth_sigmoid_prior}. (default:
#'   \code{BayesPharma::growth_sigmoid_prior()}
#' @param init initial values of the parameters being modeled. To create an
#'   init, use \code{BayesPharma::growth_sigmoid_init()}. (Default:
#'   \code{BayesPharma::growth_sigmoid_init()}
#' @param iter \code{numeric} value for the number of iterations the model runs. 
#'   Increasing \code{iter} can help with model convergence (Default:
#'   \code{8000}).
#' @param control a named \code{list} of parameters to control the sampler's
#'   behavior. Adding \code{max_treedepth} and giving a greater value than
#'   \code{10} can improve model convergence (Default: 
#'   \code{list(adapt_delta = 0.99)}).
#' @param stanvar_function Stan code for the model (default:
#'   \code{BayesPharma::growth_richards_stanvar}).
#' @param expose_functions \code{logical}. Expose the stan functions for the
#'   model. This is needed e.g. to run \code{brms::loo_compare()}. (Default:
#'   \code{TRUE}). 
#' @param ... additional arguments passed to \code{brms::brm}
#'
#' @returns \code{bpfit} \code{brmsfit}
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
    stanvar_function = growth_richards_stanvar,
    expose_functions = TRUE,
    ...) {
  
  if (!methods::is(formula, "bpformula")) {
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
  
  if (!methods::is(prior, "brmsprior")) {
    warning(
      "prior must be a 'brmsprior'. Use either the ",
      "'BayesPharma::growth_sigmoid_prior(...)'")
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
  
  model$bayes_pharma <- list(model_type = "growth_sigmoid")
  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharma_info))
  
  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }
  
  model
}


#' Run Bayesian Richards growth model
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   <https://paul-buerkner.github.io/brms/reference/brm.html>
#'   or <https://rdrr.io/cran/rstan/man/stan.html>
#'
#' @param data \code{data.frame} of experimental data. Must contain column
#'   representing the treatment, response and predictors specified in the
#'   formula.
#' @param formula \code{bpformula} object. To create a growth model formula, use
#'   \code{BayesPharma::growth_richards_formula} (default:
#'   \code{BayesPharma::growth_richards_formula()}.
#' @param prior \code{brmspriors} \code{data.frame} for \code{K}, \code{K0},
#'   \code{rate}, \code{lambda}, and \code{nu}. To create a prior, use
#'   \code{BayesPharma::growth_richards_prior}. (default:
#'   \code{BayesPharma::growth_richards_prior()}
#' @param init initial values of the parameters being modeled. To create an
#'   init, use \code{BayesPharma::growth_richards_init()}. (Default:
#'   \code{BayesPharma::growth_richards_init()}
#' @param iter \code{numeric} value for the number of iterations the model runs. 
#'   Increasing \code{iter} can help with model convergence (Default:
#'   \code{8000}).
#' @param control a named \code{list} of parameters to control the sampler's
#'   behavior. Adding \code{max_treedepth} and giving a greater value than
#'   \code{10} can improve model convergence (Default: 
#'   \code{list(adapt_delta = 0.99)}).
#' @param stanvar_function Stan code for the model (default:
#'   \code{BayesPharma::growth_richards_stanvar}).
#' @param expose_functions \code{logical}. Expose the stan functions for the
#'   model. This is needed e.g. to run \code{brms::loo_compare()}. (Default:
#'   \code{TRUE}). 
#' @param ... additional arguments passed to \code{brms::brm}
#'
#' @returns \code{bpfit} \code{brmsfit}
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
    stanvar_function = growth_richards_stanvar,
    expose_functions = TRUE,
    ...) {

  if (!methods::is(formula, "bpformula")) {
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
