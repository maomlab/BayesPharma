#' Model for the Michaelis Menten Enzyme Kinetics Model
#'
#' @param data `data.frame` of experimental data.
#'   must contain columns `time` and `P` and any predictors specified in
#'   the formula.
#' @param formula [brms::brmsformula] object. To create a dose-response
#'   [brms::brmsformula], use the [michaelis_menten_formula] function.
#' @param prior [brms::brmsprior] for `kcat`, and `kM`. Use
#'   [michaelis_menten_formula()] to create priors to be used here.
#' @param init `list` of `lists`, `numeric` value, or "random" for the initial
#'   values of the parameters being modeled.
#' @param iter `numeric` of iterations the model runs. Increasing `iter` can
#'   help with model convergence.
#' @param control a named `list` of parameters to control the sampler's
#'   behavior. Adding `max_treedepth` and giving a greater value than `10` can
#'   improve model convergence.
#' @param stanvar_function stan code for the model
#' @param expose_functions `logical`. Expose the BayesPharma functions for the
#'   model
#' @param ... additional arguments passed to [brms::brm()].
#'
#' @returns `bpfit` object, which is a wrapper around a [brms::brmsfit] object.
#'
#' @seealso [michaelis_menten_formula], [michaelis_menten_prior],
#'   [michaelis_menten_init], or [michaelis_menten_stanvar]
#'
#' @references
#' Choi, B., Rempala, G.A. & Kim, J.K. Beyond the Michaelis-Menten equation:
#' Accurate and efficient estimation of enzyme kinetic parameters. Sci Rep 7,
#' 17018 (2017). https://doi.org/10.1038/s41598-017-17072-z
#'
#' @export
michaelis_menten_model <- function(
  data,
  formula = michaelis_menten_formula(),
  prior = michaelis_menten_prior(),
  init = michaelis_menten_init(),
  iter = 8000,
  control = list(adapt_delta = 0.99),
  stanvar_function = c(
    BayesPharma::michaelis_menten_stanvar(),
    BayesPharma::michaelis_menten_genquant()),
  expose_functions = TRUE,
  ...) {

  args <- list(...)

  if (!(
    formula$bayes_pharma_info[["series_index_variable"]] %in% names(data))) {
    stop(
      paste0(
        "The series index ",
        "'", formula$bayes_pharma_info[["series_index_variable"]], "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }

  time_variable <- formula$bayes_pharma_info[["treatment_variable"]]
  if (!(time_variable %in% names(data))) {
    stop(
      paste0(
        "The treatment (time) variable '", time_variable, "' ",
        "needs to be a column of the input 'data' data.frame\n"))
  }

  time_type <- data[[time_variable]] |> names() |> class()
  if (time_type != "numeric") {
    stop(paste0(
      "The treatment (time) variable '", time_variable, "' ",
      "needs to be a 'numeric' type. ",
      "Instead it is of type '", time_type, "'"))
  }

  if (!(formula$bayes_pharma_info[["ET_variable"]] %in% names(data))) {
    stop(paste0(
      "The enzyme concentration variable ",
      "'", formula$bayes_pharma_info[["ET_variable"]], "' ",
      "needs to be a column of the input 'data' data.frame\n"))
  }

  if (!(formula$bayes_pharma_info[["ST_variable"]] %in% names(data))) {
    stop(paste0(
      "The substrate concentration variable ",
      "'", formula$bayes_pharma_info[["ST_variable"]], "' ",
      "needs to be a column of the input 'data' data.frame\n"))
  }

  if (!(formula$bayes_pharma_info[["response_variable"]] %in% names(data))) {
    stop(paste0(
      "The response variable ",
      "'", formula$bayes_pharma_info[["response_variable"]], "' ",
      "needs to be a column of the input 'data' data.frame\n"))
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

  model$bayes_pharma_info <- list(model_type = "michaelis_menten")

  if ("bayes_pharma_info" %in% names(formula)) {
    model$bayes_pharma_info <- c(
      model$bayes_pharma_info,
      list(formula_info = formula$bayes_pharma_info))
  }

  # this is needed e.g. for brms::loo_compare()
  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  class(model) <- c("bpfit", class(model))
  model
}
