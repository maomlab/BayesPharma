#' Helper Function to Prepare an Init for a [brms] Model
#'
#'
#' @param init `function` returning an `numeric` `array` of length `1` or a
#'   `numeric` value.
#' @returns `function` returning a `numeric` `array` of length `1`.
#'
#'
prepare_init <- function(init) {
  if (inherits(init, "function")) {
    # init is a function, check that it returns a numeric array of dimension 1
    x <- init()
    assertthat::assert_that(inherits(x, "array"))
    assertthat::assert_that(dim(x) == 1)
    assertthat::assert_that(is.numeric(x))
    init_fn <- init
  } else if (is.numeric(init)) {
    init_fn <- function() {
      as.array(init)
    }
  } else {
    stop(paste0(
      "Initialization should either be a function that returns an array of
      length one or numeric"))
  }
  init_fn
}

#' Helper Function to Prepare a Prior for a [brms] Model
#'
#' This extends [brms::prior()] by
#'   1) allowing just taking a `numeric` value rather than `constant(<value>)`
#'      to specify a constant prior
#'   2) if [brms::brmsprior] is given, it checks that it has the specified
#'      arguments
#'
#' This is used in building [BayesPharma] models to allow user specified priors
#' but make sure they are for the right parameters to make sure the model is
#' well specified.
#'
#' @param prior [brms::brmsprior()] or `numeric`.
#' @param ... additional arguments to [brms::prior_string()]. If `prior` is a
#'   [brms::brmsprior()] then this will check that the slots have the given
#'   values. If prior is `numeric`, then these arguments are passed to
#'   [brms::prior_string()]
#'
#'
#' @examples
#' \dontrun{
#'   # user should specify a prior for hill, but they misspell it:
#'   user_hill_prior <- brms::prior(
#'     prior = normal(1, 1),
#'     nlpar = "hilll",
#'     ub = 0)
#'
#'   # in a script where we want to validate the user_hill_prior
#'   hill_prior <- BayesPharma:::prepare_prior(
#'     prior = user_hill_prior,
#'     nlpar = "hill")
#'
#'   # gives an assert error that nlpar is not set correctly
#' }
#'
#'
#'
#' @returns [brms::brmsprior()]
prepare_prior <- function(prior, ...) {
  if (inherits(prior, "brmsprior")) {
    args <- list(...)
    for (arg in names(args)) {
      assertthat::assert_that(
        prior[[arg]] == args[[arg]],
        msg = paste0(
          "In the given prior, the field '", arg, "' is expected to be '",
          args[[arg]], "', but instead it is '", prior[[arg]], "'"))
    }
  } else if (is.numeric(prior)) {
    prior <- brms::prior_string(
      prior = paste0("constant(", prior, ")"),
      ...)
  } else {
    stop("prior must be a brms::prior(...) or a numeric value")
  }
  prior
}



#' Get the Treatment Variable from a BayesPharma Model
#' 
#' @param model `bpfit` object resulting from fitting a model with one of the
#'   model functions from the [BayesPharma] package.
#'   
#' @returns `character` with the treatment variable
#' 
#' @export
get_treatment_variable <- function(model) {
  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "plot_posterior_draws expects model to be of class 'bpfit',",
      " instead it is of class ", class(model)))
  }
  
  treatment_variable <- model$bayes_pharma_info$treatment_variable

  if (is.null(treatment_variable)) {
    stop(paste0(
      "Expected treatment_variable to be defined in the ",
      "model$bayes_pharma_info"))
  }

  if (!(treatment_variable %in% names(model$data))) {
    stop(paste0(
      "Expected the treatment variable '", treatment_variable, "' to be a ",
      "column in the model$data, but instead it has columns ",
      "[", paste0(names(model$data), collapse = ", "), "]"))
  }
  
  treatment_variable
}

#' Get the Treatment Variable Units from a BayesPharma Model
#' 
#' @param model `bpfit` object resulting from fitting a model with one of the
#'   model functions from the [BayesPharma] package.
#'   
#' @returns `character` with the treatment variable units
#' 
#' @export
get_treatment_units <- function(model) {
  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "plot_posterior_draws expects model to be of class 'bpfit',",
      " instead it is of class ", class(model)))
  }
  
  treatment_units <- model$bayes_pharma_info$treatment_units
  
  if (is.null(treatment_units)) {
    stop(paste0(
      "Expected treatment_units to be defined in the ",
      "model$bayes_pharma_info"))
  }
  
  if (!(treatment_units %in% names(model$data))) {
    stop(paste0(
      "Expected the treatment units '", treatment_units, "' to be a ",
      "column in the model$data, but instead it has columns ",
      "[", paste0(names(model$data), collapse = ", "), "]"))
  }
  
  treatment_units
}

#' Get the Response Variable from a BayesPharma Model
#' 
#' @param model `bpfit` object resulting from fitting a model with one of the
#'   model functions from the [BayesPharma] package.
#'   
#' @returns `character` with the response variable
#' 
#' @export
get_response_variable <- function(model) {
  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "plot_posterior_draws expects model to be of class 'bpfit',",
      " instead it is of class ", class(model)))
  }
  
  response_variable <- model$bayes_pharma_info$response_variable
  
  if (is.null(response_variable)) {
    stop(paste0(
      "Expected response_variable to be defined in the ",
      "model$bayes_pharma_info"))
  }
  
  if (!(response_variable %in% names(model$data))) {
    stop(paste0(
      "Expected the response variable '", response_variable, "' to be a ",
      "column in the model$data, but instead it has columns ",
      "[", paste0(names(model$data), collapse = ", "), "]"))
  }
  
  response_variable
}

#' Get the Response Variable Units from a BayesPharma Model
#' 
#' @param model `bpfit` object resulting from fitting a model with one of the
#'   model functions from the [BayesPharma] package.
#'   
#' @returns `character` with the response variable units
#' 
#' @export
get_response_units <- function(model) {
  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "plot_posterior_draws expects model to be of class 'bpfit',",
      " instead it is of class ", class(model)))
  }
  
  response_units <- model$bayes_pharma_info$response_units
  
  if (is.null(response_units)) {
    stop(paste0(
      "Expected response_units to be defined in the ",
      "model$bayes_pharma_info"))
  }
  
  if (!(response_units %in% names(model$data))) {
    stop(paste0(
      "Expected the response units '", response_units, "' to be a ",
      "column in the model$data, but instead it has columns ",
      "[", paste0(names(model$data), collapse = ", "), "]"))
  }
  
  response_units
}


