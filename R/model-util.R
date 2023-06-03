#' Helper Function to Prepare an Init for a [brms] Model
#'
#'
#' @param init `function` returning an `numeric` `array` of length `1` or a
#'   `numeric` value.
#' @returns `function` returning a `numeric` `array` of length `1`.
#'
#' @export
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

#' Helper Function to Create the Default `rstan` scalar init
#'
#' @description By default, \pkg{rstan} will initialize parameters uniformly at
#' random in the range (-2, 2), on the unconstrained scale. Description of how
#' stan transforms parameters to satisfy constraints is described in the stan
#' documentation
#' [https://mc-stan.org/docs/reference-manual/variable-transforms.html]
#'
#' This helper is especially useful for running models using the \pkg{cmdstanr}
#' backend, which requires all parameters (including distributional) parameters
#' to be initialized.
#'
#' @param lb `numeric` lower bound for parameter
#' @param ub `numeric` upper bound for parameter
#' @param dim `numeric` dimension of parameter.
#' @returns function that return the default brms initial value for a parameter.
#'   If `dim=0`, then it will be a numeric scalar, if `dim=1` or greater, than
#'   return an array with the given dimension.
#'
#' @seealso [rstan::stan]
#'
#' @examples
#' \dontrun{
#'
#' # Explicitly set the default initialization for the distributional parameter
#' # 'sigma' when family=gaussian().
#' init <- BayesPharma::sigmoid_antagonist_init(
#'   sigma = BayesPharma::rstan_default_scalar_init(lb = 0))
#'
#' }
#'
#' @export
rstan_default_init <- function(lb = NULL, ub = NULL, dim = 0) {
  if (dim == 0) {
    if (!is.null(lb) && !is.null(ub)) {
      # https://mc-stan.org/docs/reference-manual/logit-transform-jacobian.html
      return(\() brms::inv_logit_scaled(
        x = stats::runif(1, min = -2, max = 2),
        lb = lb,
        ub = ub))
    } else if (!is.null(lb)) {
      # https://mc-stan.org/docs/reference-manual/lower-bound-transform.html
      return(
        \() exp(stats::runif(1, min = -2, max = 2)) + lb)
    } else if (!is.null(ub)) {
      # https://mc-stan.org/docs/reference-manual/upper-bounded-scalar.html
      return(
        \() ub - exp(stats::runif(1, min = -2, max = 2)))
    } else {
      return(
        \() stats::runif(1, min = -2, max = 2))
    }
  } else {
    if (!is.null(lb) && !is.null(ub)) {
      # https://mc-stan.org/docs/reference-manual/logit-transform-jacobian.html
      return(\() array(
        brms::inv_logit_scaled(
          x = stats::runif(1, min = -2, max = 2),
          lb = lb,
          ub = ub),
        dim = dim))
    } else if (!is.null(lb)) {
      # https://mc-stan.org/docs/reference-manual/lower-bound-transform.html
      return(
        \() array(exp(stats::runif(1, min = -2, max = 2)) + lb, dim = dim))
    } else if (!is.null(ub)) {
      # https://mc-stan.org/docs/reference-manual/upper-bounded-scalar.html
      return(
        \() array(ub - exp(stats::runif(1, min = -2, max = 2)), dim = dim))
    } else {
      return(
        \() array(stats::runif(1, min = -2, max = 2), dim = dim))
    }
  }
}

#' Evaluate an init
#'
#' @description How brms models can be initialized depends on the backend. The
#' method that all backends supports is as a list (one for each chain) of lists
#' (one for each variable) with numeric values. Since this requires knowing
#' how many chains are being run, which may not be available when the model is
#' being defined, and to support random initialization, the rstan backend
#' also supports initialization as a function returning a list of functions
#' (one for each parameter) returning a numeric array of length 1. Also, to
#' suppor the common use-case of initializing everything to zero or randomly in
#' the range (-2, 2) on the unconstrained scale, rstan also supports
#' initializing with `0` and `"random"`.
#' 
#' To make BayesPharma more backend agnostic, this helper function takes the
#' an init and the number of chains and reduces it to the list of list format.
#'
#' @param init A valid init argument for [rstan::rstan].
#' @param chains `numeric` number of chains to generate initializations for
#' @returns `list` of `lists` form of model initialization.
#'
#' @export
eval_init <- function(init, chains = NULL) {
  if (is.null(chains)) {
    chains <- 4
  }
  if (is.null(init) || is.numeric(init) || is.character(init)) {
    # not sure if cmdstanr can support this type of initialization or not...
    return(init)
  } else if (isa(init, "list")) {
    if (length(init) != chains) {
      stop(paste0(
        "Init is given with '", length(init), "' elements, but the ",
        "number of chains requested is '", chains, "'", sep = ""))
    }
    return(init)
  } else if (isa(init, "function")) {
    # Assume evaluating the function returns a list of functions, one for each
    # parameter and evaluating each function returns a numeric array of length 1
    return(1:chains |> purrr::map(~init() |> purrr::map(~.x())))
  }
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
