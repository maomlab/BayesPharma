#' Helper function to prepare an init for a brms model
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

#' Prepare a brmsprior
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
#'   user should specify a prior for hill
#'   user_hill_prior <- brms::prior(
#'     prior = normal(1, 1),
#'     nlpar = "hilll",           # misspells hill (!)
#'     ub = 0)
#'
#'   # in a script where we want to validate the user_hill_prior
#'   hill_prior <- BayesPharma:::prepare_prior(
#'     prior = user_hill_prior
#'     nlpar = "hill)
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
