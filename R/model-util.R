#' Prepare an init for a brms model
#'
#' @param init function returning an numeric array of length one or a
#'     numeric value.
#' @returns function returning a numeric array of length 1.
prepare_init <- function(init) {
  if (methods::is(init, "function")) {
    # init is a function, check that it returns a numeric array of dimension 1
    x <- init()
    assertthat::assert_that(methods::is(x, "array"))
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
#' What this gives above \code{brms::prior(...)} and friends is
#'   1) it allow just giving a numeric value rather than constant(<value>)
#'   2) if it is already a \code{brms::brmsprior} it checks that it has
#'      specified arguments
#'
#' This is used in models where want to allow user specified priors
#' but make sure they are for the right parameters to make sure the
#' model is well specified.
#'
#' @param prior \code{brms::brmsprior} or numeric.
#' @param ... additional arguments to \code{brms::prior_string()}. If
#'     \code{prior} is a \code{brmsprior} then this will check that
#'     the slots have the given values.  If prior is numeric, then
#'     these arguments are passed to \code{brms::prior_string}
#'
#' @returns brmsprior
prepare_prior <- function(prior, ...) {
  if (methods::is(prior, "brmsprior")) {
    args <- list(...)
    for (arg in names(args)) {
      assertthat::assert_that(prior[[arg]] == args[[arg]])
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
