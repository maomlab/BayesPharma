


prepare_init <- function(init) {
  if (is(init, "function")) {
    x <- init()
    assertthat::assert_that(is(x, "array"))
    assertthat::assert_that(dim(x) == 1)
  } else if (is.numeric(init)) {
    init <- function() {as.array(init)}
  } else {
    error(paste0(
      "Initialization should either be a function that returns an array of
      length one or numeric"))
  }
  init
}

#' Prepare a brmsprior
#' 
#' What this gives above brms::prior(...) and friends is
#'   1) it allow just giving a numeric value rather than constant(<value>)
#'   2) if it is already a brmsprior it checks that it has specified arguments
#'   
#' This is used in models where want to allow user specified priors but
#' make sure they are for the right parameters to make sure the model is well
#' specified.
prepare_prior <- function(prior, ...) {
  if (is(prior, "brmsprior")){
    args <- list(...)
    for (arg in names(args)) {
      assertthat::assert_that(prior[[arg]] == args[[arg]])
    }
  } else if (is.numeric(prior)){
    prior <- brms::prior_string(
      prior = paste0("constant(", prior, ")"),
      ...)
  } else {
    error("prior must be a brms::prior(...) or a numeric value")
  }
  prior
}