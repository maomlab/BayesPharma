#' Check if an Object is a [ggplot2] Plot
#'
#' @description Use in tests to check if an object is a [ggplot2::ggplot] object
#'
#' @note borrowed from `bayesplot:::expect_gg`
#'
expect_gg <- function(x) {
  testthat::expect_s3_class(x, "ggplot")
  invisible(ggplot2::ggplot_build(x))
}
