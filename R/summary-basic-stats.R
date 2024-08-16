#' Give Basic Statistical Information About the Model Results
#'
#' @description Return a `data.frame` containing summary statistics of
#'   `bpfit` model. The summary statistics included are mean, median,
#'   standard deviation, lower confidence interval, and upper confidence
#'   interval.
#'
#' @param model [brms::brmsfit] object
#' @param exclude_vars list of regular expressions to match what variables to
#'   exclude.
#' @param l_ci `numeric` unit of the lower confidence interval
#' @param u_ci `numeric` unit of the upper confidence interval
#' @returns [tibble::tibble()] object.
#'
#' @examples
#'\dontrun{
#'   BayesPharma::basic_stats(
#'     model = my_sigmoid_model,
#'     l_ci = 0.025,
#'     u_ci = 0.975)
#'}
#'
#' @importFrom rlang .data
#' @export
basic_stats <- function(
  model,
  exclude_vars = c("__$", "lprior"),
  l_ci = 0.025,
  u_ci = 0.975) {

  model |>
    posterior::summarise_draws(
      "mean", "sd", "median",
      ~quantile(.x, probs = c(l_ci, u_ci))) |>
    dplyr::rename(
      l_ci = 4,
      u_ci = 5) |>
    dplyr::filter(
      !stringr::str_detect(
        .data[["variable"]],
        paste0("(", paste0(exclude_vars, collapse = "|"), ")")))

}
