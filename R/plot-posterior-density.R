#' Plot Posterior Density Distributions of modeled parameters
#'
#' @description plots the [bpfit()] model density distribution of each
#'   parameter and the mean, lower confidence interval, and upper confidence
#'   interval.
#'
#' @param model `bpfit` model.
#' @param predictors_col_name `character` of the predictors column in the input
#'   `data.frame`. Predictors are the perturbations tested during the experiment
#'   (i.e. Drug, Temperature, etc.).
#' @param half_max_label `character` of the label for the half maximal that
#'   fits the type of experiment that was done (i.e. ec50, ic50, ed50, id50,
#'   ld50, etc.).
#' @param l_ci `numeric` unit of the lower confidence interval
#' @param u_ci `numeric` unit of the upper confidence interval
#' @param title_label string of the plot title.
#'
#' @returns [ggplot2::ggplot()] object.
#'
#' @examples
#'\dontrun{
#'   plot_posterior_density(
#'     model = my_sigmoid_model,
#'     predictors_col_name = "predictors",
#'     half_max_label = "ic50",
#'     l_ci = 0.025,
#'     u_ci = 0.975,
#'     title_label = "Posterior Density Plots with Mean and 95% CI")
#'}
#' @importFrom rlang .data
#'@export
plot_posterior_density <- function(
  model,
  predictors_col_name = "_Intercept",
  half_max_label = "ac50",
  l_ci = 0.025,
  u_ci = 0.975,
  title_label = "Posterior Density Plots w/ Mean & 95% CI") {

  assertthat::assert_that(
    inherits(model, "bpfit"),
    msg = "Model should be model fit with BayesPharma")
  assertthat::is.number(l_ci)
  assertthat::is.number(u_ci)

  posterior <- dplyr::bind_rows(
    model |>
      tidybayes::tidy_draws() |>
      tidybayes::gather_variables() |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "__$")) |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "sigma")) |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "lprior"))) |>
    dplyr::rename(variables = tidyselect::all_of(".variable")) |>
    dplyr::mutate(
      variables = .data[["variables"]] |>
        stringr::str_extract("b_[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_remove("b_") |>
        stringr::str_extract("[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_remove(predictors_col_name) |>
        stringr::str_extract("[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_replace("ec50", half_max_label))

  summary_stats <- basic_stats(
    model = model,
    predictors_col_name = predictors_col_name,
    half_max_label = half_max_label,
    l_ci = l_ci,
    u_ci = u_ci)

  ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      data = posterior,
      mapping = ggplot2::aes(x = .data[[".value"]]),
      fill = "cyan2",
      color = "black",
      alpha = .9) +
    ggplot2::geom_vline(
      data = summary_stats,
      ggplot2::aes(xintercept = .data[["mean"]]),
      color = "red") +
    ggplot2::geom_rect(
      data = summary_stats,
      mapping = ggplot2::aes(xmin = -Inf, xmax = l_ci, ymin = -Inf, ymax = Inf),
      color = "gray",
      alpha = 0.5) +
    ggplot2::geom_rect(
      data = summary_stats,
      ggplot2::aes(xmin = u_ci, xmax = Inf, ymin = -Inf, ymax = Inf),
      color = "gray",
      alpha = 0.5) +
    ggplot2::ggtitle(label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(.data[["variables"]]),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_discrete("Distribution")
}
