#' Plot Posterior Density Distributions of modeled parameters
#'
#' @description plots the [bpfit()] model density distribution of each
#'   parameter and the mean, lower confidence interval, and upper confidence
#'   interval.
#'
#' @param model `bpfit` model.
#' @param exclude_variables list of regular expressions to match what variables
#'   to exclude.
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
#'     l_ci = 0.025,
#'     u_ci = 0.975,
#'     title_label = "Posterior Density Plots with Mean and 95% CI")
#'}
#' @importFrom rlang .data
#'@export
plot_posterior_density <- function(
  model,
  exclude_variables = c("__$", "lprior"),
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
      dplyr::filter(
        !stringr::str_detect(
          .data[[".variable"]],
          paste0("(", paste0(exclude_variables, collapse = "|"), ")")))) |>
    dplyr::rename(variable = tidyselect::all_of(".variable"))
    
  summary_stats <- basic_stats(
    model = model,
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
    ggplot2::geom_rect(
      data = summary_stats,
      ggplot2::aes(xmin = u_ci, xmax = Inf, ymin = -Inf, ymax = Inf),
      color = "gray",
      alpha = 0.5) +
    ggplot2::ggtitle(label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(.data[["variable"]]),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_discrete("Distribution")
}
