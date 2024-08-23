#' Plot Prior and Posterior Marginal Distributions
#'
#' @description Generate a plot that shows for each model variable the
#' distribution of the marginal posterior uncertainty overlaid on the
#' distribution of the marginal prior uncertainty.
#'
#' @param model `bpfit` object resulting from fitting a BayesPharma model
#' @param exclude_variables `string` vector of variables to exclude
#' @param title_label string of the plot title.
#' @param ... further arguments passed to [brms::brm()] to sample from the
#'   prior
#' @returns [ggplot2::ggplot()] object.
#'
#' @examples
#'\dontrun{
#'   plot_prior_posterior_densities(
#'     model = my_sigmoid_model,
#'     title_label = "Prior Posterior Density Plots")
#'}
#'
#'
#' @importFrom rlang .data
#' @export
plot_prior_posterior_densities <- function(
  model,
  title_label = "Prior Posterior Density",
  exclude_variables = c("__$", "lprior"),
  ...) {

  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "plot_prior_posterior_densities expects model to be of class 'bpfit',",
      " instead it is of class '", class(model), "'"))
  }

  model_prior <- model |>
    stats::update(
      sample_prior = "only",
      iter = 2000,
      ...)

  draws <- dplyr::bind_rows(
    model_prior |>
      tidybayes::tidy_draws() |>
      tidybayes::gather_variables() |>
      dplyr::ungroup() |>
      dplyr::mutate(sample_type = "Prior") |>
      dplyr::filter(
        .data[[".variable"]] |>
          stringr::str_detect(
            paste0(exclude_variables, collapse = "|"),
            negate = TRUE)),
    model |>
      tidybayes::tidy_draws() |>
      tidybayes::gather_variables() |>
      dplyr::ungroup() |>
      dplyr::mutate(sample_type = "Posterior") |>
      dplyr::filter(
        .data[[".variable"]] |>
          stringr::str_detect(
            paste0(exclude_variables, collapse = "|"),
            negate = TRUE))) |>
    dplyr::mutate(
      sample_type = .data[["sample_type"]] |> forcats::fct_inorder())

  
  
  ggplot2::ggplot(data = draws) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        x = .data[[".value"]],
        group = .data[["sample_type"]],
        fill = .data[["sample_type"]]),
      color = "black",
      alpha = .7) +
    ggplot2::ggtitle(
      label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(.data[[".variable"]]),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_manual(
      "Sample Type",
      values = c("Posterior" = "hotpink2", "Prior" = "cyan2"))
}
