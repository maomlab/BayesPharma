


posterior_densities <- function(model,
                                title_label = "Posterior Density Plots with Mean and 95% CI") {
  posterior <- dplyr::bind_rows(
    model %>%
      tidybayes::tidy_draws() %>%
      tidybayes::gather_variables() %>%
      dplyr::filter(!stringr::str_detect(.variable, "__$")) %>%
      dplyr::filter(!stringr::str_detect(.variable, "sigma"))
  ) %>%
    dplyr::rename(variable = .variable)

  ggplot2::ggplot(data = posterior) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        x = .value),
      fill = "cyan2",
      color = "black",
      alpha = .9) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = Mean),
      basic_stats(model),
      color = "red"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -Inf, xmax = l_95, ymin = -Inf, ymax = Inf),
      basic_stats(model),
      color = "gray",
      alpha = 0.5) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = u_95, xmax = Inf, ymin = -Inf, ymax = Inf),
      basic_stats(model),
      color = "gray",
      alpha = 0.5) +
    ggplot2::ggtitle(
      label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(variable),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_discrete("Distribution")
}
