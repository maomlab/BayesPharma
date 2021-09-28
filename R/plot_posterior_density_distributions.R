
#' Displays a tibble of mean, median, standard deviation,
#' and confidence intervals
#'
#' @param model brmsfit
#' @param l_ci decimal of the lower confidence interval (default = 0.025)
#' @param u_ci decimal of the upper confidence interval (default = 0.975)
#' @return tibble::tibble that is required for the 'posterior_densities'
#' function
#'
#' @export

basic_stats <- function(model,
                        l_ci = 0.025,
                        u_ci = 0.975) {

  ple_info <- brms::fixef(model, probs = c(l_ci,u_ci))

  model %>%
    posterior::summarise_draws("mean",
                               "sd",
                               "median") %>%
    dplyr::filter(!stringr::str_detect(variable, "__$")) %>%
    dplyr::filter(!stringr::str_detect(variable, "sigma")) %>%
    dplyr::select(-variable) %>%
    cbind(l_ci = c(ple_info[ ,3]),
          u_ci = c(ple_info[ ,4])) %>%
    tibble::rownames_to_column("variables") %>%
    dplyr::mutate(variables = stringr::str_extract(variables,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove("predictors"))
}


#'Create a plot of the posterior density distributions of modeled parameters
#'
#'The 'basic_stats' function is used to plot the mean, lower confidence
#'interval, and upper confidence interval
#'
#'@param model brmsfit
#'@param title_label string
#'@return ggplot2::ggplot object
#'
#'@export

posterior_densities <- function(model,
                                title_label = "Posterior Density Plots with Mean and 95% CI") {
  posterior <- dplyr::bind_rows(
    model %>%
      tidybayes::tidy_draws() %>%
      tidybayes::gather_variables() %>%
      dplyr::filter(!stringr::str_detect(.variable, "__$")) %>%
      dplyr::filter(!stringr::str_detect(.variable, "sigma"))
  ) %>%
    dplyr::rename(variables = .variable) %>%
    dplyr::mutate(variables = stringr::str_extract(variables,
                                                   "b_[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove("b_")) %>%
    dplyr::mutate(variables = stringr::str_extract(variables,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove("predictors"))

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
      ggplot2::aes(xintercept = mean),
      basic_stats(model),
      color = "red"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -Inf, xmax = l_ci, ymin = -Inf, ymax = Inf),
      basic_stats(model),
      color = "gray",
      alpha = 0.5) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = u_ci, xmax = Inf, ymin = -Inf, ymax = Inf),
      basic_stats(model),
      color = "gray",
      alpha = 0.5) +
    ggplot2::ggtitle(
      label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(variables),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_discrete("Distribution")
}
