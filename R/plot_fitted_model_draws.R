


n_sample_draws <- function(model,
                           n= 100) {
  model %>%
    brms::posterior_samples() %>%
    dplyr::sample_n(n) %>%
    dplyr::mutate(draw_id = dplyr::row_number())
  }



posterior_response_draws <- function(model,
                                   n = 100,
                                   lower = -12,
                                   upper = -3,
                                   ec50 = ec50,
                                   hill = hill,
                                   top = top,
                                   bottom = bottom,
                                   drug_name = Drug) {
  n_sample_draws(model, n) %>%
    pivot_longer(cols = starts_with("b_"),
                 names_to = "Parameters",
                 values_to = "Value") %>%
    dplyr::mutate(b_class = stringr::str_extract(Parameters, "b_[a-zA-Z0-9]+") %>%
                    stringr::str_remove("b_"),
                  Drug = stringr::str_extract(Parameters, "Drug.+") %>%
                    stringr::str_remove("Drug")) %>%
    dplyr::mutate() %>%
    dplyr::select(-Parameters) %>%
    tidyr::pivot_wider(id_cols = c("draw_id", "Drug"),
                       names_from = "b_class",
                       values_from = "Value") %>%
    dplyr::mutate(Drug = Drug, ec50 = ec50, hill = hill, top = top, bottom = bottom) %>%
    dplyr::rowwise() %>%
    dplyr::do({
      params <- .
      tibble::tibble(
        log_dose = c(
          seq(
            from = log10(1*10^(lower)),
            to = log10(1*10^(upper)),
            length.out = 100)),
        draw_id = params$draw_id,
        Drug = params$Drug,
        ec50 = params$ec50,
        hill = params$hill,
        top = params$top,
        bottom = params$bottom
      )
    }) %>%
    dplyr::mutate(Response = bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))) %>%
    dplyr::mutate(Drug = ifelse(is.na(Drug), replace_na(drug_name), Drug))
  }



posterior_mean <- function(model,
                           n = 100,
                           lower = -12,
                           upper = -3,
                           ec50 = ec50,
                           hill = hill,
                           top = top,
                           bottom = bottom,
                           drug_name = Drug) {
  model %>%
    brms::posterior_samples() %>%
    tidyr::gather(factor_key = TRUE) %>%
    dplyr::group_by(key) %>%
    dplyr::summarise(Mean = mean(value)) %>%
    dplyr::rename(variable = key) %>%
    dplyr::filter(!stringr::str_detect(variable, "__$")) %>%
    dplyr::filter(!stringr::str_detect(variable, "sigma")) %>%
    dplyr::mutate(b_class = stringr::str_extract(variable, "b_[a-zA-Z0-9]+")%>%
                    stringr::str_remove("b_"),
                  Drug = stringr::str_extract(variable, "Drug.+")%>%
                    stringr::str_remove("Drug")) %>%
    dplyr::select(-variable) %>%
    tidyr::pivot_wider(id_cols = c("Drug"),
                       names_from = "b_class",
                       values_from = "Mean") %>%
    dplyr::mutate(Drug = Drug, ec50 = ec50, hill = hill, top = top, bottom = bottom) %>%
    dplyr::rowwise() %>%
    dplyr::do({
      params <- .
      tibble::tibble(
        log_dose = c(
          seq(
            from = log10(1*10^(lower)),
            to = log10(1*10^(upper)),
            length.out = 100)),
        Drug = params$Drug,
        ec50 = params$ec50,
        hill = params$hill,
        top = params$top,
        bottom = params$bottom
      )
    }) %>%
    dplyr::mutate(Response = bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))) %>%
    dplyr::mutate(Drug = ifelse(is.na(Drug), replace_na(drug_name), Drug))
  }



plot_traj <- function(data,
                      measurement,
                      draws,
                      pred_response,
                      mean_draws,
                      title = NULL,
                      xlabel = NULL,
                      ylabel = NULL) {
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data,
                        aes(x = log_dose,
                            y = measurement),
                        size = 0.5,
                        color = "black") +
    ggplot2::geom_line(data = draws,
                       aes(x = log_dose,
                           y = pred_response,
                           group = draw_id),
                       size = 0.4,
                       alpha = 0.2,
                       color = "blueviolet") +
    ggplot2::geom_line(data = mean_draws,
                       aes(x = log_dose,
                           y = Response),
                       size = 0.5,
                       alpha = 1.0,
                       color = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title,
                  x = xlabel,
                  y = ylabel) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(Drug))
  }

