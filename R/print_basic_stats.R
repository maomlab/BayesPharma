
#' Tibble of mean, median, standard deviation,
#' and confidence intervals
#'
#' @param model brmsfit
#' @param l_ci decimal of the lower confidence interval (default = 0.025)
#' @param u_ci decimal of the upper confidence interval (default = 0.975)
#' @return tibble::tibble
#'
#' @export

basic_stats <- function(model,
                        l_ci = 0.025,
                        u_ci = 0.975) {

  ple_info <- brms::fixef(model, probs = c(l_ci,u_ci))

  model %>%
    brms::posterior_samples() %>%
    tidyr::gather(factor_key = TRUE) %>%
    dplyr::group_by(key) %>%
    dplyr::summarise(Mean = mean(value),
                     SD= sd(value),
                     Median = median(value),
    ) %>%
    dplyr::rename(variable = key) %>%
    dplyr::filter(!stringr::str_detect(variable, "__$")) %>%
    dplyr::filter(!stringr::str_detect(variable, "sigma")) %>%
    cbind(l_ci = c(ple_info[ ,3]),
          u_ci = c(ple_info[ ,4]))
  }
