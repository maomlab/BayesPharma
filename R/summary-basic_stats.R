#' Displays a data.frame of basic statistical information about the model
#' results
#'
#' @description data.frame containing summary statistics of brmsfit model.
#'   The summary statistics included are mean, median, standard deviation,
#'   lower confidence interval, and upper confidence interval.
#'
#' @param model brmsfit model
#' @param predictors_col_name string expression for predictors column in the
#'   input data.frame (default = "_Intercept"). Predictors are the perturbations
#'   tested during the experiment (i.e. Drug, Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that fits the
#'   type of experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).
#' @param l_ci numeric unit of the lower confidence interval (default = 0.025)
#' @param u_ci numeric unit of the upper confidence interval (default = 0.975)
#' @return tibble::tibble object.
#'
#' @examples
#'\dontrun{
#'   basic_stats(
#'     model = my_dr_model,
#'     predictors_col_name = "predictors",
#'     half_max_label = "ic50",
#'     l_ci = 0.025,
#'     u_ci = 0.975)
#'}
#' @export

basic_stats <- function(
    model,
    predictors_col_name = "_Intercept",
    half_max_label = "ec50",
    l_ci = 0.025,
    u_ci = 0.975) {
  
  ple_info <- model %>% brms::fixef(probs = c(l_ci, u_ci))
  cat("lower CI:", l_ci, " upper CI:", u_ci, "\n", sep = "")
  
  model %>%
    posterior::summarise_draws(
      "mean", "sd", "median") %>%
    dplyr::filter(!stringr::str_detect(variable, "__$")) %>%
    dplyr::filter(!stringr::str_detect(variable, "sigma")) %>%
    dplyr::filter(!stringr::str_detect(variable, "lprior")) %>%
    dplyr::select(-variable) %>%
    cbind(l_ci = c(ple_info[, 3]),
          u_ci = c(ple_info[, 4])) %>%
    tibble::rownames_to_column("variables") %>%
    dplyr::mutate(
      variables = variables %>%
        stringr::str_extract("[a-zA-Z0-9]+.{1,100}") %>%
        stringr::str_remove(predictors_col_name) %>%
        stringr::str_extract("[a-zA-Z0-9]+.{1,100}") %>%
        stringr::str_replace("ec50", half_max_label))
  
}