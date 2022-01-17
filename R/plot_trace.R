
#' Create a trace plot of MCMC draws
#'
#' @description Given a brms model, plot a trace plot of MCMC draws.
#'
#'
#' @usage
#' model %>% traceplot(predictors_col_name = <predictors_col_name>,
#'                     half_max_label = <label_for_the_half_maximal_effect>,
#'                     title = <plot_title_label>)
#'
#' @param model brmsfit model.
#' @param predictors_col_name string expression for predictors column in the input data.frame (default = "predictors).
#'    Predictors are the perturbations tested during the experiment (i.e. Drug, Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that fits the type of
#' experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).
#' @param title string of the plot title (default = NULL)
#' @return ggplot2::ggplot object.
#'
#' @examples
#' Consider a dose response experiment testing multiple activators.
#' traceplot(model = my_dr_model,
#'           predictors_col_name = "activators",
#'           half_max_label = "ec50",
#'           title = "Traceplot of Dose Response Activators Experiment")
#'
#'@export

traceplot <- function(model,
                      predictors_col_name = "predictors",
                      half_max_label = NULL,
                      title = NULL) {

  model_parnames <- brms::variables(model) %>%
    stringr::str_remove("b_") %>%
    stringr::str_remove(predictors_col_name) %>%
    stringr::str_replace("ec50", half_max_label)

  model_parnames <- head(model_parnames, -2)

  model_array <- as.array(model)[, , seq(model_parnames)]
  dimnames(model_array)[[3]] <- model_parnames

  bayesplot::mcmc_trace(model_array) +
    ggplot2::ggtitle(
      label = paste0("Trace Plot:", title)
    )
}
