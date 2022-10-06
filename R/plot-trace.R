#' Create a trace plot of MCMC draws
#'
#' @description Given a brms model, plot a trace plot of MCMC draws.
#'
#' @param model brmsfit model.
#' @param predictors_col_name string expression for predictors column in the
#'   input data.frame (default = "_Intercept"). Predictors are the perturbations
#'   tested during the experiment (i.e. Drug, Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that fits the
#'   type of experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.)
#'   (default = "ec50").
#' @param title string of the plot title (default = NULL)
#' @return ggplot2::ggplot object.
#'
#' @examples
#'\dontrun{
#' Consider a dose response experiment testing multiple activators.
#' traceplot(
#'   model = my_dr_model,
#'   predictors_col_name = "activators",
#'   half_max_label = "ec50",
#'   title = "Traceplot of Dose Response Activators Experiment")
#'}
#'@export
traceplot <- function(
    model,
    predictors_col_name = "_Intercept",
    half_max_label = "ec50",
    title = NULL) {

  if (is.character(predictors_col_name) == FALSE) {
    warning("predictors_col_name must be a character. If there are not a
            predictors in the data and model, then run using the default
            argument (predictors_col_name = '_Intercept').")
  }
  if (is.character(half_max_label) == FALSE) {
    warning("half_max_label must be a character. Include a label that fits the
            experiment type (i.e. ic50, ed50, id50, ld50, etc.)
            (default = 'ec50').")
  }

  model_parnames <- brms::variables(model) %>%
    stringr::str_remove("b_") %>%
    stringr::str_remove(predictors_col_name) %>%
    stringr::str_replace("ec50", half_max_label)

  model_parnames <- head(model_parnames, -3)

  model_array <- as.array(model)[, , seq(model_parnames)]
  dimnames(model_array)[[3]] <- model_parnames

  bayesplot::mcmc_trace(model_array) +
    ggplot2::ggtitle(
      label = paste0("Trace Plot:", title))
}
