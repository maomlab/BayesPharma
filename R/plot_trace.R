
#' Create a plot of MCMC chains and sample value at each MCMC iteration
#'
#'@param model brmsfit
#'@param predictors_col_name string. Name of the column with the perturbations
#'that each parameter is being estimated for.
#'@param half_max_label string. Label for the half maximal that fits the type of
#'experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).
#'@param title string for the plot title (default = NULL)
#'@return ggplot object
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
