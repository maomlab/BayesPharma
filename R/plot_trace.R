
#' Create a plot of MCMC chains and sample value at each MCMC iteration
#'
#'@param model brmsfit
#'@param title string for the plot title (default = NULL)
#'@return ggplot object
#'
#'@export

traceplot <- function(model,
                      title = NULL) {

  model_parnames <- brms::variables(model) %>%
    stringr::str_remove("b_") %>%
    stringr::str_remove("predictors")
  model_parnames <- head(model_parnames, -2)

  model_array <- as.array(model)[, , 1:length(model_parnames)]
  dimnames(model_array)[[3]] <- model_parnames

  bayesplot::mcmc_trace(model_array) +
    ggplot2::ggtitle(
      label = paste0("Trace Plot:", title)
    )
}
