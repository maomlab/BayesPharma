
#' Create a plot of MCMC chains and sample value at each MCMC iteration
#'
#'@param model brmsfit
#'@param title string for the plot title (default = NULL)
#'@return ggplot2::ggplot object
#'
#'@export

traceplot <- function(model,
                      title = NULL) {
  bayesplot::mcmc_trace(model) +
    ggplot2::ggtitle(
      label = paste0("Trace Plot:", title)
    )
}
