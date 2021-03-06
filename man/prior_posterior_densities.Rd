% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_density_distribution_plots.R
\name{prior_posterior_densities}
\alias{prior_posterior_densities}
\title{Create a plot of the prior & posterior density distributions of modeled
parameters from brmsfit model}
\usage{
prior_posterior_densities(
  model,
  predictors_col_name = "_Intercept",
  half_max_label = "ec50",
 
    title_label = "Prior Posterior Density\\n                                        Plots"
)
}
\arguments{
\item{model}{brmsfit model.}

\item{predictors_col_name}{string expression for predictors column in the
input data.frame (default = "_Intercept"). Predictors are the perturbations
tested during the experiment (i.e. Drug, Temperature, etc.).}

\item{half_max_label}{string of the label for the half maximal that fits the
type of experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).}

\item{title_label}{string of the plot title.
(default = "Prior Posterior Density Plots")}
}
\value{
ggplot2::ggplot object.
}
\description{
Plot of the prior and posterior density distributions of each
  parameter from brmsfit model. The prior and posterior density distributions
  will be displayed on the same plot and color labeled.
}
\examples{
\dontrun{
  prior_posterior_densities(model = my_dr_model,
                            predictors_col_name = "predictors",
                            half_max_response = "ic50",
                            title_label = "Prior Posterior Density Plots")
}
}
