% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-posterior-draws.R
\name{plot_posterior_draws}
\alias{plot_posterior_draws}
\title{Plot of Posterior Model Fit Draws}
\usage{
plot_posterior_draws(
  model,
  newdata = NULL,
  treatment_variable = NULL,
  treatment_units = NULL,
  treatment_from = NULL,
  treatment_to = NULL,
  response_variable = NULL,
  response_units = NULL,
  title = "Dose-Response Posterior Draws",
  n = 50,
  point_size = 0.75,
  jitter_height = 0,
  jitter_width = 0
)
}
\arguments{
\item{model}{\code{bpfit} object resulting from fitting a \link{BayesPharma} model}

\item{newdata}{\code{data.frame} or \code{NULL} of new data to use for
predictions. Default data.frame with each predictor and treatment variable.}

\item{treatment_variable}{\code{character} or \code{NULL}. If \code{NULL} the treatment
variable name will be looked up in the model. The treatment variable the
\code{model$data} or \code{newdata} if supplied must have a column corresponding to
the treatment variable.}

\item{treatment_units}{\code{character} or \code{NULL}. If \code{NULL} the treatment units
will be looked up in the model. The treatment units will be used to label
the X-axis of the plot.}

\item{treatment_from}{\code{numeric} or \code{NULL}, for the lower bound on the
treatment range. If \code{NULL}, then use the (finite) lower bound of the
treatment variable in the model data.}

\item{treatment_to}{\code{numeric} or \code{NULL}, for the upper bound on the
treatment range. If \code{NULL}, then use the (finite) upper bound of the
treatment variable in the model data.}

\item{response_variable}{\code{string} or \code{NULL}. If \code{NULL} the response variable
name will be looked up in the model. The response variable the \code{model$data}
or \code{newdata} if supplied must have a column corresponding to the response
variable.}

\item{response_units}{\code{character} or \code{NULL}. If \code{NULL} the response units
will be looked up in the model. The response units will be used to label
the Y-axis of the plot.}

\item{title}{character name for the plot}

\item{n}{\code{numeric} value of the number of draws to show.}

\item{point_size}{\code{numeric}. \code{\link[ggplot2:geom_jitter]{ggplot2::geom_jitter()}} point size.}

\item{jitter_height}{\code{numeric}. the height distance between overlapping
points.}

\item{jitter_width}{\code{numeric}. the width distance between overlapping points.}
}
\value{
\link[ggplot2:ggplot]{ggplot2::ggplot} object.
}
\description{
A plot of a sample of model fit draws from the
posterior distribution from the expected mean and median
quantile intervals.
}
\examples{
\dontrun{
  # Consider a model named my_model and data named my_data with a column
  # named predictors containing multiple different perturbations.
  plot_posterior_draws(
    model = my_model,
    newdata = my_data,
    predictors_col_name = "predictors",
    lower = -12,
    upper = -3,
    n = 50,
    facet_var = predictors,
    jitter_width = 0.10,
    title = "Dose-Response Posterior Draws",
    xlab = "Log[Molar]",
    ylab = "Response")
}

}
