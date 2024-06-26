% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-sigmoid-run.R
\name{sigmoid_model}
\alias{sigmoid_model}
\title{Fit a Bayesian Sigmoid Model}
\usage{
sigmoid_model(
  data,
  formula = sigmoid_agonist_formula(),
  prior = sigmoid_agonist_prior(),
  init = sigmoid_agonist_init(),
  iter = 8000,
  control = list(adapt_delta = 0.99),
  stanvar_function = sigmoid_stanvar(),
  expose_functions = TRUE,
  ...
)
}
\arguments{
\item{data}{\code{data.frame} of observed data. It must contain columns
for the treatment, response and any additional predictors specified in the
formula. See \code{\link[=sigmoid_agonist_formula]{sigmoid_agonist_formula()}} for more details.}

\item{formula}{\code{bpformula} formula for the sigmoid model (see above).}

\item{prior}{\code{brmsprior} defining a distribution for the model
parameters defined in the formula. (see above).}

\item{init}{\code{function} to initialize \link[brms:brm]{brms::brm} model for for the parameters
(see above)}

\item{iter}{\code{numeric} of iterations the model runs. Increasing
\code{iter} can help with model convergence (see \link[brms:brm]{brms::brm})}

\item{control}{a named \code{list} of arguments to control the sampler's
behavior. Adding \code{max_treedepth} and giving a greater
value than \code{10} can improve model convergence (see \link[brms:brm]{brms::brm})}

\item{stanvar_function}{stan code for the model.}

\item{expose_functions}{\code{logical}. Expose the sigmoid function used in
the model. This is needed e.g. for \code{\link[brms:loo_compare.brmsfit]{brms::loo_compare()}}}

\item{...}{additional arguments passed to \link[brms:brm]{brms::brm}}
}
\value{
\code{bpfit} object, which is a wrapper around a \link[brms:brmsfit-class]{brms::brmsfit} object.
}
\description{
Fits the sigmoid model. The functional form is
\preformatted{<response> ~ sigmoid(ac50, hill, top, bottom, <treatment>)}
where
\preformatted{sigmoid = bottom + (top - bottom)/(1 + 10^((ac50 - <treatment>) * hill))}
By default the observed data (and therefore should be columns in the input
data \code{data.frame}) are
\itemize{
\item{\emph{\strong{treatment}}: \code{log_dose}, the \code{log10} of the dose as a molar
concentration}
\item{\emph{\strong{response}}: \code{response}, with unspecified units}
}
and the modeled parameters are
\itemize{
\item{\strong{ac50}: the dose where the response reaches half maximal
activity}
\item{\strong{hill}: the hill coefficient controlling the slope at the \code{ac50},
by convention the slope of an agonist is positive}
\item{\strong{top}: the response when \if{html}{\out{<treatment>}} => \code{Inf}}
\item{\strong{bottom}: the response when \if{html}{\out{<treatment>}} => \code{-Inf}}
}
To configure the model you can use the following helper functions for
agonist models
\itemize{
\item{\code{\link[=sigmoid_agonist_formula]{sigmoid_agonist_formula()}}: define how the \code{response} relates to
the \code{treatment} and the parameters with \code{ac50}->\code{ec50}, and how any
covariates predict the parameters}
\item{\code{\link[=sigmoid_agonist_prior]{sigmoid_agonist_prior()}}: define the prior for the model
parameters, with the slope constrained to be positive}
\item{\code{\link[=sigmoid_agonist_init]{sigmoid_agonist_init()}}: define initial values for the
model parameters to match the default \code{\link[=sigmoid_agonist_prior]{sigmoid_agonist_prior()}}}
}
the following helper functions for an antagonist model
\itemize{
\item{\code{\link[=sigmoid_antagonist_formula]{sigmoid_antagonist_formula()}}: define how the \code{response} relates to
the \code{treatment} and the parameters with \code{ac50}->\code{ic50}, and how any
covariates predict the parameters}
\item{\code{\link[=sigmoid_antagonist_prior]{sigmoid_antagonist_prior()}}: define the prior for the
model parameters, with the slope being constrained be negative}
\item{\code{\link[=sigmoid_antagonist_init]{sigmoid_antagonist_init()}}: define initial values for the
model parameters to match the default \code{\link[=sigmoid_antagonist_prior]{sigmoid_antagonist_prior()}}}
}
}
\examples{
\dontrun{
  BayesPharma::sigmoid_model(
    data = data,
    formula = BayesPharma::sigmoid_antagonist_formula(predictors = 0 + drug),
    prior = BayesPharma::sigmoid_antagonist_prior(),
    init = BayesPharma::sigmoid_antagonist_init())
}
}
