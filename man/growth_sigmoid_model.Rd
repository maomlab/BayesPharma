% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-growth-run.R
\name{growth_sigmoid_model}
\alias{growth_sigmoid_model}
\title{Run the Bayesian Sigmoid Growth Model}
\usage{
growth_sigmoid_model(
  data,
  formula = growth_richards_formula(),
  prior = growth_richards_prior(),
  init = growth_richards_init(),
  iter = 8000,
  control = list(adapt_delta = 0.99),
  stanvar_function = growth_sigmoid_stanvar(),
  expose_functions = TRUE,
  ...
)
}
\arguments{
\item{data}{\code{data.frame} of experimental data. Must contain column
representing the treatment, response and predictors specified in the
formula.}

\item{formula}{\code{bpformula} object. To create a growth model formula, use
\code{\link[=growth_sigmoid_formula]{growth_sigmoid_formula()}}}

\item{prior}{\code{brmspriors} \code{data.frame} for \code{K}, \code{K0}, \code{rate}, and \code{lambda}.
To create a prior, use \code{\link[=growth_sigmoid_prior]{growth_sigmoid_prior()}}.}

\item{init}{initial values of the parameters being modeled. To create an
init, use \code{\link[=growth_sigmoid_init]{growth_sigmoid_init()}}}

\item{iter}{\code{numeric} value for the number of iterations the model runs.
Increasing \code{iter} can help with model convergence}

\item{control}{a named \code{list} of parameters to control the sampler's
behavior. Adding \code{max_treedepth} and giving a greater value than
\code{10} can improve model convergence}

\item{stanvar_function}{Stan code for the model}

\item{expose_functions}{\code{logical}. Expose the stan functions for the
model. This is needed e.g. to run \code{\link[brms:loo_compare.brmsfit]{brms::loo_compare()}}}

\item{...}{additional arguments passed to \code{\link[brms:brm]{brms::brm()}}}
}
\value{
\code{bpfit} a wrapper for \code{\link[brms:brmsfit-class]{brms::brmsfit()}}
}
\description{
For additional information on additional function arguments, reference:
\code{\link[brms:brms-package]{brms::brms()}} and the \href{https://rdrr.io/cran/rstan/man/stan.html}{stan}
documentation.
}
\examples{
\dontrun{
  BayesPharma::growth_sigmoid_model(
    data = data,
    formula = BayesPharma::growth_richards_formula(predictors = 0 + drug))
}
}
\seealso{
Helper functions: \link{growth_sigmoid_formula}, and
\link{growth_sigmoid_prior}, \link{growth_sigmoid_init} and \link[brms:brmsfit-class]{brms::brmsfit}
}
