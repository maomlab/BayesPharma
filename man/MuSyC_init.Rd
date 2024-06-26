% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-MuSyC-init.R
\name{MuSyC_init}
\alias{MuSyC_init}
\title{Create Initial Parameter Values for the MuSyC Model}
\usage{
MuSyC_init(
  logE0 = function() runif(n = 1, min = log(0.4), max = log(0.6)),
  logC1 = function() runif(n = 1, min = -0.2, max = 0.2),
  logE1 = function() runif(n = 1, min = log(0.4), max = log(0.6)),
  h1 = function() runif(n = 1, min = 3, max = 5),
  logC2 = function() runif(n = 1, min = -0.2, max = 0.2),
  logE2 = function() runif(n = 1, min = log(0.4), max = log(0.6)),
  h2 = function() runif(n = 1, min = 3, max = 5),
  logE3 = function() runif(n = 1, min = log(0.4), max = log(0.6)),
  logalpha = function() runif(n = 1, min = -0.2, max = 0.2),
  ...
)
}
\arguments{
\item{logE0}{\code{numeric} or function returning \code{array} of length \code{1}. Initial
value for the \code{logE0} parameter. Default: \code{log(0.5)}}

\item{logC1}{\code{numeric} or function returning array of length
1. Initial value for the \code{logC1} parameter. Default:
\code{0 = log(1)}}

\item{logE1}{\code{numeric} or function returning array of length
1. Initial value for the \code{logE1} parameter. Default:
log(0.5)}

\item{h1}{\code{numeric} or function returning array of length
1. Initial value for the \code{h1} parameter. Default: \code{4 = MuSyC_si_to_hi(si=1, Ci=1, E0=1, Ei=0.0)} the exponent for
treatment \code{1} corresponding to a slope of \code{1}}

\item{logC2}{\code{numeric} or function returning array of length
\code{1}. Initial value for the \code{logC2}
parameter. Default: \code{0 = log(1)}}

\item{logE2}{\code{numeric} or function returning array of length
\code{1}. Initial value for the \code{logE2} parameter. Default:
\code{log(0.5)}}

\item{h2}{\code{numeric} or function returning array of length
\code{1}. Initial value for the \code{h2} parameter. Default:
\code{4 = MuSyC_si_to_hi(si=1, Ci=1, E0=1, Ei=0.0)} the
exponent for treatment \code{1} corresponding to a slope of
\code{1}}

\item{logE3}{\code{numeric} or function returning array of length
1. Initial value for the \code{logE3} parameter. Default:
\code{log(0.5)}}

\item{logalpha}{\code{numeric} or function returning array of
length 1. Initial value for the \code{logalpha}
parameter. Default: \code{0 = log(1)}}

\item{...}{additional parameter initialization. Each named argument should be
a function that returns a \code{numeric} or \code{array} depending on the dimension
of the parameter, see \code{\link[=rstan_default_init]{rstan_default_init()}} to use the \pkg{rstan} default
init.}
}
\value{
input for \code{BayesPharma::model_MuSyC(init = ...)}
parameter.
}
\description{
Initial values for the MuSyC model
}
\examples{
\dontrun{
  # default prior and init for MuSyC Model
  prior <- BayesPharma::MuSyC_prior()
  init <- BayesPharma::MuSyC_init()

  # Set prior and init for MuSyC model where both treatment 1 and treatment 2
  # are assumed to be full inhibitors
  BayesPharma::MuSyC_prior(logE0 = 0, E1 = -Inf, E2 = -Inf, E3 = -Inf)
  BayesPharma::MuSyC_init(E0 = 1, E1 = -Inf, E2 = -Inf, E3 = -Inf)
}
}
