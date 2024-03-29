% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-tQ-init.R
\name{tQ_init}
\alias{tQ_init}
\title{Initialize Parameter Values for the tQ Enzyme Kinetic Model}
\usage{
tQ_init(
  kcat = function() runif(n = 1, min = 3, max = 5),
  kM = function() runif(n = 1, min = 3, max = 5),
  ...
)
}
\arguments{
\item{kcat}{\code{numeric} initial value for \code{kcat} parameter in the tQ model}

\item{kM}{\code{numeric} initial value for the \code{kM} parameter in the tQ model}

\item{...}{additional parameter initialization. Each named argument should be
a function that returns a \code{numeric} or \code{array} depending on the dimension
of the parameter, see \code{\link[=rstan_default_init]{rstan_default_init()}} to use the \pkg{rstan} default
init.}
}
\value{
input for \verb{[tQ_model](init = ...)} parameter.
}
\description{
Initialize Parameter Values for the tQ Enzyme Kinetic Model
}
\seealso{
\link{tQ_model}, \link{tQ_formula}, and \link{tQ_prior}
}
