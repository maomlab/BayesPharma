% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-constant-formula.R
\name{constant_formula}
\alias{constant_formula}
\title{Formula for a Curve with a Constant Response}
\usage{
constant_formula(
  response_variable = "response",
  response_units = NULL,
  predictors = 1,
  ...
)
}
\arguments{
\item{response_variable}{\code{character} variable representing the response to
treatment}

\item{response_units}{character the units of the response}

\item{predictors}{Additional formula objects to specify predictors of
non-linear parameters. i.e. what perturbations/experimental differences
should be modeled separately? (Default: 1) should a random effect be taken
into consideration? i.e. cell number, plate number, etc.}

\item{...}{additional arguments to \code{\link[brms:brmsformula]{brms::brmsformula()}}}
}
\value{
bpformula \code{\link[brms:brmsformula]{brms::brmsformula()}}
}
\description{
Set-up an equation for formula objects with a constant response.
}
\examples{
\dontrun{
  constant_formula(predictors = 0 + predictors)
}
}
