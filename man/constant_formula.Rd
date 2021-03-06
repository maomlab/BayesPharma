% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_dr_brm_model.R
\name{constant_formula}
\alias{constant_formula}
\title{Formula for a curve with a constant response}
\usage{
constant_formula(predictors = 1, ...)
}
\arguments{
\item{predictors}{Additional formula objects to specify predictors of
non-linear parameters. i.e. what perturbations/experimental differences
should be modeled separately? (Default: 1) should a random effect be taken
into consideration? i.e. cell number, plate number, etc.}
}
\value{
brmsformula
}
\description{
set-up an equation for formula objects with a constant response.
}
\examples{
\dontrun{
  constant_formula(predictors = 0 + predictors)
}
}
