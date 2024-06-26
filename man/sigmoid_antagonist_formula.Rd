% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-sigmoid-formula.R
\name{sigmoid_antagonist_formula}
\alias{sigmoid_antagonist_formula}
\title{Create a Formula for the Sigmoid Antagonist Model}
\usage{
sigmoid_antagonist_formula(
  treatment_variable = "log_dose",
  treatment_units = "Log[Molar]",
  response_variable = "response",
  response_units = NULL,
  predictors = 1,
  ...
)
}
\arguments{
\item{treatment_variable}{\code{character} variable representing the treatment.}

\item{treatment_units}{\code{character} the units of the treatment. The default is
log base 10 of the molar concentration.}

\item{response_variable}{\code{character} variable representing the response to
treatment.}

\item{response_units}{\code{character} the units of the response.}

\item{predictors}{\code{character} or expression of predictors of non-linear
parameters. i.e. what perturbations/experimental differences should be
modeled separately?}

\item{...}{additional arguments to \code{\link[brms:brmsformula]{brms::brmsformula()}}}
}
\value{
a \code{bpformula}, which is a subclass of \link[brms:brmsformula]{brms::brmsformula} and can
be passed to \code{\link[=sigmoid_model]{sigmoid_model()}}.
}
\description{
Set-up an antagonist sigmoid dose response model formula to
define a non-linear model or multilevel non-linear model for \code{ic50},
\code{hill}, \code{top}, and, \code{bottom} for use in the \code{\link[=sigmoid_model]{sigmoid_model()}}.
}
\examples{
\dontrun{
  # Data has a string column drug_id with drug identifiers
  # Fit a separate model for each drug
  BayesPharma::sigmoid_antagonist_formula(predictors = 0 + drug_id)

  # Data has a string column plate_id with plate identifiers
  # Estimate the change in response for each plate relative to a global
  # baseline.
  BayesPharma::sigmoid_antagonist_formula(predictors = plate_id)

  # data has columns drug_id and plate_id
  # fit a multilevel model where the drug effect depends on the plate
  BayesPharma::sigmoid_antagonist_formula(
    predictors = 0 + (drug_id|plate_id))
}

}
\seealso{
\link[brms:brmsformula]{brms::brmsformula}, which this function wraps. The resulting
\code{bpformula} object can be passed into \code{\link[=sigmoid_model_antagonist]{sigmoid_model_antagonist()}}.
}
