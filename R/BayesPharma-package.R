#' BayesPharma: Tools for Bayesian Analysis of Non-Linear Pharmacology Models
#'
#' @docType package
#' @name BayesPharma-package
#' @aliases BayesPharma
#'
#' @description
#' The \pkg{BayesPharma} package builds on the \pkg{Stan} and \pkg{brms} to
#' provide support for Bayesian regression modeling for foundational
#' pharmacology models. For each type of model, the user provides
#' \itemize{
#'   \item{**formula**: }{Describing how the model parameters, treatment,
#'     and optional predictors lead to the measured response using functions
#'     provided by BayesPharma for each type of model.}
#'   \item{**observed data**: }{response, treatment, and optional
#'     predictors as a `data.frame`}
#'   \item{**prior**: }{Initial distributions over the model parameters}
#' }
#'
#' The models that BayesPharma support are
#' \itemize{
#'   \item{**[sigmoid_model]**: }{4-parameter Hill equation}
#'   \item{**[MuSyC_model]**: }{Bivariate synergy model with Bliss and
#'     Loewe interaction models as special cases}
#'   \item{**[tQ_model]**: }{Generalization of enzyme progress curve
#'     kinetics ordinary differential equation}
#'   \item{**[growth_sigmoid_model]**: }{Sigmoid model for growth
#'     kinetics}
#'   \item{**[growth_richards_model]**: }{Generalized Richards model
#'     for growth kinetics}
#' }
#'
#' The BayesPharma package also provides a range of case studies as templates
#' and examples for getting started at applying Bayesian modeling to
#' pharmacology data analysis.
#'
#' Building on \pkg{Stan} brings the performance and stability of No-U-Turn
#' Sampling (NUTs) Hamiltonian Monte Carlo and a whole ecosystem of tools for
#' model assessment, and visualization (see <https://mc-stan.org/>).
#'
#' Building on \pkg{brms} allows for compact formula based model specification
#' adding complexity to the model incrementally, including handling missing
#' data, measurement error, and other response distributions (see
#' <https://paul-buerkner.github.io/brms/>).
#'
NULL
