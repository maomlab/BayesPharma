#' Class `bpfit` of Models Fit with the [BayesPharma] Package
#'
#' Models fit with the [BayesPharma] package model function
#' represented as a `bpfit` object, which is a wrapper around the
#' [brms::brmsfit] that contains the posterior draws (samples), model formula,
#' Stan code, relevant data, and other information. Additionally, `bpfit`
#' objects contain a `bayes_pharma_info` list with the following elements:
#'
#'  * **model_type**: `character` for the type of model
#'  * elements from defined in the formula, including the treatment and response
#'    variables and their units
#'
#'  The BayesPharma information is used by some of the plots and model analysis
#'  methods.
#'
#'  Additionally, by default `bpfit` objects have their methods exposed by
#'  default so that it is possible e.g. compare models with
#'  [brms::loo_compare].
#'
#' @docType class
#' @name bpfit-class
#' @aliases bpfit
#'
#'
NULL
