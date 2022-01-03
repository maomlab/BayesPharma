#' Add model fit loo criterion to model objects
#'
#'This is required to use loo compare to compare multiple model fits against
#'each other.
#'
#'@param model brmsfit object
#'@param n_params numeric value. The number of params being estimated by the
#'model. i.e. how many parameters have non-constant priors? in the case of dose
#'response cureves, are all 4 parameters being predicted by the model?
#'@param ... extra arguments passed on to `brms::add_criterion``
#'@return brmsfit object with loo criterion.
#'
#'@export

add_loo_criterion <- function(model,
                              n_params,
                              ...) {

  model <- model %>% brms::add_criterion(
    criterion = c("loo"),
    model_name = paste0(n_params, "param"),
    reloo = TRUE,
    ...)

  return(model)

}

#'Compare different model fits against each other using loo compare
#'
#'Use the add_loo_criterion function on the models that will be compared before
#'using this function to compare the models.
#'
#'@param model1 brmsfit object with loo criterion.
#'@param model2 brmsfit object with loo criterion.
#'@return compare.loo object
#'
#'@export

compare_models <- function(model1,
                           model2,
                           ...) {

  brms::loo_compare(model1, model2, ...)

}
