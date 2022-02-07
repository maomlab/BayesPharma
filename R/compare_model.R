#' Add model fit loo criterion to model objects
#'
#' @description This is required to use loo (leave one out) compare to compare
#'   multiple model fits against each other.
#'
#' @usage
#'   add_loo_criterion(model = <model>,
#'                     model_name = <model_name>)
#'
#' @param model brmsfit model.
#' @param model_name variable name given to the model.
#' @param ... extra arguments passed on to `brms::add_criterion`
#' @return brmsfit model with loo criterion.
#'
#' @examples
#'   add_loo_criterion(model = my_dr_model,
#'                     model = my_dr_model_1)
#'
#' @export

add_loo_criterion <- function(model,
                              model_name,
                              ...) {

  model <- model %>% brms::add_criterion(
    criterion = c("loo"),
    model_name = paste0(model_name),
    reloo = TRUE,
    ...)

  return(model)

}

#' Compare different model fits against each other using loo compare
#'
#' @decription Use the add_loo_criterion function on the models that will be compared before
#' using this function to compare the models using leave one out.
#'
#' @usage
#'   compare_models(model1 = <model1>,
#'                  model2 = <model2>)
#'
#' @param model1 brmsfit model with loo criterion.
#' @param model2 brmsfit model with loo criterion.
#' @return compare.loo object
#'
#' @examples
#'   compare_models(model1 = my_dr_model_1,
#'                  model2 = my_dr_model_2)
#'
#'@export

compare_models <- function(model1,
                           model2,
                           ...) {

  brms::loo_compare(model1, model2, ...)

}
