#' Add model fit loo criterion to model objects
#'
#' @description This is required to use loo (leave one out) compare to compare
#'   multiple model fits against each other.
#'
#' @param model brmsfit model.
#' @param ... extra arguments passed on to `brms::add_criterion`
#' @return brmsfit model with loo criterion.
#'
#' @examples
#' \dontrun{
#'   add_loo_criterion(model = test_model)
#'}
#' @export

add_loo_criterion <- function(model,
                              ...) {

  model <- model %>% brms::add_criterion(
    criterion = c("loo"),
    reloo = TRUE,
    ...)

  return(model)

}

#' Compare different model fits against each other using loo compare
#'
#' @description Use the add_loo_criterion function on the models that will be
#'   compared before using this function to compare the models using
#'   leave one out.
#'
#' @param model1 brmsfit model with loo criterion.
#' @param model2 brmsfit model with loo criterion.
#' @return compare.loo object
#'
#' @examples
#'\dontrun{
#'   compare_models(model1 = my_dr_model_1,
#'                  model2 = my_dr_model_2)
#'}
#'@export

compare_models <- function(model1,
                           model2,
                           ...) {

  brms::loo_compare(model1, model2, ...)

}
