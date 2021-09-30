#'Create a formula for a singular perturbation/experimental variable for the
#'brms model
#'
#'additional arguments for brmsformula can be added.
#'Reference this website for more information on additional arguments:
#'https://paul-buerkner.github.io/brms/reference/brmsformula.html
#'
#'@return brmsformula
#'
#'@export

no_predictors_dr_formula <- function(...){

  predictor_formula = rlang::new_formula(lhs = quote(ec50 + hill + top + bottom),
                                         rhs = quote(1))

  sigmoid_formula = brms::brmsformula(
    response ~ (bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))),
    predictor_formula, nl = TRUE, ...)

  return(sigmoid_formula)

}

#'Create a formula for a multiple different perturbations/experimental variables
#'for the brms model
#'
#'@param predictors expression of the grouping variables for the parameters.
#'i.e. what perturbations/experimental differences should be modeled separately?
#'
#'

predictors_dr_formula <- function(predictors = 0 + predictors, ...){

  predictor_formula = rlang::new_formula(lhs = quote(ec50 + hill + top + bottom),
                                         rhs = rlang::enexpr(predictors))

  sigmoid_formula = brms::brmsformula(
    response ~ (bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))),
    predictor_formula, nl = TRUE, ...)

  return(sigmoid_formula)

}


#'Run Bayesian Regression Model using Stan
#'
#'For additional information on additional function arguments, reference:
#'https://paul-buerkner.github.io/brms/reference/brm.html
#'or
#'https://rdrr.io/cran/rstan/man/stan.html
#'
#'@param data tibble or data.frame of experimental data.
#'@param priors brmspriors data.frame for ec50, hill, top, and bottom.
#'Use 'dr_priors' to create priors to use here.
#'@param inits list of lists, numeric value, or "random" for the initial values
#'of the parameters being modeled. (default = 0)
#'@param iter number of iterations the model runs. Increasing iter can help with
#'model convergence. (default = 8000)
#'@param control a named list of parameters to control the sampler's behavior.
#'Adding max_treedepth and giving a greater value than 10 can improve model
#'convergence. (default = list(adapt_delta = 0.99))
#'
#'@return brmsfit
#'
#'@export

dose_response_model <- function(data,
                                response_col_name,
                                log_dose_col_name,
                                predictors_col_name = NULL,
                                sigmoid_formula = no_predictors_dr_formula(),
                                priors = NULL,
                                inits = 0,
                                iter = 8000,
                                control = list(adapt_delta = 0.99),
                                ...){

  if (is.null(priors)){
    stop("priors for ec50, hill, top and bottom are required. Use make_priors function to get default priors.")
  }

  # this will probably be removed later
  # will say the input tibble/data.frame needs to contain response and log_dose
  input_data <- data %>%
    dplyr::rename(response = response_col_name) %>%
    dplyr::rename(log_dose = log_dose_col_name) %>%
    dplyr::rename(predictors = predictors_col_name)

  brms::brm(
    formula = sigmoid_formula,
    data = input_data,
    prior = priors,
    inits = inits,
    iter = iter,
    control = control,
    ...)
  }
