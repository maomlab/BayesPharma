

#' stanvar function
dr_stanvar <- brms::stanvar(
  scode = paste(
    "   real sigmoid(",
    "      real ec50,",
    "      real hill,",
    "      real top,",
    "      real bottom,",
    "      real log_dose) {",
    "        if( log_dose > negative_infinity() ){",
    "          return (bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill)));",
    "        } else { ",
    "           if( hill > 0) {",
    "            return bottom;",
    "            } else {",
    "               return top;",
    "            }",
    "        }",
    "   }", sep = "\n"),
  block = "functions")

#'Create a formula for a one or multiple different perturbations/experimental
#'variables for the brms model
#'
#'@param multiple_perturbations TRUE/FALSE. If FALSE, the model will produce a
#'singular estimate for each parameter. If TRUE, the parameter will produce
#'parameter estimates for each perturbation.
#'@param predictors Additional formula objects to specify predictors of non-linear
#'parameters. i.e. what perturbations/experimental differences should be modeled
#'separately? (Default: 0 + predictors) should a random effect be taken into consideration?
#'i.e. cell number, plate number, etc.
#'@return brmsformula
#'
#'@export

dr_formula <- function(multiple_perturbations = FALSE,
                       predictors = 0 + predictors,
                       ...){

  if (multiple_perturbations == FALSE) {
    predictor_eq <- rlang::new_formula(lhs = quote(ec50 + hill + top + bottom),
                                       rhs = quote(1))
  } else{
    predictor_eq <- rlang::new_formula(lhs = quote(ec50 + hill + top + bottom),
                                       rhs = rlang::enexpr(predictors))
  }

  sigmoid_formula <- brms::brmsformula(
    response ~ sigmoid(ec50, hill, top, bottom, log_dose),
    predictor_eq, nl = TRUE, ...)

  return(sigmoid_formula)
}

#' Formula for the constant curve
#'
#'@param multiple_perturbations TRUE/FALSE. If FALSE, the model will produce a
#'singular estimate for each parameter. If TRUE, the parameter will produce
#'parameter estimates for each perturbation.
#'@param predictors Additional formula objects to specify predictors of non-linear
#'parameters. i.e. what perturbations/experimental differences should be modeled
#'separately? (Default: 0 + predictors) should a random effect be taken into consideration?
#'i.e. cell number, plate number, etc.
#'@return brmsformula
#'
#'@export
constant_formula <- function(multiple_perturbations = FALSE,
                       predictors = 0 + predictors,
                       ...){

  if (multiple_perturbations == FALSE) {
    constant_eq <- rlang::new_formula(lhs = quote(response),
                                      rhs = quote(1))
  } else{
    constant_eq <- rlang::new_formula(lhs = quote(response),
                                      rhs = rlang::enexpr(predictors))
  }


  formula <- brms::brmsformula(constant_eq, ...)

  return(formula)
}



#'Run Bayesian Regression Model using Stan
#'
#'For additional information on additional function arguments, reference:
#'https://paul-buerkner.github.io/brms/reference/brm.html
#'or
#'https://rdrr.io/cran/rstan/man/stan.html
#'
#'@param data tibble or data.frame of experimental data.
#'@param multiple_perturbations TRUE/FALSE. If FALSE, the model will produce a
#'singular estimate for each parameter. If TRUE, the parameter will produce
#'parameter estimates for each perturbation.
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
dr_model <- function(data,
                     formula = dr_formula(),
                     priors = NULL,
                     inits = 0,
                     iter = 8000,
                     control = list(adapt_delta = 0.99),
                     ...) {

  if (is.null(priors)) {
    warning("priors for ec50, hill, top and bottom are required. Use make_priors function to get default priors.")
  }
  brms::brm(
    formula = formula,
    data = data,
    prior = priors,
    inits = inits,
    iter = iter,
    control = control,
    stanvars = dr_stanvar,
    ...)
}
