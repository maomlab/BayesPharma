#' Create a sigmoid formula for the brms model
#'
#' @description set-up a sigmoid dose response model formula to define a
#'   non-linear model or multilevel non-linear model for ec50, hill, top, and
#'   bottom for use in bayesian_model and in the brms package.
#'
#' @param predictors Additional formula objects to specify predictors of
#'   non-linear parameters. i.e. what perturbations/experimental differences
#'   should be modeled separately? (Default: 1) should a random effect be taken
#'   into consideration? i.e. cell number, plate number, etc.
#' @return brmsformula
#'
#' @examples
#'\dontrun{
#'   Consider observations made using 4 different drugs and the column header
#'   containing the labels for the 4 different drugs is `predictors`.
#'   dr_formula(predictors = 0 + predictors)
#'
#'   Consider that the cell_ID was recorded and the noise from the cell_ID is to
#'   be accounted for. dr_formula(predictors = 0 + predictors + (1|cell_ID))
#'}
#'
#'@export
dr_formula <- function(
  predictors = 1,
  ...) {


  predictor_eq <- rlang::new_formula(
    lhs = quote(ec50 + hill + top + bottom),
    rhs = rlang::enexpr(predictors))

  sigmoid_formula <- brms::brmsformula(
    response ~ sigmoid(ec50, hill, top, bottom, log_dose),
    predictor_eq,
    nl = TRUE,
    ...)

  return(sigmoid_formula)
}

#' Formula for a curve with a constant response
#'
#' @description set-up an equation for formula objects with a constant response.
#'
#' @param predictors Additional formula objects to specify predictors of
#'   non-linear parameters. i.e. what perturbations/experimental differences
#'   should be modeled separately? (Default: 1) should a random effect be taken
#'   into consideration? i.e. cell number, plate number, etc.
#' @return brmsformula
#'
#' @examples
#'\dontrun{
#'   constant_formula(predictors = 0 + predictors)
#'}
#' @export
constant_formula <- function(
  predictors = 1,
  ...) {

  constant_eq <- rlang::new_formula(
    lhs = quote(response),
    rhs = rlang::enexpr(predictors))

  formula <- brms::brmsformula(constant_eq, ...)

  return(formula)
}


#' Run Bayesian Regression Model using Stan
#'
#' @description
#'   For additional information on additional function arguments, reference:
#'   https://paul-buerkner.github.io/brms/reference/brm.html
#'   or
#'   https://rdrr.io/cran/rstan/man/stan.html
#'
#' @param data data.frame of experimental data.
#'   must contain columns \code{response} and any predictors specified in
#'   the formula.
#' @param formula brmsformula object. To create a dose-response brmsformula,
#'   use the \code{dr_formula} function.
#' @param priors brmspriors data.frame for ec50, hill, top, and bottom.
#'   Use one of the priors functions provided to create priors to use here.
#' @param init list of lists, numeric value, or "random" for the initial values
#'   of the parameters being modeled (default = 0).
#' @param iter number of iterations the model runs. Increasing iter can help
#'   with model convergence (default = 8000).
#' @param control a named list of parameters to control the sampler's behavior.
#'   Adding max_treedepth and giving a greater value than 10 can improve model
#'   convergence (default = list(adapt_delta = 0.99)).
#' @param stanvar_function stan code for the model.
#' @return brmsfit model
#'
#' @examples
#'\dontrun{
#'   dr_model(data,
#'    formula = dr_formula(predictors = 0 + predictors),
#'    priors = dr_priors(),
#'    inits = dr_inits(),
#'    iter = 8000,
#'    control = list(adapt_delta = 0.99),
#'    stanvar_function = dr_stanvar)
#'}
#' @export
dr_model <- function(
   data,
   formula,
   priors = NULL,
   init = 0,
   iter = 8000,
   control = list(adapt_delta = 0.99),
   stanvar_function = dr_stanvar,
   ...) {

  if (is.null(priors)) {
    warning("priors for each parameter is required. Use prior functions provided
             to get default priors.")
  }

  if (!("response" %in% names(data))) {
    warning(
      "There needs to be a column 'response' in the input 'data' data.frame\n")
  }

  brms::brm(
    formula = formula,
    data = data,
    prior = priors,
    init = init,
    iter = iter,
    control = control,
    stanvars = stanvar_function,
    ...)
}

#' @note stanvar script of a sigmoid dose response equation.
#' For log dose greater than negative infinity (dose greater than 0), the
#' sigmoid dose response equation is used. ec50 is the half maximal response,
#' hill is the slope, top is the maximum response, and bottom is the minimum
#' response. For log dose equal to negative infinity (dose equal to 0), if hill
#' is positive, then the bottom value is returned. If hill is negative, then the
#' top value is returned.
#'
dr_stanvar <- brms::stanvar(
  scode = paste(
    "   real sigmoid(",
    "      real ec50,",
    "      real hill,",
    "      real top,",
    "      real bottom,",
    "      real log_dose) {",
    "        if( log_dose > negative_infinity() ) {",
    "          return (bottom + (top - bottom) /",
    "            (1 + 10^((ec50 - log_dose)*hill)));",
    "        } else { ",
    "           if( hill > 0) {",
    "            return bottom;",
    "            } else {",
    "               return top;",
    "            }",
    "        }",
    "   }", sep = "\n"),
  block = "functions")
