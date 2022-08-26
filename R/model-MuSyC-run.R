#' Convert from slope parametrization to the exponent parametrization for drug i
#'
#' This can be used for setting priors and interpreting parameter estimates
#'
#' @param si slope of drug i at it's IC50 and doses of all other drugs are zero
#' @param Ci the IC50 of drug i
#' @param E0 the reponse with no treatments
#' @param Ei the reponse of inifinite drug i and no other treatments
#' @return hi the exponent in the MuSyC equation for drug i
#'
#'
#' Claim: When d1=0 and d2=C2 then d(Ed)/d(d2) = s2
#'        where s2 = h2 * (E0 + E2) / (4 * C2)
#'
#' d(Ed)/d(d2)
#'   =  d/d(d2)
#'      (C1^h1 * C2^h2 * E0 + C1^h1 * d2^h2 * E2) /
#'      (C1^h1 * C2^h2      + C1^h1 * d2^h2)
#'
#'Cancle the C1^h1 terms:
#'   =  d/d(d2)
#'      (C2^h2 * E0 + d2^h2 * E2) /
#'      (C2^h2      + d2^h2)
#'
#'
#' distribute the derivative across the terms in the numerator
#'   =  E0 * C2^h2 * [d/d(d2) 1     / (C2^h2 + d2^h2)] +
#'      E2         * [d/d(d2) d2^h2 / (C2^h2 + d2^h2)]
#'
#'   =  E0 * C2^h2 * [h2 * d2^(h2-1) / (C2^h2 + d2^h2)^2] +
#'      E2 * [C2^h2 * h2 * d2^(h2-1) / (C2^h2 + d2^h2)^2]
#'
#'   =  (E0 + E2) * C2^h2 * h2 * d2^(h2-1)/(C2^h2 + d2^h2)^2
#'
#' Evaluate at d2 = C2:
#'   =  (E0 + E2) * h2 * C2^(2*h2-1) / [4*C2^(2*h2))]
#'   =  h2 * (E0 + E2) / (4 * C2)
#'
#'
#'@export
MuSyC_si_to_hi <- function(si, Ci, E0, Ei){
  si * 4 * Ci / (E0 + Ei)
}

#' Convert from slope parametrization to the exponent parametrization for drug i#
#'
#' This can be used for setting priors and interpreting parameter estimates
#' see MuSyC_si_to_hi for details
#'
#' @param si slope of drug i at it's IC50 and doses of all other drugs are zero
#' @param Ci the IC50 of drug i
#' @param E0 the response with no treatments
#' @param Ei the response of infinite drug i and no other treatments
#' @return hi the exponent in the MuSyC equation for drug i
#'
#'@export
MuSyC_hi_to_si <- function(hi, Ci, E0, Ei){
  hi * (E0 + Ei) / (4 * Ci)
}


#' Drug Synergy
#' MuSyC Drug Synergy model
#'
#' Assume that the response metric decreases with more effective drugs
#' Let E3 be the effect at the maximum concentration of both drugs
#'
#'
#' Special cases:
#'    * dose additive model: alpha1 = alpha2 = 0
#'        * loewe: h1 = h2 = 1
#'            * CI:  E0 = 1, E1 = E2 = E3 = 0
#'                   the drug effect is equated with percent inhibition
#'    * bliss drug independence model:
#'        E0 = 1, E1 = E2 = E3 = 0, alpha1 = alpha2 = 1
#' @param d1 Dose of drug 1
#' @param d2 Dose of drug 2
#'
#' @param E0 effect with no drug treatment
#'
#' # params for drug 1 by it self
#' @param s1 drug 1 hill slope
#' @param C1 drug 1 EC50
#' @param E1 drug 1 maximum effect
#'
#' # params for drug 2 by it self
#' @param s2 drug 2 hill slope
#' @param C2 drug 2 EC50
#' @param E2 drug 2 maximum effect
#'
#' @param beta synergistic efficacy
#'    percent increase in a drug combination’s effect
#'    beyond the most efficacious single drug.
#'
#'    beta > 0 => synergistic efficacy
#'      the effect at the maximum concentration of both drugs (E3) exceeds the
#'      maximum effect of either drug alone (E1 or E2)
#'
#'    beta < 0 => antagonistic efficacy
#'      at least one or both drugs are more efficacious as
#'      single agents than in combination
#'
#' @param alpha1 synergistic potency
#'    how the effective dose of drug 1
#'    is altered by the presence of drug 2
#' @param alpha2 synergistic potency
#'    how the effective dose of drug 2
#'    is altered by the presence of drug 1
#'
#'    alpha > 1 => synergistic potency
#'      the EC50 decreases because of the addition of the other drug,
#'      corresponding to an increase in potency
#'
#'    0 <= alpha < 1 => antagonistic potency
#'      the EC50 of the drug increases as a result of the other drug,
#'      corresponding to a decrease in potency
#'
#'    alpha1 == alpha2 if detailed balance
#' @export
generate_MuSyC_effects <- function(
    d1,
    d2,
    E0,
    s1, C1, E1,
    s2, C2, E2,
    alpha,
    E3) {
  h1 <- MuSyC_si_to_hi(s1, C1, E0, E1)
  h2 <- MuSyC_si_to_hi(s2, C2, E0, E2)
  numerator <-
    C1^h1 * C2^h2 * E0 +
    d1^h1 * C2^h2 * E1 +
    C1^h1 * d2^h2 * E2 +
    d1^h1 * d2^h2 * E3 * alpha
  denominator <-
    C1^h1 * C2^h2 +
    d1^h1 * C2^h2 +
    C1^h1 * d2^h2 +
    d1^h1 * d2^h2 * alpha
  response <- numerator / denominator
}

#' Generate MuSyC Ed scores using a robust functional form
#' It should give the same results as the simple one, but
#' be more numerically stable
#' @export
generate_MuSyC_effects_robust <- function(
    logd1,
    logd2,
    logE0,
    h1, logC1, logE1,
    h2, logC2, logE2,
    logE3,
    logalpha) {
  numerator_parts <- c(
    h1*logC1 + h2*logC2 + logE0,
    h1*logd1 + h2*logC2 + logE1,
    h1*logC1 + h2*logd2 + logE2,
    h1*logd1 + h2*logd2 + logE3 + logalpha)
  numerator_max <- max(numerator_parts)
  log_numerator <- numerator_max + log(sum(exp(numerator_parts - numerator_max)))
  denominator_parts <- c(
    h1*logC1 + h2*logC2,
    h1*logd1 + h2*logC2,
    h1*logC1 + h2*logd2,
    h1*logd1 + h2*logd2 + logalpha)
  denominator_max <- max(denominator_parts)
  log_denominator <- denominator_max + log(sum(exp(denominator_parts - denominator_max)))
  exp(log_numerator - log_denominator)
}



#' Fit the MuSyC synergy model by dose
#'
#' @param well_scores data.frame
#'        with columns: dose1, dose2, n_positive, count, [<group_vars>]
#' @param group_vars quosures list
#'        dplyr::vars(...) columns to over when fitting synergy model
#' @param C1_prior prior distribution for Ed when d1=d1_IC50, d2=0
#' @param C2_prior prior distribution for Ed when d1=0, d2=d2_IC50
#' @param s1_prior prior distribution for d(Ed)/d(d1) when d1=d1_IC50, d2=0
#' @param s2_prior prior distribution for d(Ed)/d(d2) when d1=0, d2=d2_IC50
#' @param log10alpha_prior prior distribution for alpha synergy parameter
#' @param E0_prior prior distribution for Ed when d1=0, d2=0
#' @param E1_prior prior distribution for Ed when d1=Inf, d2=0
#' @param E2_prior prior distribution for Ed when d1=0, d2=Inf
#' @param E3_alpha_prior prior distribution for Ed scaled by alpha when d1=Inf, d2=Inf
#' @param C1_init initial sampling distribution for the C1 parameter
#' @param C2_init initial sampling distribution for the C2 parameter
#' @param s1_init initial sampling distribution for the s1 parameter
#' @param s2_init initial sampling distribution for the s2 parameter
#' @param log10alpha_init intial sampling distribution for the alpha parameter
#' @param E0_init initial sampling distribution for the E0 parameter
#' @param E1_init initial sampling distribution for the E1 parameter
#' @param E2_init initial sampling distribution for the E2 parameter
#' @param E3_alpha_init initial sampling distribution for the E3 parameter
#' @param combine combine the grouped models into a single brms model
#' @param verbose verbose output
#'
#' @param iter number of stan NUTS sampling steps
#' @param cores number of cores used for sampling
#' @param stan_model_args stan model arguments
#' @param control stan control arguments
#'
#' The
#'
#'    bernoulli_inf(n_positive / count) = Ed ~ MuSyC(d1, d2, C_params, E_params, s_params, alpha)
#'
#'    To improve numeric stability, the d1 and d2 and C1 and C2 variables
#'    are scaled to improve numeric stability:
#'
#'       d1 = dose1/max(dose1)
#'       d2 = dose2/max(dose2)
#'       drug1_IC50 = C1 * max(dose1)
#'       drug2_IC50 = C2 * max(dose2)
#'
#' Functional form:
#' Ed ~ (
#'        C1^h1 * C2^h2 * E0 +
#'        d1^h1 * C2^h2 * E1 +
#'        C1^h1 * d2^h2 * E2 +
#'        d1^h1 * d2^h2 * E3 * alpha
#'      ) / (
#'        C1^h1 * C2^h2 +
#'        d1^h1 * C2^h2 +
#'        C1^h1 * d2^h2 +
#'        d1^h1 * d2^h2 * alpha
#'      )
#'
#'
#'
#'
#'
#' ##############################################
#' # Proof of the definitions of the parameters #
#' ##############################################
#'
#' Claim: When d1=0 and d2=0 then Ed = E0
#' Ed = (C1^h1 * C2^h2 * E0) / (C1^h1 * C2^h2)
#'    = E0
#'
#' Claim: When d1=0 and d2 -> Inf then Ed = E2
#' Ed = (C2^h2 * E0 + d2^h2 * E2) / (C2^h2 + d2^h2)
#'    = (d2^h2 * E2) / (d2^h2)
#'    = E2
#'
#; Claim: When d1=0 and d2=C2 then Ed = (E0 + E2) / 2
#' When d1>0 and d2 -> Inf then Ed
#' Ed = (C1^h1 * C2^h2 * E0 + C1^h1 * C2^h2 * E2) / (C1^h1 * C2^h2 + C1^h1 * C2^h2)
#'    = (E0 + E2) / 2
#'
#'@export
fit_MuSyC_score_by_dose <- function(
    well_scores,
    group_vars = vars(compound),
    C1_prior = brms::prior(normal(0.5, 0.5), nlpar = "C1", lb = 0),
    C2_prior = brms::prior(normal(0.5, 0.5), nlpar = "C2", lb = 0),
    s1_prior = brms::prior(normal(1, 3), nlpar = "s1", lb = -.1),
    s2_prior = brms::prior(normal(1, 3), nlpar = "s2", lb = -.1),
    log10alpha_prior = brms::prior(normal(0, .5), nlpar = "log10alpha", lb = 0),
    E0_prior = brms::prior(beta(1, 1), nlpar = "E0", lb = 0, ub = 1),
    E1_prior = brms::prior(beta(1, 1), nlpar = "E1", lb = 0, ub = 1),
    E2_prior = brms::prior(beta(1, 1), nlpar = "E2", lb = 0, ub = 1),
    E3_alpha_prior = brms::prior(normal(0, 2), nlpar = "E3alpha", lb = 0),
    C1_init = function() {as.array(runif(1, 0, 2))},
    C2_init = function() {as.array(runif(1, 0, 2))},
    s1_init = function() {as.array(runif(1, -0.1, 2))},
    s2_init = function() {as.array(runif(1, -0.1, 2))},
    log10alpha_init = function() {as.array(0)},
    E0_init = function() {as.array(rbeta(1, 1, 1))},
    E1_init = function() {as.array(rbeta(1, 1, 1))},
    E2_init = function() {as.array(rbeta(1, 1, 1))},
    E3_alpha_init = function() {as.array(rnorm(1, 0, 2))},
    combine = FALSE,
    verbose = FALSE,
    iter = 8000,
    cores = 4,
    stan_model_args = list(verbose = FALSE),
    control = list(
      adapt_delta = .99,
      max_treedepth = 12),
    model_evaluation_criteria = c("loo", "bayes_R2"),
    ...) {
  
  if (is.data.frame(well_scores)) {
    grouped_data <- well_scores %>%
      dplyr::group_by(!!!group_vars) %>%
      dplyr::mutate(
        d1_scale_factor = max(dose1),
        d2_scale_factor = max(dose2)) %>%
      tidyr::nest() %>%
      dplyr::ungroup()
  }
  
  if (verbose) {
    cat("Fitting MuSyC model\n")
  }
  
  model <- brms::brm_multiple(
    formula = brms::brmsformula(
      n_positive | trials(count) ~ (
        C1^h1 * C2^h2 * E0 +
          d1^h1 * C2^h2 * E1 +
          C1^h1 * d2^h2 * E2 +
          d1^h1 * d2^h2 * E3alpha
      ) / (
        C1^h1 * C2^h2 +
          d1^h1 * C2^h2 +
          C1^h1 * d2^h2 +
          d1^h1 * d2^h2 * 10^log10alpha),
      brms::nlf(d1 ~ dose1 / d1_scale_factor),
      brms::nlf(d2 ~ dose2 / d2_scale_factor),
      brms::nlf(h1 ~ s1 * (4 * C1) / (E0 + E1)),
      brms::nlf(h2 ~ s2 * (4 * C2) / (E0 + E2)),
      E0 + C1 + E1 + s1 + C2 + E2 + s2 + log10alpha + E3alpha ~ 1,
      nl = TRUE),
    data = grouped_data$data,
    family = binomial("identity"),
    prior = c(
      C1_prior,
      C2_prior,
      s1_prior,
      s2_prior,
      log10alpha_prior,
      E0_prior,
      E1_prior,
      E2_prior,
      E3_alpha_prior),
    inits = function() {
      list(
        b_C1 = C1_init,
        b_C2 = C2_init,
        b_s1 = s1_init,
        b_s2 = s2_init,
        b_log10alpha = log10alpha_init,
        b_E0 = E0_init,
        b_E1 = E1_init,
        b_E2 = E2_init,
        b_E3alpha = E3_alpha_init)},
    #    stanvars = c(
    #        brms::stanvar(
    #            scode = "  real d1_scale_factor = max(dose1));",
    #            block = "tdata",
    #            position = "end"),
    #        brms::stanvar(
    #            scode = "  real d2_scale_factor = max(dose2));",
    #            block = "tdata",
    #            position = "end"),
    #        brms::stanvar(
    #            scode = "  real drug1_IC50 = b_C1 * d1_scale_factor);",
    #            block = "genquant",
    #            position = "end"),
    #        brms::stanvar(
    #            scode = "  real drug2_IC50 = b_C2 * d2_scale_factor;",
    #            block = "genquant",
    #            position = "end")),
    combine = FALSE,
    data2 = NULL,
    iter = iter,
    cores = cores,
    stan_model_args = stan_model_args,
    control = control,
    ...)
  
  if (!is.null(model_evaluation_criteria)) {
    # evalate fits
    model <- model %>%
      purrr::imap(function(model, i) {
        group_index <- grouped_data[i, ] %>% dplyr::select(-data)
        group_index_label <- paste0(
          names(group_index), ": ", group_index, collapse = ", ")
        cat("Evaluating model fit for ", group_index_label, "...\n", sep = "")
        model <- model %>% brms::add_criterion(
          criterion = model_evaluation_criteria,
          model_name = paste0("MuSyC:", group_index_label),
          reloo = TRUE)
        model
      })
  }
  grouped_data %>%
    dplyr::mutate(
      model = model)
}

#'Default priors for MuSyC synergy model
#'
#'@return named vector of class 'brms::brmsprior'
#'
#'@export
MuSyC_default_prior <- function() {
  c(
    brms::prior_string(
      prior = paste0("normal(", signif(log(.5), 4), ", 3)"),
      nlpar = "logE0",
      ub = 0),
    brms::prior_string(
      prior = paste0("normal(", signif(log(0.25), 4), ", 3)"),
      nlpar = "logE1",
      ub = 0),
    brms::prior_string(
      prior = paste0("normal(", signif(log(0.25), 4), ", 3)"),
      nlpar = "logE2",
      ub = 0),
    brms::prior_string(
      prior = paste0("normal(", signif(log(0.25), 4), ", 3)"),
      nlpar = "logE3",
      ub = 0),
    brms::prior_string(
      prior = paste0("normal(0, 3)"),
      nlpar = "logC1"),
    brms::prior_string(
      prior = paste0("normal(0, 3)"),
      nlpar = "logC2"),
    brms::prior_string(
      prior = paste0("normal(", signif(MuSyC_si_to_hi(si = 1, Ci = 1, E0 = 1, Ei = 0), 4), ", 5)"),
      nlpar = "h1",
      lb = .1),
    brms::prior_string(
      prior = paste0("normal(", signif(MuSyC_si_to_hi(si = 1, Ci = 1, E0 = 1, Ei = 0), 4), ", 5)"),
      nlpar = "h2",
      lb = .1),
    brms::prior_string(
      prior = paste0("normal(0, .5)"),
      nlpar = "logalpha"))
}

#'@export
MuSyC_default_inits <- function(
    n_plates = 1,
    n_sample1 = 1,
    n_sample2 = 1) {
  cat("Generating default inits for ", n_sample1, " x ", n_sample2, " samples\n", sep = "")
  inits_fn <- function() {
    list(
      b_logE0 = 0.5 %>% log() %>% rep(times = n_plates) %>% as.array(),
      b_logC1 = 0.0 %>% rep(times = n_sample1) %>% as.array(),
      b_logE1 = 0.25 %>% log() %>% rep(times = n_sample1) %>% as.array(),
      b_h1 = MuSyC_si_to_hi(si = 1, Ci = 1, E0 = 1, Ei = 0.0) %>%
        rep(times = n_sample1) %>%
        as.array(),
      b_logC2 = 0.0 %>% rep(times = n_sample2) %>% as.array(),
      b_logE2 = 0.25 %>% log() %>% rep(times = n_sample2) %>% as.array(),
      b_h2 = MuSyC_si_to_hi(si = 1, Ci = 1, E0 = 1, Ei = 0.0) %>%
        rep(times = n_sample2) %>%
        as.array(),
      b_logalpha = 0.0 %>% rep(times = n_sample1*n_sample2) %>% as.array(),
      b_logE3 = 0.25 %>% log() %>% rep(times = n_sample1*n_sample2) %>% as.array)
  }
}

MuSyC_function_stanvar <- brms::stanvar(
  scode = paste(
    "  real MuSyC(",
    "    real logd1, real logd2,",
    "    real logE0,",
    "    real logC1, real logE1, real h1,",
    "    real logC2, real logE2, real h2,",
    "    real logE3, real logalpha) {",
    "      vector[4] numerator_parts;",
    "      vector[4] denominator_parts;",
    "      numerator_parts[1] = h1*logC1 + h2*logC2 + logE0;",
    "      denominator_parts[1] = h1*logC1 + h2*logC2;",
    "      if( logd1 > negative_infinity() ) {",
    "        numerator_parts[2] = h1*logd1 + h2*logC2 + logE1;",
    "        denominator_parts[2] = h1*logd1 + h2*logC2;",
    "      } else {",
    "        numerator_parts[2] = negative_infinity();",
    "        denominator_parts[2] = negative_infinity();",
    "      }",
    "      if( logd2 > negative_infinity() ) {",
    "        numerator_parts[3] = h1*logC1 + h2*logd2 + logE2;",
    "        denominator_parts[3] = h1*logC1 + h2*logd2;",
    "      } else {",
    "        numerator_parts[3] = negative_infinity();",
    "        denominator_parts[3] = negative_infinity();",
    "      }",
    "      if( (logd1 > negative_infinity()) && (logd2 > negative_infinity())) {",
    "        numerator_parts[4] = h1*logd1 + h2*logd2 + logE3 + logalpha;",
    "        denominator_parts[4] = h1*logd1 + h2*logd2 + logalpha;",
    "      } else {",
    "        numerator_parts[4] = negative_infinity();",
    "        denominator_parts[4] = negative_infinity();",
    "      }",
    "      return exp(log_sum_exp(numerator_parts) - log_sum_exp(denominator_parts));",
    "  }", sep = "\n"),
  block = "functions")

MuSyC_genquant_stanvar <- brms::stanvar(
  scode = paste(
    "  real E0 = exp(b_logE0[1]);",
    "  real E1 = exp(b_logE1[1]);",
    "  real E2 = exp(b_logE2[1]);",
    "  real E3 = exp(b_logE3[1]);",
    "  real s1 = b_h1[1] * (E0 + E1) / (4 * exp(b_logC1[1]));",
    "  real s2 = b_h2[1] * (E0 + E2) / (4 * exp(b_logC2[1]));",
    "  real C1 = exp(b_logC1[1]) * C_logd1_2[1];",
    "  real C2 = exp(b_logC2[1]) * C_logd2_2[1];",
    "  real alpha = exp(b_logalpha[1]);",
    sep = "\n"),
  block = "genquant",
  position = "end")

#'
#'
#'
#' Combined:
#'      sample1 = 
#'
#' 
#'      nlp_logE0 = X_logE0 * b_logE0
#'                  _______   _______
#'                     = 1    = param
#'
#'      nlp_logE1 = X_logE1 * b_logE0
#'                  _______   ______
#'                  =sample1  
#'@export
fit_MuSyC_score_by_dose_robust <- function(
    well_scores,
    group_vars = NULL,
    prior = MuSyC_default_prior(),
    inits = MuSyC_default_inits,
    formula = "MuSyC_combined",
    verbose = FALSE,
    iter = 8000,
    cores = 4,
    stan_model_args = list(verbose = FALSE),
    control = list(
      adapt_delta = .99,
      max_treedepth = 12),
    debug = FALSE,
    ...) {
  
  if (verbose) {
    cat("Fitting MuSyC model\n")
  }
  
  
  if(class(formula) == "brmsformula"){
    # use the formula as provided
    inits_fn <- inits
  } else if (formula == "MuSyC_separate") {
    inits_fn <- inits(n_plates = 1, n_sample1 = 1, n_sample2 = 1)
    formula <- brms::brmsformula(
      n_positive | trials(count) ~ MuSyC(
        logd1, logd2,
        logE0,
        logC1, logE1, h1,
        logC2, logE2, h2,
        logE3, logalpha),
      brms::nlf(logd1 ~ log(dose1 / d1_scale_factor)),
      brms::nlf(logd2 ~ log(dose2 / d2_scale_factor)),
      logE0 + logC1 + logE1 + h1 + logC2 + logE2 + h2 + logE3 + logalpha  ~ 1,
      nl = TRUE)
    data <- well_scores %>%
      dplyr::group_by(!!!group_vars) %>%
      tidyr::nest() %>%
      dplyr::ungroup()
    brms_fn <- function(...) {
      brms::brms_multiple(combine = FALSE, ...)}
    family <- binomial("identity")
    
  } else if (formula == "MuSyC_combined") {
    data <- well_scores
    inits_fn <- inits(
      n_plates = well_scores %>% dplyr::distinct(plate_id) %>% nrow(),
      n_sample1 = well_scores %>% dplyr::distinct(sample1) %>% nrow(),
      n_sample2 = well_scores %>% dplyr::distinct(sample2) %>% nrow())
    formula <- brms::brmsformula(
      n_positive | trials(count) ~ MuSyC(
        logd1, logd2,
        logE0,
        logC1, logE1, h1,
        logC2, logE2, h2,
        logE3, logalpha),
      brms::nlf(logd1 ~ log(dose1 / d1_scale_factor)),
      brms::nlf(logd2 ~ log(dose2 / d2_scale_factor)),
      logE0 ~ 0 + plate_id,
      logC1 + logE1 + h1 ~ 0 + sample1,
      logC2 + logE2 + h2 ~ 0 + sample2,
      logE3 + logalpha ~ 0 + sample1:sample2,
      nl = TRUE)
    brms_fn <- brms::brm
    family <- binomial(link = "identity")
    
  } else if (formula == "MuSyC_combined_od") {
    data <- well_scores
    inits_fn <- inits(
      n_plates = well_scores %>% dplyr::distinct(plate_id) %>% nrow(),
      n_sample1 = well_scores %>% dplyr::distinct(sample1) %>% nrow(),
      n_sample2 = well_scores %>% dplyr::distinct(sample2) %>% nrow())
    formula <- brms::brmsformula(
      n_positive ~ MuSyC(
        logd1, logd2,
        logE0,
        logC1, logE1, h1,
        logC2, logE2, h2,
        logE3, logalpha) + log(count),
      brms::nlf(logd1 ~ log(dose1 / d1_scale_factor)),
      brms::nlf(logd2 ~ log(dose2 / d2_scale_factor)),
      logE0 ~ 0 + plate_id,
      logC1 + logE1 + h1 ~ 0 + sample1,
      logC2 + logE2 + h2 ~ 0 + sample2,
      logE3 + logalpha ~ 0 + sample1:sample2,
      nl = TRUE)
    brms_fn <- brms::brm
    family <- brms::negbinomial(
      link = "log",
      link_shape = "log")
  }    
  
  if (debug) {
    model_code <- brms::make_stancode(
      formula = formula,
      data = data,
      family = family,
      prior = prior,
      inits = inits,
      stanvars = c(
        MuSyC_function_stanvar,
        MuSyC_genquant_stanvar))
    cat(model_code)
    browser()
  }
  
  
  model <- brms_fn(
    formula = formula,
    data = data,
    family = family,
    prior = prior,
    stanvars = c(
      MuSyC_function_stanvar,
      MuSyC_genquant_stanvar),
    inits = inits_fn,
    iter = iter,
    cores = cores,
    stan_model_args = stan_model_args,
    control = control,
    ...)
}

