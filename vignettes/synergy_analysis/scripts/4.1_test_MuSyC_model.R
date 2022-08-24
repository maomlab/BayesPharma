library(plyr)
library(tidyverse)
library(MPStats)
library(brms)


#########################
# check functional form #
#########################

devtools::load_all()
steps <- 2
test_generative <- expand.grid(
    logd1 = seq(-100, -4, length.out = steps),
    logd2 = seq(-100, -4, length.out = steps),
    logE0 = seq(-10, 0, length.out = steps),
    logE1 = seq(-10, 0, length.out = steps),
    logE2 = seq(-10, 0, length.out = steps),
    logE3 = seq(-10, 0, length.out = steps),
    logC1 = seq(-10, -4, length.out = steps),
    logC2 = seq(-10, -4, length.out = steps),
    h1 = seq(0, 5, length.out = steps),
    h2 = seq(0, 5, length.out = steps),
    logalpha = seq(-10, 3, length.out = steps)) %>%
    dplyr::rowwise() %>%
    dplyr::do({
        Ed_simple <- generate_MuSyC_effects(
            d1 = exp(.$logd1),
            d2 = exp(.$logd2),
            E0 = exp(.$logE0),
            s1 = .$h1 * (exp(.$logE0) + exp(.$logE1)) / (4 * exp(.$logC1)),
            C1 = exp(.$logC1),
            E1 = exp(.$logE1),
            s2 = .$h2 * (exp(.$logE0) + exp(.$logE2)) / (4 * exp(.$logC2)),
            C2 = exp(.$logC2),
            E2 = exp(.$logE2),
            alpha = exp(.$logalpha),
            E3 = exp(.$logE3))
        Ed_robust <- generate_MuSyC_effects_robust(
            logd1 = .$logd1,
            logd2 = .$logd2,
            logE0 = .$logE0,
            h1 = .$h1,
            logC1 = .$logC1,
            logE1 = .$logE1,
            h2 = .$h2,
            logC2 = .$logC2,
            logE2 = .$logE2,
            logE3 = .$logE3,
            logalpha = .$logalpha)
        data.frame(
            .,
            s1 = .$h1 * (exp(.$logE0) + exp(.$logE1)) / (4 * exp(.$logC1)),
            s2 = .$h2 * (exp(.$logE0) + exp(.$logE2)) / (4 * exp(.$logC2)),
            Ed_simple = Ed_simple,
            Ed_robust = Ed_robust)
        })
        
test_generative %>%
    dplyr::filter(abs(Ed_simple - Ed_robust)  > .0001)

test_generative %>%
    dplyr::filter((Ed_robust > 1) | (Ed_robust < 0)) 

Ed_robust <- generate_MuSyC_effects_robust(
    logd1 = .$logd1,
    logd2 = .$logd2,
    logE0 = .$logE0,
    h1 = .$h1,
    logC1 = .$logC1,
    logE1 = .$logE1,
    h2 = .$h2,
    logC2 = .$logC2,
    logE2 = .$logE2,
    logE3 = .$logE3,
    logalpha = .$logalpha)

###################
# Check gradients #
###################

# MuSyC function

model_code <- "
functions {
  real MuSyC(
    real logd1, real logd2,
    real logE0,
    real logC1, real logE1, real h1,
    real logC2, real logE2, real h2,
    real logE3, real logalpha) {
      vector[4] numerator_parts;
      vector[4] denominator_parts;
      numerator_parts[1] = h1*logC1 + h2*logC2 + logE0;
      numerator_parts[2] = h1*logd1 + h2*logC2 + logE1;
      numerator_parts[3] = h1*logC1 + h2*logd2 + logE2;
      numerator_parts[4] = h1*logd1 + h2*logd2 + logE3 + logalpha;
      denominator_parts[1] = h1*logC1 + h2*logC2;
      denominator_parts[2] = h1*logd1 + h2*logC2;
      denominator_parts[3] = h1*logC1 + h2*logd2;
      denominator_parts[4] = h1*logd1 + h2*logd2 + logalpha;
      return exp(log_sum_exp(numerator_parts) - log_sum_exp(denominator_parts));
  }
}
parameters {
  real logd1; real logd2;
  real logE0;
  real logC1; real logE1; real h1;
  real logC2; real logE2; real h2;
  real logE3; real logalpha;
}
model {
  target += MuSyC(
    logd1, logd2, logE0,
    logC1, logE1, h1,
    logC2, logE2, h2,
    logE3, logalpha);
}
"
model_fit <- rstan::stan(
    model_code = model_code,
    chains = 0,
    verbose = TRUE)

tfun <- function(
    logd1, logd2, logE0,
    logC1, logE1, h1,
    logC2, logE2, h2,
    logE3, logalpha) {
    rstan::log_prob(
        model_fit,
        c(
            logd1, logd2, logE0,
            logC1, logE1, h1,
            logC2, logE2, h2,
            logE3, logalpha),
        gradient = TRUE)
}

tfun(
    logd1 = -Inf, logd2 = -2,
    logE0 = -2,
    logC1 = .3, logE1 = -1.5, h1 = .3,
    logC2 = .3, logE2 = -1.5, h2 = .3,    
    logE3 = -4,
    logalpha = .5)

## handle zero doses
model_code <- "
functions {
  real MuSyC(
    real logd1, real logd2,
    real logE0,
    real logC1, real logE1, real h1,
    real logC2, real logE2, real h2,
    real logE3, real logalpha) {
      vector[4] numerator_parts;
      vector[4] denominator_parts;
      numerator_parts[1] = h1*logC1 + h2*logC2 + logE0;
      denominator_parts[1] = h1*logC1 + h2*logC2;
      if( logd1 > negative_infinity() ){
        numerator_parts[2] = h1*logd1 + h2*logC2 + logE1;
        denominator_parts[2] = h1*logd1 + h2*logC2;
      } else {
        numerator_parts[2] = negative_infinity();
        denominator_parts[2] = negative_infinity();
      }
      if( logd2 > negative_infinity() ){
        numerator_parts[3] = h1*logC1 + h2*logd2 + logE2;
        denominator_parts[3] = h1*logC1 + h2*logd2;
      } else {
        numerator_parts[3] = negative_infinity();
        denominator_parts[3] = negative_infinity();
      }
      if( (logd1 > negative_infinity()) && (logd2 > negative_infinity())){
        numerator_parts[4] = h1*logd1 + h2*logd2 + logE3 + logalpha;
        denominator_parts[4] = h1*logd1 + h2*logd2 + logalpha;
      } else {
        numerator_parts[4] = negative_infinity();
        denominator_parts[4] = negative_infinity();
      }
      return exp(log_sum_exp(numerator_parts) - log_sum_exp(denominator_parts));
  }
}
parameters {
  real logd1; real logd2;
  real logE0;
  real logC1; real logE1; real h1;
  real logC2; real logE2; real h2;
  real logE3; real logalpha;
}
model {
  target += MuSyC(
    logd1, logd2, logE0,
    logC1, logE1, h1,
    logC2, logE2, h2,
    logE3, logalpha);
}
"
model_fit <- rstan::stan(
    model_code = model_code,
    chains = 0,
    verbose = TRUE)

tfun <- function(
    logd1, logd2, logE0,
    logC1, logE1, h1,
    logC2, logE2, h2,
    logE3, logalpha) {
    rstan::log_prob(
        model_fit,
        c(
            logd1, logd2, logE0,
            logC1, logE1, h1,
            logC2, logE2, h2,
            logE3, logalpha),
        gradient = TRUE)
}

tfun(
    logd1 = -Inf, logd2 = -2,
    logE0 = -2,
    logC1 = .3, logE1 = -1.5, h1 = .3,
    logC2 = .3, logE2 = -1.5, h2 = .3,    
    logE3 = -4,
    logalpha = .5)




##########################################
# check fitting on simulated data values #
##########################################


devtools::load_all()
test_well_scores <- expand.grid(
    log10d1 = c(-Inf, seq(-7, -5, length.out = 20)),
    log10d2 = c(-Inf, seq(-7, -5, length.out = 20))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        compound = "Test",
        dose1 = 10^log10d1,
        dose2 = 10^log10d2,
        Ed = MPStats::generate_MuSyC_effects_robust(
            logd1 = log10d1 * log(10),
            logd2 = log10d2 * log(10),
            logE0 = log(.12),
            h1 = 1,
            logC1 = -6 * log(10),
            logE1 = log(.08),
            h2 = 1,
            logC2 = -6.5 * log(10),
            logE2 = log(.06),
            logE3 = log(.01),
            logalpha = 1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        count = 1000,
        n_positive = rbinom(n=21*21, count, prob=Ed))

devtools::load_all()
synergy_model_t3 <- test_well_scores %>%
    MPStats::fit_MuSyC_score_by_dose_robust(
        stan_model_args = list(verbose = TRUE),
        model_evaluation_criteria = NULL,
        open_progress = FALSE,
        silent = FALSE,
        future = FALSE)


