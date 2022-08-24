


library(plyr)
library(tidyverse)
library(MPStats)
library(brms)

#################
# plate effects #
#################


# cells per well
well_scores <- readr::read_tsv("intermediate_data/well_scores.tsv") %>%
    dplyr::mutate(
        dose1_nM = dose1,
        dose2_nM = dose2,
        dose1 = dose1_nM * 1e-9,
        dose2 = dose2_nM * 1e-9,
        d1_scale_factor = 1e-6,
        d2_scale_factor = 1e-6)

plot <- ggplot2::ggplot(data = well_scores) +
    ggplot2::theme_bw() +
    ggplot2::theme(
        legend.position = c(.15, .75)) +
    ggplot2::geom_density(
        mapping = ggplot2::aes(
            x = log10(count),
            fill = plate_id,
            color = plate_id),
        size = .8,
        alpha = .2) +
    ggplot2::scale_x_continuous(
        "Cell count per well",
        limits = log10(c(1000, 10800)),
        breaks = log10(c(1000, 2500, 5000, 10000)),
        labels = c("1k", "2.5k", "5k", "10k")) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_color_discrete("Plate ID") +
    ggplot2::scale_fill_discrete("Plate ID") +
    ggplot2::ggtitle(
        label = "Distribution of cells per well across plates")

ggplot2::ggsave(
    filename = "product/figures/batch_effects/count_by_plate_id_density_20201129.pdf",
    width = 5,
    height = 4)

# infected cells per well by plate
plot <- ggplot2::ggplot(data = well_scores) +
    ggplot2::theme_bw() +
    ggplot2::theme(
        legend.position = c(.15, .75)) +
    ggplot2::geom_density(
        mapping = ggplot2::aes(
            x = log10(n_positive + 1),
            fill = plate_id,
            color = plate_id),
        size = .8,
        alpha = .2) +
    ggplot2::scale_x_continuous(
        "Infected cells per well",
        limits = log10(c(1, 1050)),
        breaks = log10(c(1, 3, 10, 30, 100, 300, 1000)),
        labels = c("1", "3", "10", "30", "100", "300", "1000")) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_color_discrete("Plate ID") +
    ggplot2::scale_fill_discrete("Plate ID") +
    ggplot2::ggtitle(
        label = "Distribution of infected cells per well across plates")

ggplot2::ggsave(
    filename = "product/figures/batch_effects/n_positive_by_plate_id_density_20201129.pdf",
    width = 5,
    height = 4)


# % infected cells per well by plate
plot <- ggplot2::ggplot(data = well_scores) +
    ggplot2::theme_bw() +
    ggplot2::theme(
        legend.position = c(.85, .75)) +
    ggplot2::geom_density(
        mapping = ggplot2::aes(
            x = score,
            fill = plate_id,
            color = plate_id),
        size = .8,
        alpha = .2) +
    ggplot2::scale_x_continuous(
        "% infected cells per well",
        labels = scales::percent_format()) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_color_discrete("Plate ID") +
    ggplot2::scale_fill_discrete("Plate ID") +
    ggplot2::ggtitle(
        label = "Distribution of % infected cells per well across plates")

ggplot2::ggsave(
    filename = "product/figures/batch_effects/score_by_plate_id_density_20201129.pdf",
    width = 5,
    height = 4)


# infected cell count by total cell count per well by plate
plot <- ggplot2::ggplot(data = well_scores) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE)) +
    ggplot2::geom_point(
        mapping = ggplot2::aes(
            x = log10(count),
            y = log10(n_positive),
            fill = plate_id,
            color = plate_id),
        size = .8,
        alpha = 1) +
    ggplot2::scale_x_continuous(
        "Cell count per well",
        limits = log10(c(1000, 10800)),
        breaks = log10(c(1000, 2500, 5000, 10000)),
        labels = c("1k", "2.5k", "5k", "10k")) +
    ggplot2::scale_y_continuous(
        "Infected cells per well",
        limits = log10(c(1, 1050)),
        breaks = log10(c(1, 3, 10, 30, 100, 300, 1000)),
        labels = c("1", "3", "10", "30", "100", "300", "1000")) +
    ggplot2::scale_color_discrete("Plate ID") +
    ggplot2::scale_fill_discrete("Plate ID") +
    ggplot2::ggtitle(
        label = "Infected by total cells per well across plates")

ggplot2::ggsave(
    filename = "product/figures/batch_effects/n_poisitive_by_count_by_plate_id_scatter_20201129.pdf",
    width = 5,
    height = 4)




# Batch effects
# Having this in the formula
#
#    logE0 ~ 1 + (1|plate_id)
#
# gives
# $prior:
#                    prior class      coef    group resp dpar    nlpar       bound       source
#       normal(-0.6931, 3)     b                                 logE0   <upper=0>         user
#       normal(-0.6931, 3)     b Intercept                       logE0             (vectorized)
#   student_t(3, 0, 281.7)    sd                                 logE0                  default
#   student_t(3, 0, 281.7)    sd           plate_id              logE0             (vectorized)
#   student_t(3, 0, 281.7)    sd Intercept plate_id              logE0             (vectorized)
#
# $model:
#   data {
#       int<lower=1> N;  // total number of observations
#       int Y[N];  // response variable
#       int trials[N];  // number of trials
#       matrix[N, K_logE0] X_logE0;  // population-level design matrix
#       int<lower=1> N_1;  // number of grouping levels
#       int<lower=1> M_1;  // number of coefficients per level
#       int<lower=1> J_1[N];  // grouping indicator per observation
#       // group-level predictor values
#       vector[N] Z_1_logE0_1;
#   }
#   parameters {
#       vector<upper=0>[K_logE0] b_logE0;  // population-level effects
#       vector<lower=0>[M_1] sd_1;  // group-level standard deviations
#       vector[N_1] z_1[M_1];  // standardized group-level effects
#   }
#   transformed parameters {
#       vector[N_1] r_1_logE0_1;  // actual group-level effects
#       r_1_logE0_1 = (sd_1[1] * (z_1[1]));
#   }
#   model {
#       // likelihood including all constants
#       if (!prior_only) {
#         vector[N] nlp_logE0 = X_logE0 * b_logE0;
#         for (n in 1:N) {
#           // add more terms to the linear predictor
#           nlp_logE0[n] += r_1_logE0_1[J_1[n]] * Z_1_logE0_1[n];
#         }
#       for (n in 1:N) {
#         // compute non-linear predictor values
#         mu[n] = MuSyC(..., nlp_logE0[n], ...);
#       }
#       target += binomial_lpmf(Y | trials, mu);
#       
#       }
#       // priors including all constants
#       target += normal_lpdf(b_logE0 | -0.6931, 3) - 1 * normal_lcdf(0 | -0.6931, 3);
#       target += student_t_lpdf(sd_1 | 3, 0, 281.7) - 1 * student_t_lccdf(0 | 3, 0, 281.7);
#       target += std_normal_lpdf(z_1[1]);
#   }
#
# BRMS summary
#   Group-Level Effects: 
#   ~plate_id (Number of levels: 5) 
#                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#   sd(logE0_Intercept)     0.23      0.17     0.09     0.65 1.05       56     2722
#   
#   Population-Level Effects: 
#                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#   logE0_Intercept       -2.39      0.13    -2.63    -2.14 1.01     2039     3801
#   logC1_Intercept        0.79      0.01     0.76     0.82 1.47        8       28
#   logE1_Intercept       -7.57      0.15    -7.87    -7.29 1.02      237      154
#   h1_Intercept           3.62      0.08     3.46     3.76 1.37        9       27
#   logC2_Intercept        1.44      1.22    -0.74     2.21 1.53        7       26
#   logE2_Intercept       -5.35      2.12    -9.53    -2.39 1.53        7       26
#   h2_Intercept           5.90      6.04     2.29    19.40 1.53        7       26
#   logE3_Intercept       -9.17      1.11   -11.88    -7.79 1.38        9       44
#   logalpha_Intercept     1.54      0.79     0.13     2.19 1.53        7       28
#
# stan summary
#                                              mean se_mean     sd     2.5%      25%      50%      75%    97.5% n_eff   Rhat
#   b_logE0_Intercept                         -2.39    0.00   0.13    -2.63    -2.45    -2.38    -2.33    -2.14  1991   1.01
#   b_logC1_Intercept                          0.79    0.01   0.01     0.76     0.79     0.80     0.80     0.82     3   1.74
#   b_logE1_Intercept                         -7.57    0.01   0.15    -7.87    -7.67    -7.57    -7.47    -7.29   244   1.01
#   b_h1_Intercept                             3.62    0.04   0.08     3.46     3.57     3.63     3.68     3.76     4   1.43
#   b_logC2_Intercept                          1.44    0.86   1.22    -0.74     1.29     2.13     2.16     2.21     2  29.57
#   b_logE2_Intercept                         -5.35    1.25   2.12    -9.53    -6.75    -5.53    -3.15    -2.39     3   1.75
#   b_h2_Intercept                             5.90    4.20   6.04     2.29     2.43     2.53     4.67    19.40     2   5.15
#   b_logE3_Intercept                         -9.17    0.49   1.11   -11.88    -9.80    -8.99    -8.23    -7.79     5   1.26
#   b_logalpha_Intercept                       1.54    0.56   0.79     0.13     1.28     1.94     2.04     2.19     2   8.86
#   sd_plate_id__logE0_Intercept               0.23    0.02   0.17     0.09     0.14     0.19     0.27     0.65    88   1.03
#   r_plate_id__logE0[NFU005316,Intercept]    -0.09    0.01   0.13    -0.34    -0.15    -0.09    -0.03     0.16   294   1.02
#   r_plate_id__logE0[NFU005317,Intercept]    -0.08    0.07   0.15    -0.42    -0.16    -0.06     0.01     0.18     5   1.26
#   r_plate_id__logE0[NFU005318,Intercept]    -0.10    0.04   0.14    -0.35    -0.17    -0.11    -0.04     0.17    14   1.10
#   r_plate_id__logE0[NFU005319,Intercept]     0.06    0.00   0.13    -0.20     0.00     0.07     0.13     0.31   981   1.02
#   r_plate_id__logE0[NFU005320,Intercept]     0.21    0.02   0.13    -0.04     0.14     0.20     0.27     0.47    42   1.04
#   s1                                         0.04    0.00   0.01     0.03     0.04     0.04     0.04     0.05  1797   1.01
#   s2                                         0.36    0.43   0.62     0.01     0.01     0.01     0.20     1.75     2   5.19
#   lp__                                   -7187.57  232.28 328.52 -7760.71 -7199.60 -6998.95 -6996.35 -6992.94     2 111.97
#   E0                                         0.09    0.00   0.01     0.07     0.09     0.09     0.10     0.12  2149   1.01
#   E1                                         0.00    0.00   0.00     0.00     0.00     0.00     0.00     0.00   134   1.02
#   E2                                         0.03    0.03   0.04     0.00     0.00     0.00     0.05     0.09     2  11.57
#   E3                                         0.00    0.00   0.00     0.00     0.00     0.00     0.00     0.00     3   1.59
#   C1                                         0.00    0.00   0.00     0.00     0.00     0.00     0.00     0.00     3   1.74
#   C2                                         0.00    0.00   0.00     0.00     0.00     0.00     0.00     0.00     2  13.52
#   alpha                                      5.81    1.90   2.75     1.13     4.05     6.99     7.66     8.91     2   4.28
# 
