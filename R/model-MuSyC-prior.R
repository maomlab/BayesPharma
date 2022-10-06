
#' Default priors for the MuSyC model
#'
#' @description Creating a data.frame of priors of the dose response parameters
#'    required for the MuSyC model.
#'
#' 
#' If you would like to set a parameter as a constant, enter a numeric value for
#' the function argument.
#' If you would like to set your own distribution, for example, in the function
#' arguments type 'ec50 = brms::prior(distribution(mu,sigma), nlpar = 'ec50')'.
#' For other distribution options, reference
#' http://mc-stan.org/rstanarm/reference/priors.html#arguments.
#'
#' @param logE0 numeric or `brms::prior(...)`. Prior for the log response with
#'   no treatment. Default normal(log(0.5), 3) lower bounded by 0.
#' @param logE1 numeric or `brms::prior(...)`. Prior for the full log response
#'   for treatment 1 and the dose of treatment 2 is zero. Default 
#'   normal(log(0.25), 3) lower bounded by 0.
#' @param logC1 numeric or `brms::prior(...)`. Prior for the log IC50 of drug 1
#'   Default to normal(log(1), 3). 
#' @param h1 numeric or `brms::prior(...)`. Prior the the slope of drug 1 at
#'   it's IC50 and dose of the treatment 2 is zero converted to exponent form
#'   Default mu = MuSYC_si_to_hi(si = 1, Ci = 1, E0 = 0, Ei = 0); normal(mu, 3)
#'   lower bounded by .1
#' @param logE2 numeric or `brms::prior(...)`. Prior for the full log response
#'   for treatment 2 and the dose of treatment 1 is zero. Default 
#'   normal(log(0.25), 3) lower bounded by 0.
#' @param logC2 numeric or `brms::prior(...)`. Prior for the log IC50 of drug 2
#'   Default to normal(log(1), 3). 
#' @param h2 numeric or `brms::prior(...)`. Prior the the slope of drug 2 at
#'   it's IC50 and dose of the treatment 1 is zero converted to exponent form
#'   Default mu = MuSYC_si_to_hi(si = 1, Ci = 1, E0 = 0, Ei = 0); normal(mu, 3)
#'   lower bounded by .1
#' @param logE3 numeric or `brms::prior(...)`. Prior for the low full response
#'   for treatment 1 and treatment 2. Default to normal(log(0.25), 3) lower
#'   bounded by 0.
#' @param logalpha numeric or `brms::prior(...)`. Prior for the log synergistic
#'   potency parameter with greater greater than 1 synergistic, less than one
#'   antagonistic. Default normal(log(0), 0.5).

#' @return brmsprior data.frame
#'
#' @export
MuSyC_prior <- function(
    logE0 = brms::prior(normal(-0.6931, 3), nlpar = "logE0", ub = 0),
    logE1 = brms::prior(normal(-1.386, 3), nlpar = "logE1", ub = 0),
    logC1 = brms::prior(normal(0, 3), nlpar = "logC1"),
    h1 = brms::prior(normal(4, 3), nlpar = "h1", lb = .1),
    logE2 = brms::prior(normal(-1.386, 3), nlpar = "logE2", ub = 0),
    logC2 = brms::prior(normal(0, 3), nlpar = "logC2"),
    h2 = brms::prior(normal(4, 3), nlpar = "h2", lb = .1),
    logE3 = brms::prior(normal(-1.386, 3), nlpar = "logE3", ub = 0),
    logalpha = brms::prior(normal(0, 0.5), nlpar = "logalpha"),
    ...) {  

  c(
    prepare_prior(logE0, nlpar = "logE0"),
    prepare_prior(logE1, nlpar = "logE1"),
    prepare_prior(logC1, nlpar = "logC1"),
    prepare_prior(h1, nlpar = "h1"),
    prepare_prior(logE2, nlpar = "logE2"),
    prepare_prior(logC2, nlpar = "logC2"),
    prepare_prior(h2, nlpar = "h2"),
    prepare_prior(logE3, nlpar = "logE3"),
    prepare_prior(logalpha, nlpar = "logalpha"))
}
