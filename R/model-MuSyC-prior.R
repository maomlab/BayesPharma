#' Create a prior for the MuSyC synergy model
#'
#' @description Creating a `data.frame` of priors of the dose response
#' parameters required for the MuSyC model.
#'
#' If you would like to set a parameter as a constant, enter a
#' numeric value for the function argument.  If you would like to set your own
#' distribution, for example, in the function arguments type
#' `ec50 = brms::prior(prior = normal(<mu>, <sigma>), nlpar = 'ec50')`.

#' For other distribution options, reference [brms::prior] and [Prior Choice
#' Recommendations
#' ](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations)
#'
#' @param logE0 `numeric` or [brms::brmsprior()]. Prior for the
#'   log response with no treatment. (Default: `normal(log(0.5), 3)` lower
#'   bounded by `0`).
#' @param logE1 `numeric` or [brms::brmsprior()]. Prior for the
#'   full log response for treatment 1 and the treatment 2 is zero. (Default:
#'   `normal(log(0.25), 3)` lower bounded by `0`).
#' @param logC1 `numeric` or [brms::brmsprior()]. Prior for the
#'   log AC50 of treatment 1 and the dose of treatment 2 is zero. (Default:
#'   `normal(log(1), 3)`).
#' @param h1 `numeric` or [brms::brmsprior()]. Prior for the
#'   slope of treatment 1 at its AC50 and treatment 2 is zero in exponential
#'   form. To converted the slope to exponential form, use
#'   [MuSyC_si_to_hi()]). In particular, let `mu =
#'   MuSyC_si_to_hi(si = 1, Ci = 1, E0 = 0, Ei = 0)`, then (Default:
#'   `normal(mu, 3)` lower bounded by `.1`).
#' @param logE2 `numeric` or [brms::brmsprior()]. Prior for the
#'   full log response for treatment 2 and the treatment 1 is zero. (Default:
#'   `normal(log(0.25), 3)` lower bounded by `0`).
#' @param logC2 `numeric` or [brms::brmsprior()]. Prior for the
#'   log AC50 of treatment 2. (Default: to `normal(log(1), 3)`).
#' @param h2 `numeric` or [brms::brmsprior()]. Prior the slope of
#'   treatment 2 at its AC50 and treatment 1 is zero in exponential form. To
#'   converted the slope to exponential form, use [MuSyC_si_to_hi()]).
#'   In particular, let `mu = MuSyC_si_to_hi(si = 1, Ci = 1, E0 = 0, Ei =
#'   0)`, then (Default: `normal(mu, 3)` lower bounded by `.1`).
#' @param logE3 `numeric` or [brms::brmsprior()]. Prior for the
#'   combined full response for treatment 1 and treatment 2. (Default:
#'   `normal(log(0.25), 3)` lower bounded by 0).
#' @param logalpha `numeric` or [brms::brmsprior()]. Prior for
#'   the log synergistic potency. Where values greater than `1` are
#'   synergistic, values less than `1` are antagonistic. (Default:
#'   `normal(log(0), 0.5)`.
#' @param ... additional [brms::brmsprior()] objects to add to the
#'   prior.
#'
#' @returns [brms::brmsprior()] `data.frame`
#'
#' @export
MuSyC_prior <- function(
    logE0 = brms::prior(prior = normal(-0.6931, 3), nlpar = "logE0", ub = 0),
    logE1 = brms::prior(prior = normal(-1.386, 3), nlpar = "logE1", ub = 0),
    logC1 = brms::prior(prior = normal(0, 3), nlpar = "logC1"),
    h1 = brms::prior(prior = normal(4, 3), nlpar = "h1", lb = .1),
    logE2 = brms::prior(prior = normal(-1.386, 3), nlpar = "logE2", ub = 0),
    logC2 = brms::prior(prior = normal(0, 3), nlpar = "logC2"),
    h2 = brms::prior(prior = normal(4, 3), nlpar = "h2", lb = .1),
    logE3 = brms::prior(prior = normal(-1.386, 3), nlpar = "logE3", ub = 0),
    logalpha = brms::prior(prior = normal(0, 0.5), nlpar = "logalpha"),
    ...) {

  c(
    prepare_prior(prior = logE0, nlpar = "logE0"),
    prepare_prior(prior = logE1, nlpar = "logE1"),
    prepare_prior(prior = logC1, nlpar = "logC1"),
    prepare_prior(prior = h1, nlpar = "h1"),
    prepare_prior(prior = logE2, nlpar = "logE2"),
    prepare_prior(prior = logC2, nlpar = "logC2"),
    prepare_prior(prior = h2, nlpar = "h2"),
    prepare_prior(prior = logE3, nlpar = "logE3"),
    prepare_prior(prior = logalpha, nlpar = "logalpha"),
    ...)
}
