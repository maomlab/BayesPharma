library(deSolve)

#' Simulate data from the total QSSA model a refinement of the classical
#' Michaelis-Menten enzyme kinetics ordinary differential equation described in
#' (Choi, et al., 2017, DOI: 10.1038/s41598-017-17072-z). Consider the kinetic
#' rate equation
#'
#'                   kf
#'                  --->    kcat
#'           E + S  <---  C --->  E + P
#'                   kb
#'
#' where the free enzyme (E) reversibly binds to the stubstrate (S) to form
#' a complex (C) with forward and backward rate constants of kf and kb, which is
#' irreversibly catalyzed into the product (P), with rate constant of kcat,
#' releasing the enzyme to catalyze additional substrate. The total enzyme
#' concentration is defined to be the `ET := E + C`. The total substrate and
#' product concentration is defined to be `ST := S + C + P`. The Michaelis
#' constant is the defined to be the `kM := (kb + kcat) / kf`. The kcat rate
#' constant determines the maximum turn over at saturating substrate
#' concentrations, `Vmax := kcat * ET`. The rate constants `kcat` and `kM` can
#' be estimated by monitoring the product accumulation over time (enzyme
#' progress curves), by varying the enzyme and substrate concentrations.
#'
#' From (Choi, et al, 2017, equation 2, the total quasi-steady-state
#' approximation (tQ) differential equation is defined by
#'
#'   Observed data:
#'      M     = number of measurements        # number of measurements
#'      t[M]  = time                          # measured in seconds
#'      Pt[M] = product                       # product produced at time t
#'      ST    = substrate total concentration # specified for each experiment
#'      ET    = enzyme total concentration    # specified for each experiment
#'
#'   Model parameters:
#'     kcat    # catalytic constant (min^-1)
#'     kM      # Michaelis constant ()
#'
#'   ODE formulation:
#'     dPdt = kcat * (
#'              ET + kM + ST - Pt +
#'              -sqrt((ET + kM + ST - Pt)^2 - 2 * ET * (ST - Pt))) / 2
#'
#'   initial condition:
#'      P := 0
#'
#' In (Choi, et al. 2017) they prove, that the tQ model is valid when
#'
#'     K/(2*ST) * (ET+kM+ST) / sqrt((ET+kM+ST+P)^2 - 4*ET(ST-P)) << 1,
#'
#' where K = kb/kf is the dissociation constant.
#'
#'@param time numeric vector of increasing time points.
#'
#'
#' @export
simulate_tQ_model <- function(time, kcat, kM, ET, ST, ...) {
  ode_tQ <- function(time, Pt, theta) {
    list(c(theta[1] * (
      ET + theta[2] + ST - Pt +
        -sqrt((ET + theta[2] + ST - Pt)^2 - 4 * ET * (ST - Pt))) / 2))
  }
  deSolve::ode(
    y = c(P = 0),
    times = time,
    func = ode_tQ,
    parms = c(kcat, kM),
    ...)
}



#' stanvars for the tQ enzyme kinetics model
#'
#' @usage Pass to calls to `brms::brm`
#'
#' @export
tQ_stanvars <- brms::stanvar(
  scode = paste("
vector tQ_ode(
   real time,
   vector state,
   vector params,
   data real ET,
   data real ST) {

   real Pt = state[1];   // product at time t
   real kcat = params[1];
   real kM = params[2];
   vector[1] dPdt;
   dPdt[1] = kcat * (
     ET + kM + ST - Pt
     -sqrt((ET + kM + ST - Pt)^2 - 4 * ET * (ST - Pt))) / 2;
   return(dPdt);
}

vector tQ_single(
  data vector time,
  vector vkcat,
  vector vkM,
  data vector vET,
  data vector vST) {

  vector[2] params;
  params[1] = vkcat[1];
  params[2] = vkM[1];
  vector[1] initial_state;
  initial_state[1] = 0.0;
  real initial_time = 0.0;
  int M = size(time);

  vector[1] P_ode[M] = ode_bdf(     // Function signature:
    tQ_ode,                         // function ode
    initial_state,                  // vector initial_state
    initial_time,                   // real initial_time
    to_array_1d(time),              // array[] real time
    params,                         // vector params
    vET[1],                         // ...
    vST[1]);                        // ...

  vector[M] P;                      // Need to return a vector not array

  //for( j in 1:3) {
  //  print(
  //  \"tQ_single: \",
  //  \"time:\", time[j], \" \",
  //  \"kcat:\", vkcat[1], \" \",
  //  \"kM:\", vkM[1], \" \",
  //  \"ET:\", vET[1], \" \",
  //  \"ST:\", vST[1], \" \",
  //  \"product:\", P_ode[j,1]);
  //}

  for(i in 1:M) P[i] = P_ode[i,1];
  return(P);
}

vector tQ_multiple(
  array[] int series_index,
  data vector time,
  vector vkcat,
  vector vkM,
  data vector vET,
  data vector vST) {

  int N = size(time);
  vector[N] P;
  int begin = 1;
  int current_series = series_index[1];
  for (i in 1:N) {
    if(current_series != series_index[i]) {
      P[begin:i-1] = tQ_single(
        time[begin:i-1],
        vkcat[begin:i-1],
        vkM[begin:i-1],
        vET[begin:i-1],
        vST[begin:i-1]);

      // for(j in 1:3) {
      //  print(
      //    \"tQ_multiple: \",
      //    \"time:\", time[begin+j], \" \",
      //    \"kcat:\", vkcat[1], \" \",
      //    \"kM:\", vkM[1], \" \",
      //    \"ET:\", vET[begin+j], \" \",
      //    \"ST:\", vST[begin+j], \" \",
      //    \"product:\", P[begin+j]);
      //}

      begin = i;
      current_series = series_index[i];
    }
  }
  P[begin:N] = tQ_single(time[begin:N], vkcat, vkM, vET[begin:N], vST[begin:N]);
  return(P);
}
", sep = "\n"),
block = "functions")

#' Define formula for the tQ enzyme kinetics model
#'
#' @param multiple_perturbations (default FALSE)
#' @param predictors predictors to use for kcat and kM
#'
#'
#' @export
tQ_formula <- function(
    multiple_perturbations = FALSE,
    predictors = 0 + predictors,
    ...) {

  if (multiple_perturbations == FALSE) {
    predictor_eq <- rlang::new_formula(
      lhs = quote(kcat + kM),
      rhs = quote(1))
  } else{
    predictor_eq <- rlang::new_formula(
      lhs = quote(kcat + kM),
      rhs = rlang::enexpr(predictors))
  }

  brms::brmsformula(
    P ~ tQ_multiple(
      series_index, time, kcat, kM, ET, ST),
    predictor_eq,
    nl = TRUE,
    loop = FALSE,
    ...)
}


#' Define priors for the tQ enzyme kinetics model
#'
#' Default priors are gamma(4, 1) for both `kcat` and `kM`. We use the gamma
#' distribution because it is naturally lower bounded by 0. The first parameter
#' is the shape, `alpha=4`, and the second is the rate, `beta=1`. The mean of
#' gamma distributions is `alpha/beta` and the variance is `alpha/beta^2`.
#'
#' @param kcat prior for kcat parameter (Default: NULL). Given a numeric value,
#'   it will be used as a constant and not estimated.
#' @param kM prior for kM parameter (Default: NULL). Given a numeric value,
#'   it will be used as a constant and not estimated.
#'
#' @export
tQ_prior <- function(
    kcat = NULL,
    kM = NULL) {

  if (is.null(kcat)) {
    kcat_prior <- brms::prior(
      prior = gamma(4, 1),
      lb = 0,
      nlpar = "kcat")
  } else if (is.numeric(kcat)) {
    kcat_prior <- brms::prior_string(
      prior = paste0("constant(", kcat, ")"),
      nlpar = "kcat")
  } else {
    kcat_prior <- kcat
  }

  if (is.null(kM)) {
    kM_prior <- brms::prior(
      prior = gamma(4, 1),
      lb = 0,
      nlpar = "kM")
  } else if (is.numeric(kM)) {
    kM_prior <- brms::prior_string(
      prior = paste0("constant(", kM, ")"),
      nlpar = "kM")
  } else {
    kM_prior <- kM
  }

  c(kcat_prior, kM_prior)
}


#' Initialize parameter values for the the tQ enyzme kinetic model
#' @export
tQ_init <- function(
    kcat = 4,
    kM = 4,
    chains = 4) {

  init_list <- list(
    kcat = kcat,
    kM = kM)

  inits <- list()

  i <- 0
  while (i < chains) {
    inits <- append(inits, list(init_list))
    i <- i + 1
  }

  return(inits)
}

#' BRMS model for the tQ enzyme kinetics model
#' @export
tQ_model <- function(
    data,
    formula = mm_formula(),
    prior = NULL,
    init = 0,
    iter = 8000,
    control = list(adapt_delta = 0.99),
    ...) {

  if (is.null(prior)) {
    warning(
      paste0(
        "priors for kcat and kM are required. ",
        "Use tQ_priors function to get default priors."))
  }

  brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    iter = iter,
    control = control,
    stanvars = tQ_stanvars,
    ...)
}
