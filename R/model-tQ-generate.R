#' Generate data from the total QSSA model for enzyme kinetics
#' 
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
tQ_model_generate <- function(time, kcat, kM, ET, ST, ...) {
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