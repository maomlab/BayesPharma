#' Generate Data from the Total QSSA (tQ) Model for Enzyme Kinetics
#'
#' Simulate data from the total QSSA (tQ) model a refinement of the
#' classical Michaelis-Menten enzyme kinetics ordinary differential
#' equation described in (Choi, et al., 2017, DOI:
#' 10.1038/s41598-017-17072-z). Consider the kinetic rate equation
#' \preformatted{
#'                   kf
#'                  --->    kcat
#'           E + S  <---  C --->  E + P
#'                   kb}
#'
#' where the free enzyme (E) reversibly binds to the substrate (S) to
#' form a complex (C) with forward and backward rate constants of kf
#' and kb, which is irreversibly catalyzed into the product (P), with
#' rate constant of kcat, releasing the enzyme to catalyze additional
#' substrate. The total enzyme concentration is defined to be the
#' `ET := E + C`. The total substrate and product concentration
#' is defined to be `ST := S + C + P`. The Michaelis constant is
#' the defined to be the `kM := (kb + kcat) / kf`. The kcat rate
#' constant determines the maximum turn over at saturating substrate
#' concentrations, `Vmax := kcat * ET`. The rate constants `kcat`
#' and `kM` can be estimated by monitoring the product accumulation
#' over time (enzyme progress curves), by varying the enzyme and
#' substrate concentrations.
#'
#' From (Choi, et al, 2017, equation 2, the total quasi-steady-state
#' approximation (tQ) differential equation is defined by
#' \preformatted{
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
#'      P := 0}
#'
#' In (Choi, et al. 2017) they prove, that the tQ model is valid when
#' \preformatted{
#'     K/(2*ST) * (ET+kM+ST) / sqrt((ET+kM+ST+P)^2 - 4*ET(ST-P)) << 1,}
#'
#' where K = kb/kf is the dissociation constant.
#'
#' @param time `numeric` vector. Increasing time points (e.g.
#'   `time[i] > time[i+1]`, for `i` in `[1, ... n]`) 
#' @param kcat `numeric` value catalytic rate constant
#' @param kM `numeric` value Michaelis rate constant
#' @param ET `numeric` value total enzyme concentration
#' @param ST `numeric` value total substrate concentration
#' @param ... additional arguments to [deSolve::ode()]
#'
#' @returns run the tQ ordinary differential equation forwards starting
#'   with initial product concentration of `0` and specified `kcat` and
#'   `kM` parameters for the specified time steps.
#'
#' @seealso [tQ_model]
#'
#' @references
#' Choi, B., Rempala, G.A. & Kim, J.K. Beyond the Michaelis-Menten equation:
#' Accurate and efficient estimation of enzyme kinetic parameters. Sci Rep 7,
#' 17018 (2017). https://doi.org/10.1038/s41598-017-17072-z
#'
#' @examples
#' \dontrun{
#' BayesPharma::tQ_model_generate(
#'   time = seq(0.1, 3, by = .5),
#'   kcat = 3,
#'   kM = 5,
#'   ET = 10,
#'   ST = 10) |>
#'   as.data.frame(col.names = c("time", "P_pred"))
#' }
#'
#' @export
tQ_model_generate <- function(time, kcat, kM, ET, ST, ...) {

  assertthat::assert_that(
    all(time == cummax(time)),
    msg = "Time is monotonic increasing")

  ode_tQ <- function(time, Pt, theta) {
    list(c(theta[1] * (
      ET + theta[2] + ST - Pt +
        - sqrt((ET + theta[2] + ST - Pt)^2 - 4 * ET * (ST - Pt))) / 2))
  }
  deSolve::ode(
    y = c(P = 0),
    times = time,
    func = ode_tQ,
    parms = c(kcat, kM),
    ...)
}
