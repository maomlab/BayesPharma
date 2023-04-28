#' Stan code for the tQ enzyme kinetics model
#'
#' The tQ is an ordinary differential equation model for the total
#' quasi-steady-state assumption kinetics defined in
#' (Choi et al., 2017), which is related to the Michaelis-Menten kinetics model,
#' but doesn't assume the enzyme concentration is negligibly small.
#'
#' To implement the tQ model in \pkg{Stan}, the function `tQ_ode` is defined
#' and then passed to `tQ_single` to integrate it using the _stiff backward
#' differentiation formula (BDF) method_. To fit multiple time series
#' in one model, the `tQ_multiple` can be used. Note that to handle fitting
#' time-series with different numbers of observations, an additional
#' `series_index` argument is used. Note that observations in the same
#' time-series should be in sequential order in the supplied data.
#'
#' @usage
#' brms::brm(
#'   data = ...,
#'   formula = brms::brmsformula(
#'     P ~ tQ_multiple(series_index, time, kcat, kM, ET, ST),
#'     kcat + kM ~ 1,
#'     nl = TRUE,
#'     loop=FALSE),
#'   prior = ...,
#'   init =  ...,
#'   stanvars = BayesPharma::tQ_stanvar)
#'
#' @seealso [tQ_model], [tQ_formula], [tQ_prior], or [tQ_init]
#'
#' @references
#' Choi, B., Rempala, G.A. & Kim, J.K. Beyond the Michaelis-Menten equation:
#' Accurate and efficient estimation of enzyme kinetic parameters. Sci Rep 7,
#' 17018 (2017). https://doi.org/10.1038/s41598-017-17072-z
#'
#' @export
tQ_stanvar <- brms::stanvar(
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

  vector[2] params = [ vkcat[1], vkM[1] ]';
  vector[1] initial_state = [0.0]';
  real initial_time = 0.0;
  int M = dims(time)[1];

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
  data vector series_index,
  data vector time,
  vector vkcat,
  vector vkM,
  data vector vET,
  data vector vST) {

  int N = size(time);
  vector[N] P;
  int begin = 1;
  real current_series = series_index[1];
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
