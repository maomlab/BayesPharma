#'
#'
#'
#'
#'
#'

plot_pp_check <- function(model,
                          plot_type = "dens_overlay",
                          n = 100,
                          ...) {
  brms::pp_check(model, type = plot_type, nsamples = n,...)
}
