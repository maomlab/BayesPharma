
#' Compute the Pnear quality metric for RMSD funnel
#'
#' The Pnear metric defined in [(Bhardwaj, et al., Nature, 2016)](https://www.nature.com/articles/nature19791)
#' measures the how "funnel-like" a score-vs-rmsd plot is.
#' https://github.com/RosettaCommons/main/blob/master/tests/benchmark/util/quality_measures.py#L268
#'
#' @details
#' ```{r}
#' # subtract off the min-score as is done in the Rosetta Code
#' scores = scores - min(scores)
#'
#' # write down the equation in more code-like notation
#' Pnear <- Sum_i[exp(-RMSD[i]^2/lambda^2)*exp(-scores[i]/k_BT)] /
#'          Sum_i[exp(-scores[i]/k_BT)]
#'
#' # combine the terms in the first exponential
#' Pnear = Sum_i[exp(-RMSD[i]^2/lambda^2 - scores[i]/k_BT)] /
#'         Sum_i[exp(-scores[i]/k_BT)]
#'
#' let x_i  = RMSD[i]^2/lambda^2 * k_BT/scores[i]
#'     beta = -scores[i]
#'
#' Pnear = Sum_i[exp(-RMSD[i]^2/lambda^2*k_BT/scores])]
#'
#' # Use the log-sum-exponential trick
#' log(Pnear) =   log_sum_exp(-RMSD[i]^2/lambda^2 - scores[i]/k_BT)
#'              - log_sum_exp(-scores[i]/k_BT)
#' ```
#'
#' @note Unlike the Conway discrimination score, the PNear calculation uses no
#' hard cutoffs.  This is advantageous for repeated testing: if the scatter of
#' points on the RMSD plot changes very slightly from run to run, the PNear
#' value will only change by a small amount, whereas any metric dependent on
#' hard cutoffs could change by a large amount if a low-energy point crosses an
#' RMSD threshold.
#'
#' @author Vikram K. Mulligan (vmulligan\@flatironinstitute.org) adapted from
#'   Rosetta
#'
#' @param score a vector of scores e.g. Rosetta energies e.g. in the Ref2015.
#' @param rmsd root mean squared deviation values for e.g. backbone atoms
#' @param lambda Lambda is a value in Angstroms indicating the breadth of the
#'   Gaussian used to define "native-like-ness".  The bigger the value, the more
#'   permissive the calculation is to structures that deviate from native.
#'   Typical values for peptides range from 1.5 to 2.0, and for proteins from
#'   2.0 to perhaps 4.0.
#' @param kbt The value of k_B*T, in energy units, determines how large an
#'   energy gap must be in order for a sequence to be said to favour the
#'   native state. The default value, 0.62, should correspond to physiological
#'   temperature for ref2015 or any other scorefunction with units of kcal/mol.
#' @param verbose give verbose output.
#' @return numeric value.
#'
#' @examples
#'\dontrun{
#'  Pnear(score = score_a, rmsd = rmsd_a)
#'}
#'
#' @export

Pnear <- function(
  score,
  rmsd,
  lambda = 1.5,
  kbt = 0.62,
  verbose = FALSE) {

  nscores <- length(score)
  if (nscores == 0) {
    stop("ERROR: length(nscores) == 0")
  }

  if (nscores != length(rmsd)) {
    stop(paste0(
      "ERROR: Length of scores and rsmds do not match:\n",
      "ERROR:    length(scores) == '", length(score), "'\n",
      "ERROR:    length(rmsds)  == '", length(rmsd),  "'\n"))
  }

  if (!all(rmsd >= 0)) {
    stop(paste0("ERROR: All RMSD values must be greater or equal to zero."))
  }

  if (!is.numeric(kbt) || kbt <= 0) {
    stop(paste0("ERROR: kbt must be great than zero"))
  }

  if (!is.numeric(lambda) || lambda <= 0) {
    stop(paste0("ERROR: lambda must be great than zero"))
  }

  score <- score - min(score)
  # log-sum-exponent trick is to make it more numerically stable
  # This may be important if there are lots of high-scoring points
  exp(
    matrixStats::logSumExp(-rmsd^2 / lambda^2 - score/kbt) -
      matrixStats::logSumExp(-score / kbt))
}
