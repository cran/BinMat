#' Example input data containing a binary matrix comprising replicate pairs
#'
#' @docType data
#'
#' @usage data(BinMatInput_reps)
#'
#' @format A dataframe with columns for loci, and rows of replicate pairs.
#'
#' @examples data(BinMatInput_reps)
#' mat = BinMatInput_reps
#' check.data(mat)
#' cons = consolidate(mat)
#' pks = peaks.consolidated(cons)
#' err = errors(cons)
#' rem = peak.remove(cons, 4)
#' clust = upgma(cons)
#'
#'
"BinMatInput_reps"
