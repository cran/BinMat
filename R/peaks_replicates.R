#' @title Calculates peak numbers for the data set with all replicates (total, maximum, and minimum).
#'
#' @description Returns total, maximum, and minimum number of peaks in the binary matrix.
#'
#' @param x Binary matrix comprising replicate pairs.
#'
#' @return Peak information.
#'
#' @examples data(BinMatInput_reps)
#' mat = BinMatInput_reps
#' peaks.original(mat)
#'
#' @export

peaks.original = function(x){

  row.names(x) <- x[[1]]
  x[,1] <- NULL

  nr_peaks = matrix(nrow = nrow(x), ncol = 1)

  for(i in 1:nrow(x)) {
    total = 0
    for(j in 1:ncol(x)) {if(x[i,j] == 1) total = total + 1}

    nr_peaks[i,] = total

  }

  summary_table = data.frame("Summary" = matrix(ncol = 2, nrow = 5))
  summary_table[1,1] = "Average no. peaks: "
  summary_table[1,2] = round(base::mean(nr_peaks),4)
  summary_table[2,1] = "sd: "
  summary_table[2,2] = round(stats::sd(nr_peaks),4)
  summary_table[3,1] = "Max. no. peaks: "
  summary_table[3,2] = max(nr_peaks)
  summary_table[4,1] = "Min. no. peaks: "
  summary_table[4,2] = min(nr_peaks)
  summary_table[5,1] = "No. loci: "
  summary_table[5,2] = ncol(x)

  colnames(summary_table) = c("Metric", "Value")

  return(summary_table)
}
