#' @title Calculates peak numbers (total, maximum, and minimum).
#'
#' @description Returns total, maximum, and minimum number of peaks.
#'
#' @param x Binary matrix comprising replicate pairs.
#'
#' @return Peak information.
#'
#' @examples data(BinMatInput_reps)
#' mat = BinMatInput_reps
#' peaks(mat)
#' @export

peaks = function(x){

  nr_peaks = matrix(nrow = nrow(x), ncol = 1)


  for(i in 1:nrow(x)) {
    total = 0
    for(j in 1:ncol(x)) {if(x[i,j] == 1) total = total + 1}

    nr_peaks[i,] = total

  }

  summary_table = data.frame("Summary" = matrix(ncol = 1, nrow = 10))
  summary_table[1,] = "Average no. peaks: "
  summary_table[2,] = round(base::mean(nr_peaks),4)
  summary_table[3,] = "sd: "
  summary_table[4,] = round(stats::sd(nr_peaks),4)
  summary_table[5,] = "Max. no. peaks: "
  summary_table[6,] = max(nr_peaks)
  summary_table[7,] = "Min. no. peaks: "
  summary_table[8,] = min(nr_peaks)
  summary_table[9,] = "No. loci: "
  summary_table[10,] = ncol(x)-1

  return(summary_table)
}
