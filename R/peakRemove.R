#' @title Removes samples with peaks equal to or less than a specified threshold value.
#'
#' @description Removes samples with a peak number less than a specified value.
#' @param x Binary matrix - consolidated or original.
#' @param thresh Peak threshold value for removal.
#'
#' @return Filtered dataset, and either the row name/s or row number/s of samples that were removed.
#' @examples mat = BinMatInput_ordination
#' new = peak.remove(mat, 4)
#'
#' @export

peak.remove = function(x, thresh) {

  #row.names(x) <- x[[1]] # make the sample names rownames,
  #x[,1] <- NULL # and then remove the sample name column

  peak_record = matrix(data = NA, nrow(x), ncol = 1)

  for(i in 1:nrow(x)){
    count = length(which(x[i,2:ncol(x)]==1))
    peak_record[i,]=count
  }

  if(thresh <= 0)
    stop("Enter a threshold value greater than zero.")
  if(thresh > max(peak_record))
    stop(c("Do not remove samples with peaks less than ", max(peak_record)+1, ", as that the maximum peak number detected in your data set is ", max(peak_record), "."))

  else {

    x_tally = cbind(x, peak_record)
    x_keep = subset(x_tally, peak_record >= thresh)
    x_removed = subset(x_tally, peak_record < thresh)
    x_keep$peak_record = NULL
    x_removed$peak_record = NULL
    names(x_keep)[1]="Sample"

    #x_keep[is.na(x_keep)]<-"?"

   # names(x_removed)[1]="Group"

    message(c("Number of samples removed: ", nrow(x_removed)))

    if(nrow(x_removed) > 0)
    message(c("This was/these were: ", rownames(x_removed)))

    return(x_keep)
  }

}
