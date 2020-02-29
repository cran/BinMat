#' @title Checks binary matrix for unwanted characters.
#'
#' @description Checks for unwanted values (other than 1, 0, and ?).
#'
#' @param x A CSV file containing replicate pairs of binary data.
#'
#' @return Index positions where unwanted values occur (row, column).
#'
#' @examples data(BinMatInput_reps)
#' mat = BinMatInput_reps
#' check.data(mat)
#'
#' @export

check.data = function(x){
  row.names(x) <- x[[1]]
  x[,1] <- NULL
  x[,] <- sapply(x[,], as.numeric)
  answer = which(x != 0 & x != 1 & x != "?", arr.ind = TRUE)
  if(length(answer) > 0) message(answer)
  else {message("None found.")}

}


