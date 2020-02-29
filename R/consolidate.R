#' @title Consolidates replicate pairs in a binary matrix.
#'
#' @description Reads in a binary matrix comprising replicate pairs and consolidates each pair into a consensus read. For each replicate pair at each locus, 1 & 1 -> 1 (shared presence), 0 & 0 -> 0 (shared absence), 0 & 1 -> ? (ambiguity).
#'
#' @param x A CSV file containing replicate pairs of binary data. See the example input file "BinMatInput_reps".
#'
#' @return Consolidated binary matrix.
#' @examples data(BinMatInput_reps)
#' mat = BinMatInput_reps
#' cons = consolidate(mat)
#'
#' @export

consolidate = function(x){

  if(length(unique(x[,1]))!=length(x[,1])) {

    n = as.character(x[,1])
    dups = which(duplicated(n))
    stop(c("These sample names are duplicated in your dataset: ", c(n[dups]), ". Please ensure that all sample names are unique."))
  }

  else if ((nrow(x) %% 2) != 0) {

    stop(c("There is an odd number of samples in this dataset.\nIf you have a set of replicate pairs, there should be an even number. \nPlease correct this and re-upload your file."))

  }

  else {

    row.names(x) <- x[[1]]
    x[,1] <- NULL
    x[,] <- sapply(x[,], as.numeric)
    odd = x[seq(1, nrow(x), by = 2),]
    even = x[seq(2, nrow(x), by = 2),]
    new = odd+even
    new[,] <- lapply(new[,], gsub, pattern = "1", replacement = "?", fixed = TRUE)
    new[,] <- lapply(new[,], gsub, pattern = "2", replacement = "1", fixed = TRUE)
    nams = row.names(x)
    samplenames = vector()

    for(i in 1:(length(nams)/2))
    { #divide by two, because the new matrix is half the size (due to the rep pairs being combined)
      samplenames[i] = paste(nams[i*2-1], nams[i*2], sep = '+')
    }

    row.names(new) = samplenames
    colnames(new) = colnames(x)
    return(data.frame(new))

  }



}


