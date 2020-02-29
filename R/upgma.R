#' @title Draws a hierarchical clustering tree (UPGMA).
#'
#' @description Creates an unweighted pair group method with arithmetic mean (UPGMA) hierarchical clustering tree, with a specified number of bootstrap repetitions.
#'
#' @param x Consolidated binarx matrix.
#' @param bts Bootstrap replications. Set to 10 by default.
#' @param method Distance method. Set to 'binary' (=Jaccard distance) by default.
#' @param hclust Clustering method. Set to 'average' (=UPGMA) by default
#' @param size Size of plot. Set to 0.55 by default.
#' @param lab_size Size of label text. Set to 0.55 by default.
#' @param fromFile Indicates whether the binary data used by the function has been consolidated by BinMat, or whether it comes from the user's own file. Set to FALSE by default (in the assumption that the data has been consolidated by BinMat, and that that object is being passed to the function).
#'
#'
#' @return UPGMA tree
#' @examples data(BinMatInput_reps)
#' mat = BinMatInput_reps
#' cons = consolidate(mat)
#' clust = upgma(cons)
#'
#' @export

upgma = function(x, bts = 10, size = 0.55, lab_size = 0.55, method = "binary", hclust="average", fromFile = FALSE){

  if(bts <= 0)
    stop("Enter a bootstrap repetition value > 0.")

  if (fromFile == TRUE){
    row.names(x) <- x[[1]]
    x[,1] <- NULL
  }

  new_names_upgma = substring(row.names(x),0,50)
  row.names(x) = new_names_upgma
  x[x=="?"] = NA
  x = as.data.frame(x)
  result = pvclust::pvclust(t(x), method.dist = method, method.hclust = hclust, nboot = bts) # 'average' method is the UPGMA
  dendro = graphics::plot(result, cex = size, print.num = F, print.pv = T, cex.pv = lab_size)

  return(dendro)
}
