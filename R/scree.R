#' @title Draws a scree plot.
#'
#' @description Creates a scree plot for the nMDS. This indicates the optimum number of dimensions to use to minimise the stress value.    The stress value is indicated by a red dotted line at 0.15. Values equal to or below this are considered acceptable.
#'
#' @param x Consolidated binary matrix with grouping information in column 2.
#' @param dimensions Number of dimensions to plot. Set to 4 by default.
#' @param dist_meth Distance method. Set to "binary" by default. Other options are "euclidean", "maximum", "manhattan", "canberra", or "minkowski".
#' @return Scree plot.
#' @examples mat = BinMatInput_ordination
#' scree(mat)
#'
#'
#' @export

scree = function(x, dimensions = 4, dist_meth = "binary") {

  if(dimensions <=0)
    stop("Enter a positive number for dimensions.")

  row.names(x) <- x[[1]] # make the sample names rownames,
  x[,1] <- NULL # and then remove the sample name column

  # make the names shorter (here, only 10 characters long)
  newnames =substring(row.names(x), 0, 50)
  row.names(x) = newnames
  x[x =="?"] <- NA

  x = as.data.frame(x)

  # x[,2:ncol(x)] starts calculating the distance from the second column to avoid including the grouping column information
  d = stats::dist((x[,2:ncol(x)]), method = dist_meth, diag = TRUE, upper = T)
  d = as.data.frame(as.matrix(d))
  d2 = stats::as.dist(d)
  d2 = d2 + 0.01 # adding 0.01 here to cover for cases where there are identical sequences, leading to zero distances. Zero distances give the error "Warning: Error in isoMDS: zero or negative distance between objects x and y"

  scree.plot = function(d, k) {
    stresses=MASS::isoMDS(d, k=k)$stress
    for(i in rev(seq(k-1)))
      stresses=append(stresses,MASS::isoMDS(d, k=i)$stress)
    graphics::plot(seq(k),rev(stresses), type="b", xaxp=c(1,k, k-1), ylab="Stress (%)", xlab="Number of dimensions")
  }

  scree_plot = scree.plot(d2, k=dimensions)
  graphics::abline(b=0,h=15, col = "red", lty = 2)

  return(scree_plot)

}
