#' @title Creates a shepard plot.
#'
#' @description Creates a Shepard plot for the non-metric multidimensional scaling plot. This indicates the 'goodness of fit' of the original distance matrix vs the ordination representation. A high R-squared value is favourable.
#'
#' @param x Consolidated binary matrix.
#' @param k_val Number of dimensions. Set to 2 by default.
#' @param dist_meth Distance method. Set to "binary" by default. Other options are "euclidean", "maximum", "manhattan", "canberra", or "minkowski".

#' @return Shepard plot.
#'
#' @examples mat = BinMatInput_ordination
#' shepard(mat)
#' @export

shepard = function(x, k_val = 2, dist_meth = "binary"){

  if(k_val <= 0)
    stop("Enter a positive k-value.")

  row.names(x) <- x[[1]] # make the sample names rownames,
  x[,1] <- NULL # and then remove the sample name column

  # make the names shorter (here, only 10 characters long)
  newnames =substring(row.names(x), 0, 50)
  row.names(x) = newnames
  x[x =="?"] <- NA

  x = as.data.frame(x)

  # x[,2:ncol(x)] starts calculating the distance from the second column to avoid including the grouping column information
  d = stats::dist((x[,2:ncol(x)]), method = dist_meth, diag = TRUE, upper = TRUE)
  d = as.data.frame(as.matrix(d))
  d2 = stats::as.dist(d)
  d2 = d2 + 0.01 # adding 0.01 here to cover for cases where there are identical sequences, leading to zero distances. Zero distances give the error "Warning: Error in isoMDS: zero or negative distance between objects x and y"

  isoplot = MASS::isoMDS(d2,k=k_val)

  shep = MASS::Shepard(d2,isoplot$points, p=2)
  summ = summary(stats::lm(shep$y~shep$x)) # R squared value
  r_sq = round(summ$r.squared, digits = 2)

  shep_plot = graphics::plot(shep, pch=16, xlab = "Original data distances", ylab = "Ordination distances", main = c("R-squared = ", r_sq))
  graphics::abline(stats::lm(shep$y~shep$x), col = "blue", lty = 2,lwd = 2)

  return(shep_plot)

}
