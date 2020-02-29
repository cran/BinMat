#' @title Creates a non-metric multidimensional scaling plot (nMDS).
#'
#' @description Creates an non-metric multidimensional scaling plot from a consolidated binary matrix with grouping information. Colours and shapes of plotted points need to be specified. For example, if there are two groups, then: clrs = c("red", "blue"), sh = c(16, 16). This assigns red to the first group name, and blue to the second. Both will have a pch shape of 16 (round dot). These two vectors are then passed to the function nmds() as: colours = clrs, shapes = sh.
#'
#' @param x Consolidated binary matrix with grouping information in the second column.
#' @param dist_meth Distance method. Set to "binary" by default. Other options are "euclidean", "maximum", "manhattan", "canberra", or "minkowski".
#' @param k_val Number of dimensions for the nMDS plot. Set to 2 by default.
#' @param pt_size Point size for symbols on the plot. Set to 1 by default.
#' @param colours Vector containing colours to be assigned to groups.
#' @param shapes Vector containing pch values for shapes to be used for points.
#' @param labs Indicate whether labels should appear on the graph or not (TRUE or FALSE). Default = FALSE.
#' @return nMDS plot.
#'
#' @examples  mat = BinMatInput_ordination
#' group.names(mat)
#' clrs = c("red", "green", "black")
#' shp = c(16,16,16)
#' nmds(mat, colours = clrs, shapes = shp, labs = TRUE)
#'
#' @export

nmds = function(x, dist_meth = "binary", k_val = 2, pt_size = 1, colours, shapes, labs = FALSE){

  if(k_val <= 0)
    stop("Enter a positive k-value.")

  row.names(x) <- x[[1]] # make the sample names rownames,
  x[,1] <- NULL # and then remove the sample name column

  # make the names shorter (here, only 10 characters long)
  newnames =substring(row.names(x), 0, 50)
  row.names(x) = newnames
  x[x =="?"] <- NA

  x = as.data.frame(x)

  d = stats::dist((x[,2:ncol(x)]), method = dist_meth, diag = TRUE, upper = TRUE)
  d = as.data.frame(as.matrix(d))
  d2 = stats::as.dist(d)
  d2 = d2 + 0.01 # adding 0.01 here to cover for cases where there are identical sequences, leading to zero distances. Zero distances give the error "Warning: Error in isoMDS: zero or negative distance between objects x and y"

  isoplot = MASS::isoMDS(d2, k = k_val)
  fac = as.factor(x[,1])

  nmds = MASS::eqscplot(isoplot$points, xlab = "Dimension 1", ylab = "Dimension 2", col = colours[fac], pch = shapes[fac], cex = pt_size)

  if(labs == T)
    graphics::text(isoplot$points, labels = row.names(x), cex = 1, pos= 4)

  return(nmds)

}
