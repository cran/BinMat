#' @title Creates a non-metric multidimensional scaling plot (nMDS).
#'
#' @description Creates an nMDS plot from a consolidated binary matrix with grouping information. Colours and shapes of plotted points need to be specified. For example, if there are two groups, then: clrs = c("red", "blue"), sh = c(16, 16). This assigns red to the first group name, and blue to the second. Both will have a pch shape of 16 (round dot). These two vectors are then passed to the function nmds() as: colours = clrs, shapes = sh.
#'
#' @param x Consolidated binary matrix with grouping information in the second column.
#' @param dist_meth Distance method. Set to "binary" by default. Other options are "euclidean", "maximum", "manhattan", "canberra", or "minkowski".
#' @param k_val Number of dimensions for the nMDS plot. Set to 2 by default.
#' @param pt_size Point size for symbols on the plot. Set to 1 by default.
#' @param colours Vector containing colours to be assigned to groups.
#' @param shapes Vector containing pch values for shapes to be used for points.
#' @param labs Indicate whether labels should appear on the graph or not (TRUE or FALSE). Default = FALSE.
#' @param include_ellipse Indicate whether ellipses should be included around groups. Default = FALSE.
#' @param dimension1 Indicate the first dimension to plot (1, 2, or 3) for the x axis. If k = 2, the first two dimensions will automatically be plotted. If k = 3, select between the three.
#' @param dimension2 Indicate the second dimension to plot (1, 2, or 3) for the y axis
#' @return nMDS plot.
#'
#' @examples  mat = BinMatInput_ordination
#' group.names(mat)
#' clrs = c("red", "green", "black")
#' shps = c(16,16,16)
#' nmds(mat, colours = clrs, shapes = shps, labs = TRUE, include_ellipse = TRUE)
#'
#' @export

nmds = function(x, dist_meth = "binary", k_val = 2, pt_size = 1, colours, shapes, labs = FALSE, include_ellipse = FALSE, dimension1 = 1, dimension2 = 2){

  if(k_val <= 0)
    stop("Enter a positive k-value.")

  row.names(x) <- x[[1]] # make the sample names rownames,
  x[,1] <- NULL # and then remove the sample name column

  # make the names shorter (here, only 10 characters long)
  newnames =substring(row.names(x), 0, 50)
  row.names(x) = newnames
  x[x =="?"] <- NA

  #x = as.data.frame(x)

  d = stats::dist((x[,2:ncol(x)]), method = dist_meth, diag = TRUE, upper = T)
  d = as.data.frame(as.matrix(d))
  d2 = stats::as.dist(d)
  d2 = d2 + 0.01 # adding 0.01 here to cover for cases where there are identical sequences, leading to zero distances. Zero distances give the error "Warning: Error in isoMDS: zero or negative distance between objects x and y"

  isoplot = MASS::isoMDS(d2, k = k_val)
  fac = as.factor(x[,1]) # groups

  isoplot_df = as.data.frame(tibble::as_tibble( isoplot$points ))

  if(k_val == 2) colnames(isoplot_df) = c("Dimension 1", "Dimension 2")
  if(k_val ==3 ) colnames(isoplot_df) = c("Dimension 1", "Dimension 2", "Dimension 3")

  if(labs == TRUE) mds_labs = row.names(x)
  if(labs == FALSE) mds_labs = ""

  x_dimension = paste("Dimension", dimension1)
  y_dimension = paste("Dimension", dimension2)

  isoplot_df$groups = fac

  nmds = ggpubr::ggscatter(isoplot_df,
                    x = x_dimension,
                    y = y_dimension,
                    label = mds_labs,
                    color = "groups",
                    palette = colours,
                    shape = shapes[fac],
                    ellipse = include_ellipse,
                    size = pt_size
  )

  return(nmds)

}
