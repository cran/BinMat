#' @title Creates a non-metric multidimensional scaling plot (nMDS).
#'
#' @description Creates an nMDS plot from a consolidated binary matrix with grouping information. Colours and shapes of plotted points need to be specified. For example, if there are two groups, then: clrs = c("red", "blue"), sh = c(16, 16). This assigns red to the first group name, and blue to the second. Both will have a pch shape of 16 (round dot). These two vectors are then passed to the function nmds() as: colours = clrs, shapes = sh.
#'
#' @param x Consolidated binary matrix with grouping information in the second column.
#' @param dist_meth Distance method. Set to "binary" by default. Other options are "euclidean", "maximum", "manhattan", "canberra", or "minkowski".
#' @param k_val Number of dimensions for the nMDS plot. Set to 2 by default.
#' @param pt_size Point size for symbols on the plot. Set to 1 by default.
#' @param colours Vector containing colours to be assigned to groups. This can be changed to the options available in the RColorBrewer palette set (e.g. "Set1"). See <http://applied-r.com/rcolorbrewer-palettes/> for more palette options.
#'  Alternatively, the colours can be set manually using, for example, c("red", "green", "blue"), thereby setting a colour for each group
#'  in your dataset. There are 28 default colours that will be set automatically to your groups.
#' @param shapes Vector containing pch values for shapes to be used for points. The default is set to round filled circles (pch = 16).
#' To change this to custom shapes, use something like c(2,3,16) for each group you have in the dataset.
#' @param labs Indicate whether labels should appear on the graph or not (TRUE or FALSE). Default = FALSE.
#' @param legend_pos Indicate the position of the legend. Default = "right", but other options are "left", "bottom", "top", or "none"
#' @param include_ellipse Indicate whether ellipses should be included around groups. Default = FALSE.
#' @param ellipse_type Select the type of ellipses to include around groups. Options are "convex", "confidence", "t", "norm", and "euclid". See the ggpubr::ggscatter() function documentation for more details.
#' @param dimension1 Indicate the first dimension to plot (1, 2, or 3) for the x axis. If k = 2, the first two dimensions will automatically be plotted. If k = 3, select between the three.
#' @param dimension2 Indicate the second dimension to plot (1, 2, or 3) for the y axis
#' @return nMDS plot.
#'
#' @examples  mat = BinMatInput_ordination
#' group.names(mat)
#' clrs = c("red", "green", "black")
#' shps = c(16,10,12)
#' nmds(mat, colours = clrs, shapes = shps, labs = TRUE, include_ellipse = TRUE)
#'
#' @export

nmds = function(x, dist_meth = "binary", k_val = 2, pt_size = 1,
                colours = c("dodgerblue", "black", "red", "green3", "orange", "darkblue", "gold2",
                            "darkgreen", "darkred", "grey", "darkgrey", "magenta", "darkorchid",
                            "purple", "brown", "coral3", "turquoise", "deeppink", "lawngreen",
                            "deepskyblue", "tomato", "yellow", "yellowgreen",
                            "royalblue", "olivedrab", "midnightblue", "indianred1", "darkturquoise"),
                shapes = 16, labs = FALSE,
                legend_pos = "right",
                include_ellipse = FALSE,
                ellipse_type = "norm", dimension1 = 1,
                dimension2 = 2){

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

  isoplot_df = suppressWarnings( as.data.frame(tibble::as_tibble( isoplot$points )) )

  if(k_val == 2) colnames(isoplot_df) = c("Dimension 1", "Dimension 2")
  if(k_val ==3 ) colnames(isoplot_df) = c("Dimension 1", "Dimension 2", "Dimension 3")

  if(labs == TRUE) mds_labs = row.names(x)
  if(labs == FALSE) mds_labs = ""

  x_dimension = paste("Dimension", dimension1)
  y_dimension = paste("Dimension", dimension2)

  isoplot_df$groups = fac

  if(length(shapes) == 1){shapes_nmds = rep(shapes, length(fac))} # if only one shape is specified, apply it to all groups
  else shapes_nmds = shapes

  nmds = suppressWarnings( ggpubr::ggscatter(isoplot_df,
                    x = x_dimension,
                    y = y_dimension,
                    label = mds_labs,
                    color = "groups",
                    palette = colours,
                    shape = shapes_nmds[fac],
                    ellipse = include_ellipse,
                    ellipse.type = ellipse_type,
                    size = pt_size,
                    legend = legend_pos,
                    legend.title = "Groups",
                    show.legend.text = FALSE

  ) ) # end of suppress warnings


  return(nmds)

}
