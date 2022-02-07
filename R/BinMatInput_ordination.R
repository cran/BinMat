#' Example input data containing a consolidated binary matrix with groups
#' @docType data
#'
#' @usage data(BinMatInput_ordination)
#'
#' @format A dataframe with columns for loci, and rows of replicate pairs. Grouping information is in the second column.
#' @examples data(BinMatInput_ordination)
#' mat = BinMatInput_ordination
#' group.names(mat)
#' scree(mat)
#' shepard(mat)
#' clrs = c("red", "green", "black")
#' shp = c(16,16,16)
#' nmds(mat, colours = clrs, shapes = shp, labs = TRUE)

"BinMatInput_ordination"
