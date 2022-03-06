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
#' nmds(mat, colours = clrs, labs = TRUE)
"BinMatInput_ordination"
