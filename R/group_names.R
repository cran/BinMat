#' @title Outputs group names specified in the input file for the creation of an nMDS plot.
#'
#' @description Returns group names in the uploaded consolidated binary data. This will help in knowing which colours are assigned to which group name.
#'
#' @param x Consolidated binary matrix with grouping information in column 2.
#'
#' @return Scree plot.
#'
#' @examples mat = BinMatInput_ordination
#' group.names(mat)
#'
#' @export

group.names = function(x){

  grps = as.factor(x[,2])
  print(levels(grps))

}
