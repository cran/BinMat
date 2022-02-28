#' Example input file of Bunias orientalis AFLP data, taken from Tewes et. al. (2017). This dataset has already been consolidated, and can be used
#' as input for the generation of an nMDS plot. The paper can be found here: <https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2745.12869>
#' @docType data
#'
#' @usage data(bunias_orientalis)
#'
#' @format A dataframe with columns for loci, and rows of replicate pairs. Grouping information is in the second column.
#' @examples data(bunias_orientalis)
#' mat = bunias_orientalis
#' group.names(mat)
#' nmds(mat, labs = TRUE, include_ellipse = TRUE)

"bunias_orientalis"
