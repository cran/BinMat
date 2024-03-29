#' Example input file of Nymphaea ISSR data, taken from Reid et. al. (2021).
#' This dataset has already been consolidated, and can be used
#' as input for the generation of an nMDS plot. The paper can be found here: <https://www.sciencedirect.com/science/article/pii/S0304377021000218>
#' @docType data
#'
#' @usage data(nymphaea)
#'
#' @format A dataframe with columns for loci, and rows of replicate pairs. Grouping information is in the second column
#' @example nymph = nymphaea
#' group.names(nymph)
#' colrs = c("dodgerblue", "black", "red", "green3", "orange", "darkblue", "gold2", "darkgreen", "darkred", "grey", "darkgrey", "magenta", "darkorchid", "purple", "brown", "coral3", "turquoise", "deeppink", "lawngreen", "deepskyblue", "tomato")
#' nmds(nymph, labs = FALSE, include_ellipse = FALSE, colours = colrs, legend_pos = "right", pt_size = 2)

"nymphaea"
