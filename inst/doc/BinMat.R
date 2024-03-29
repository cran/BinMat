## ----setup, fig.height = 5, fig.width = 5-------------------------------------
library(BinMat)

data1 = BinMatInput_reps
data2 = BinMatInput_ordination

# data1 contains all the replicate pairs that need to be consolidated into a consensus output
# data2 contains a consolidated binary matrix with grouping information in the second column

# Check the data for unwanted values
check.data(data1)
# Get information about peak numbers for all replicates
peaks.original(data1)
# Consolidate the replicate pairs in the matrix
cons = consolidate(data1)
# View the original matrix 
data1
# View the consolidated output
cons
# Get the Jaccard and Euclidean error rates
errors(cons)
# Get information about the peak numbers in the consolidated matrix
peaks.consolidated(cons)
# Create a hierarchical clustering tree using the UPGMA method
clustTree = upgma(cons, size = 0.6)
# Find samples with peaks less than a specified threshold value, and return the new, filtered data set
filtered_data1 = peak.remove(cons, thresh = 6)
filtered_data1

# data2 contains an already-consolidated matrix with grouping information. This is used to create a scree, shepard, and an nMDS plot.

# # Find samples with peaks less than a specified threshold value, and return the new, filtered data set
filtered_data2 = peak.remove(data2, thresh = 7)
filtered_data2
# Get the names of the groups specified in the second column
group.names(data2)
# Create an object containing colours for each group
# Colours: Africa = red, Australia = blue, Europe = dark green
clrs = c("red", "blue", "darkgreen")
# Create a scree plot to check how the number of dimensions for an nMDS plot will affect the resulting stress values
scree(data2)
# Create a shepard plot showing the goodness of fit for the original data vs the ordination data
shepard(data2)
# Create an nMDS plot for the data. Default dimension is 2
nmds(data2, colours = clrs, labs = TRUE)


## ----fig.height = 5, fig.width = 5--------------------------------------------

bunias = bunias_orientalis
group.names(bunias)
nmds(bunias, labs = FALSE, include_ellipse = TRUE, legend_pos = "right")


## ----fig.height = 7, fig.width = 7--------------------------------------------

nymph = nymphaea
group.names(nymph)
colrs = c("dodgerblue", "black", "red", "green3", "orange", "darkblue", "gold2", "darkgreen", "darkred", "grey", "darkgrey", "magenta", "darkorchid", "purple", "brown", "coral3", "turquoise", "deeppink", "lawngreen", "deepskyblue", "tomato")
nmds(nymph, labs = FALSE, include_ellipse = FALSE, colours = colrs, legend_pos = "right", pt_size = 2)


