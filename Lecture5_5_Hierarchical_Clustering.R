# Data Analysis class - Lecture 5-5
#
# Provided by: AMULET Analytics (2013)
#
# Hierarchical Clustering

# ----------------------------------------------------------------
# Use a simulated data set with 4 points in each cluster

set.seed(1234)  # Set seed to get same data set each time
par(mar=c(0,0,0,0))  # Set plot margins: c(bottom, left, top, right)

# Define numeric vector, length=12
# vector of means: 1 1 1 1 2 2 2 2 3 3 3 3
x <- rnorm(12, mean=rep(1:3, each=4), sd=0.2)

# Define numeric vector, length=12
# vector of means: 1 1 1 1 2 2 2 2 1 1 1 1
y <- rnorm(12, mean=rep(c(1,2,1), each=4), sd=0.2)

plot(x,y, col="blue", pch=19, cex=2)

# Display integer labels to the upper-right of the dot
text(x+0.05, y+0.05, labels=as.character(1:12))

# ----------------------------------------------------------------

dataFrame <- data.frame(x=x, y=y)   # 12x2
# Calculate distance between points. dist() is just for clusterin

# Calculate distance between 12 points observed (distance between columns)
distxy <- dist(dataFrame)   # Default distance method = euclidean metric
#distxy <- dist(dataFrame, method="minkowski")   # Class=dist!!

# ----------------------------------------------------------------

# Produce cluster object
hClustering <- hclust(distxy)    # hclust requires a dist object, returns hclust object
# Plot dendrogram showing 3 clusters
plot(hClustering)   

# Now a prettier version of the dendrogram using function: myplclust()
# Label each leaf of the tree according to the cluster it belongs to
# Each cluster is labeled with "lab" parameter
myplclust(hClustering, lab=rep(1:3, each=4), lab.col=rep(1:3, each=4))

# myplclust() function definition
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ## hclust: hclust object
  ## lab: a character vector of labels of the leaves of the tree
  ## lab.col: colour for the labels; NA=default device foreground colour
  ## hang: as in hclust & plclust
  ## Side effect:
  ## A display of hierarchical cluster with coloured leaf labels.
  
  y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
  y <- y[order(x)]; x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
        labels=lab[hclust$order], col=lab.col[hclust$order],
        srt=90, adj=c(1,0.5), xpd=NA, ... )
}


# ----------------------------------------------------------------------

# Visualizing hierarchical clustering using a Heatmap
set.seed(143)

# Take a small sample of the rows. Each sample() returns different random seq
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]  # 12x2

# Clusters together the rows and columns
heatmap(dataMatrix)   # heatmap() requires matrix argument






