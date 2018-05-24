olive <- read.table("olive.txt",h=T)
newolive <- olive[,3:10]

# agnes
# Let us try first standardize the variables and use the “single” linkage:
library(cluster)
x <- daisy(newolive, stand=T)
agn<-agnes(x,metric="euclidean",method="single")
# Use the following interactive command for both the “dedrogram” and “banner plot” :
plot(agn,ask=T)
# or use the following command for only a dendrogram :
plot(agn,which.plots=2)




# Partitioning Method -----------------------------------------------------

# (1)K-means clustering
km <- kmeans(newolive,3,20)

# We show the clustering result on the 2-D plane of PC1 vs PC2 :
pca.newolive <- princomp(scale(newolive,scale=TRUE,center=TRUE),cor=FALSE)
pcs.newolive <- predict(pca.newolive)
plot(pcs.newolive[,1:2], type="n")
text(pcs.newolive,as.character(km$cluster),col=km$cluster,cex=0.6)

# For comparison, a similar plot can be derived from PCA :
plot(pcs.newolive[,1:2],type="n",xlab='1st PC',ylab='2nd PC')
text(pcs.newolive[,1:2],as.character(olive$Region),col=olive$Region,cex=0.6)

# From these two plots, we found that the original regions (shown in PCA) somehow disagree with the
# K-means clustering, especially on the overlap of “region 1” and “region 2”, the overlap of “region 1”
# and “region 3”.

# (2)pam
pa <- pam(daisy(newolive,stand = T), 3, diss = T)
plot(pa, ask = T)
# The clustering result (which takes a few seconds) is projected on a 2-D PC or MDS space:
# The SC (Silhouette Coefficient) is derived to be 0.3, which shows a weak structure of clustering.
# We can use the following command to see if the clustering recovers the original groups of “Regions” :
pa$clustering

# We can also compare this result with PCA :
plot(pcs.newolive[,1:2], type="n")
text(pcs.newolive,as.character(pa$clustering),col=pa$clustering,cex=0.6)

# Self-Organizing Maps (SOM) ----------------------------------------------

install.packages('som')
library(som)
n.newolive<-normalize(newolive, byrow=F) # Standardize variables
install.packages('kohonen')
library(kohonen)

# Run SOM with 20x20=400 grids (neurons), the default number of iterations = 100:
olive.som <- som(n.newolive,grid = somgrid(20, 20, "hexagonal"))
# We first mark the labels of “Region” in the resulting SOM:
plot(olive.som,type="mapping",labels=olive[,1])
# Another display to show clustering:
plot(olive.som, type="dist.neighbours", main = "SOM neighbour distances")

# 分五群show出來
som.hc <- cutree(hclust(dist(olive.som$codes[[1]])), 5)
add.cluster.boundaries(olive.som,som.hc)

# Observe the detailed clustering for each object:
# 每個圓圈被分到哪裡
cutree(hclust(dist(olive.som$codes[[1]])), 5)

# We can make a new SOM by changing the number of iterations, say, by setting “rlen” to be the total
# number of observations:

olive.som<-som(n.newolive, grid = somgrid(20, 20, "hexagonal"), rlen=572)
plot(olive.som,type="mapping",labels=olive[,1])
plot(olive.som, type="dist.neighbours", main = "SOM neighbour distances")
som.hc <- cutree(hclust(dist(olive.som$codes[[1]])), 3)
add.cluster.boundaries(olive.som,som.hc)

