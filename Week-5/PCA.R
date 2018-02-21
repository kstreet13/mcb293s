# --------------------------------
# MCB 293S
# Week 5
# PCA
# --------------------------------

# read in the data from the RData object
load('~/Projects/mcb293s/Week-5/expression_data.RData')

# Exploratory Data Analysis

class(expr)

dim(expr)

expr[1:10, 1:10]

boxplot(expr[,1:10])

# --------------------------------
# 1. Principal Components Analysis
# --------------------------------

pca <- prcomp(t(expr))

names(pca)

# What does the data look like?
require(scales)
plot(pca$x[,1:2])
plot(pca$x[,1:2], col = alpha(1, alpha = .5) )

require(rgl)
plot3d(pca$x[,1:3], aspect='iso')

# How important is each dimension?
barplot(pca$sdev^2)

barplot(pca$sdev[1:20]^2)

# Percentage of variance explained
total.var <- sum(pca$sdev^2)
pct.explained <- pca$sdev^2 / total.var

barplot(pct.explained[1:20])

pct.explained.total <- cumsum(pct.explained)
barplot(pct.explained.total[1:20])

barplot(pct.explained.total)

# We lose a LOT of information by going down to 3 dimensions!

# What genes drive the variance along PC1?
pca$rotation[1:10, 1:10]

barplot(pca$rotation[,1])

# 5 highest and 5 lowest loadings
cutoff <- sort(pca$rotation[,1], decreasing = TRUE)[5]
highest <- pca$rotation[,1] >= cutoff

cutoff <- sort(pca$rotation[,1], decreasing = FALSE)[5]
lowest <- pca$rotation[,1] <= cutoff

rownames(expr)[highest]
rownames(expr)[lowest]

plot(pca$x[,1:2], col = alpha(1, alpha = .5) )

# -----------------------
# 2. Clustering (k-means)
# -----------------------

# Using Euclidean distances in 1000 dimensions is a bad idea

# Using Euclidean distances in 3 dimensions is completely reasonable

dat <- pca$x[,1:3]

km <- kmeans(dat, centers=5)

km$cluster
table(km$cluster)

plot(dat, col = km$cluster)

plot3d(dat, aspect='iso', col = km$cluster)

# Look at stability
# If we run k-means again, do we get similar clusters?
km2 <- kmeans(dat, centers=5)

plot3d(dat, aspect='iso', col = km2$cluster)

confusion <- sapply(1:5,function(k1){
    table(factor(km2$cluster)[km$cluster==k1])
})

require(RColorBrewer)
barplot(confusion, col = brewer.pal(5,'Set1'),
        xlab = 'Original Cluster', names.arg = 1:5)

legend('topleft', fill = brewer.pal(5,'Set1'), legend = 1:5, cex = .6)

# Another useful clustering method
require(mclust)
mc <- Mclust(dat)

table(mc$classification)

plot3d(dat, aspect='iso', col = mc$classification)

plot3d(dat, aspect='iso', col = brewer.pal(8,'Set1')[mc$classification])

