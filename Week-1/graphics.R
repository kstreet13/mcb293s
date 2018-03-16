
### 1-D plots ###

# continuous
x <- rchisq(500, df=2)

hist(x)

hist(x, breaks = 30, col = 'grey50')

colors()

plot(density(x))

boxplot(x, horizontal = TRUE)

par(pch = 16)

boxplot(x, col = 'grey75')


# categorical
x <- sample(c('red','blue','green'), 500, replace = TRUE, prob = c(.5,.2,.3))

barplot( table(x) )

barplot( table(x)/length(x) )

barplot( matrix( table(x)/length(x), ncol=1) )

x <- factor(x, levels = c('red','blue','green'))

barplot( table(x) )

barplot( table(x), col = c('red','blue','green'))

# aside: RColorBrewer
install.packages("RColorBrewer")
require(RColorBrewer)
display.brewer.all()

barplot( table(x), col = brewer.pal(3, 'Set1') )


### 2-D plots ###

# continuous x continuous
x <- rnorm(500)
y <- x + rnorm(500)

plot( x,y )

plot( x,y, col = rgb(0,0,0, .5))

require(scales)
plot( x,y, col = alpha(brewer.pal(3,'Set1')[2], .5))

x <- 1:100
y <- sqrt(x) + rnorm(100)

plot( x,y )

plot( x,y, type = 'l') # l = line

x <- 1:10
y <- sqrt(x) + rnorm(10)

plot( x,y, type = 'b') # b = both

plot( x,y )
lines( x,y )

# sneaking in an extra dimension
z <- sqrt(x + y^2)

plot( x,y, type = 'b', cex = z)

plot( x,y, type = 'b', cex = z/2)

clus <- 1 + (y > 2) + (y > 3)

plot( x,y, type = 'b', col = clus)

plot( x,y, type = 'l')
points( x,y, col = c('red','yellow','green')[clus])

plot( x,y, type = 'l')
points( x,y, col = c('red','yellow','green')[clus], cex = z/2)

plot( x,y, type = 'l')
points( x,y, col = colorby(z, colors = c('red','yellow','green')), cex = z/2)

### plot elements ###
axis()
{
  plot( x,y, type = 'b', axes = FALSE, xlab = 'Day', ylab = 'Outcome')
  axis(1, at = c(1,5,10))
}
lines()
{
  plot( x,y )
  lines( x,y )
}
points()
{
  
}
text()
{
  plot( x,y, type = 'b')
  ind <- which.min(y)
  text( x[ind],y[ind], 'MIN', pos = 4, col = 'red')
  ind <- which.max(y)
  text( x[ind],y[ind], 'MAX', pos = 2, col = 'red')
}
rect()
{
  plot( x,y )
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
  grid(col="white")
  points( x,y, type='b')
}
polygon()
{
  x1 <- rchisq(500, df=2)
  x2 <- rchisq(500, df=4)
  d1 <- density(x1)
  d2 <- density(x2)
  plot( range(c(d1$x,d2$x)), range(c(d1$y,d2$y,0)))
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
  grid(col="white")
  polygon( d1$x,d1$y, col=alpha('red',.5))
  polygon( d2$x,d2$y, col=alpha('blue',.5))
}
segments()
{
  x1 <- 1:30; y1 <- x1 + rnorm(30)
  x2 <- x1 + rnorm(30); y2 <- x2 + rnorm(30)
  plot( x1,y1, xlim = range(c(x1,x2)), ylim = range(c(y1,y2)))
  points( x2,y2, col = 'red')
  segments( x1, y1, x2, y2)
}


### Small Multiples ###
theta <- abs(rnorm(9))
data <- sapply(theta, function(t){ rnorm(500, sd=t) })
par(mfrow=c(3,3))
for(i in 1:9){
  plot(density(data[,i]), main=theta[i], xlim = range(data), xlab='', ylab='')
  #hist(data[,i], breaks = 30, col = 'grey50', xlim=range(data), main = theta[i])
}
par(mfrow=c(1,1))

boxplot(data, col = 'grey75')

pairs(data[,1:4], col=rgb(0,0,0,.25))


### Heatmaps ###
# generate some data (X):
{
  X <- matrix(NA, nrow = 100, ncol = 50)
  X[,1] <- rnorm(100)
  X[,2] <- rnorm(100)
  X[,3] <- rnorm(100)
  clus <- c(1:3, rep(NA,47))
  for(j in 4:50){
    i <- sample(1:3, 1)
    X[,j] <- .75*X[,i] + sqrt(1-.75^2)*rnorm(100)
    clus[j] <- i
  }
  rownames(X) <- paste('feature',1:100, sep='')
  colnames(X) <- paste('sample',1:50, sep='')
}

boxplot(X)

heatmap(X)

heatmap(X, col = heat.colors(50))

heatmap(X, col = topo.colors(50))

mypal <- colorRampPalette( c('red','black','green') )
heatmap(X, col = mypal(50))

heatmap(X, Rowv = NA)

require(gplots)
heatmap.2(X)

heatmap.2(X, trace = 'none')

heatmap.2(X, trace = 'none', Rowv = NA, dendrogram = 'col')

heatmap.2(X, trace = 'none', Rowv = NA, dendrogram = 'col', ColSideColors = c('red','blue','green')[clus])

heatmap.2(X, trace = 'none', Rowv = NA, dendrogram = 'col', ColSideColors = c('red','blue','green')[clus], RowSideColors = topo.colors(100))

heatmap.2(X, trace = 'none', dendrogram = 'col', ColSideColors = c('red','blue','green')[clus], RowSideColors = topo.colors(100))


### 3D plots ###
pca <- prcomp(t(X))

require(rgl)
plot3d( pca$x[,1:3] )

plot3d( pca$x[,1:3], col = c('red','blue','green')[clus])

plot3d( pca$x[,1:3], col = c('red','blue','green')[clus], size = 5)

plot3d( pca$x[,1:3], col = c('red','blue','green')[clus], size = 5, aspect = 'iso')
