getwd()
library(ape)

# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

data(iris)
View(iris)

iris["Rows"] <- 1:150
iris["RowNames"] <- NA

row <- lab <- c("SE","VE","VIR")
iris$RowNames <- row[iris$Species]

labelColors = c('red', 'blue','green')

iris <- within(iris, Rows <- paste(iris[,7], iris[,6], sep='_')) 

rownames(iris) <- iris[,6]

plot(iris[,1:4])
legend("center", legend=c("Setosa","Versicolor","Virginica"),title="IrisGroup")
## install packages fpc, MASS, cluster
library(fpc)
library(MASS)
library(cluster)

lab <- c("Setosa","Versicolor","Virginica")

#plot(iris,panel = function(x,y) text(x,y,labels = lab,xpd=T))
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(iris[,1:4], col=c("red","blue","green")[iris$Species]);
#legend('topright', lab, col=c('red', 'blue', 'green'))
legend("center", inset=c(-20,-20), legend=c("Setosa","Versicolor","Virginica"), pch=c(1,3), title="IrisGroup")

iris.dist <- dist(iris,method="euclidean")



round(iris.dist,2)

min(iris.dist)
sort((round(iris.dist,1)))

iris.min <- hclust(iris.dist,method = "single")
clusMember = cutree(iris.min, 2)
iris.min = dendrapply(as.dendrogram(iris.min), colLab)

par(mfrow=c(1,1))
plot(iris.min,sub='Minimum Distance',main="Iris Data")

iris.max <- hclust(iris.dist,method = "complete")
plot(iris.max, main="Complete")
iris.ave <- hclust(iris.dist,method = "average")
plot(iris.ave, main="Average")





clusMember = cutree(iris.min, 3)
iris.min = dendrapply(as.dendrogram(iris.min), colLab)

clusMember = cutree(iris.max, 3)
iris.max = dendrapply(as.dendrogram(iris.max), colLab)

clusMember = cutree(iris.ave, 3)
iris.ave = dendrapply(as.dendrogram(iris.ave), colLab)

par(mfrow=c(1,3))
plot(iris.min,sub='Minimum Distance')
plot(iris.max,sub='Maximum Distance',main="Iris Data with three Clusters")
plot(iris.ave,sub='Average Distance')


