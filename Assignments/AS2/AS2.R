#a
set.seed(123)
library(mvtnorm)
set.seed(123)
n<-100
mu<-c(4,7)
Sigma<-matrix(c(10,6,6,8),byrow=T, ncol=2)
X<-rmvnorm(n,mu,Sigma)
rnx <- as.character(1:100)
plot(X)
text(X, labels=rnx)

#b
head(X)
dim(X)
help("princomp")
X.PCA<-princomp(X,cor=F)

#c
plot(X.PCA$scores,xlim = c(-10,12),ylim = c(-6,5))
text(X.PCA$scores,labels=rnx)

#d
#Conclution of comparision:
#1. The data was centered by y-axis
#2. The data points were rotated
#3. The distance of the points were not changed, only rotation were performed


#e
fun.cov <- function(A){
  m <- colMeans(A)
  
  #centred observations
  n = dim(A)[1]
  ones = rep(1,n)
  
  1/n * t(A-ones %*% t(m)) %*% (A-ones %*% t(m))
}
G <- eigen(fun.cov(X))$vectors
G1 <- eigen(fun.cov(X))$values
ones <- rep(1,100)
X1 <- colMeans(X)
Y <- (X-ones%*%t(X1))%*%G

#f
X.PCA$scores==Y

#g
#arrows in original data
plot(X,xlim=c(-3,14),ylim=c(0,15))
E <- X.PCA$loadings
arrows(5, 7, x1 = E[1,1], y1 =E[1,2])
arrows(5, 7, x1 = E[2,1], y1 =E[2,2])

#arrows in PCA data
plot(X.PCA$scores)
E <- X.PCA$loadings
arrows(0, 0, x1 = E[1,1], y1 =E[1,2])
arrows(0, 0, x1 = E[2,1], y1 =E[2,2])
#PCA always orthogonal