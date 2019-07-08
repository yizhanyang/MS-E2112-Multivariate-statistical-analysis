#a
#setwd()
help(c)
help(matrix)

#b
A<-matrix(c(2,1,5,-2,7,0,5,-8,1),ncol=3,byrow=TRUE)
x1<-c(8,-4,2)
x2=matrix(c(8,-4,2),nrow=1)
b<-c(3,10,-19)
y1<-x1%*%solve(A)+b
y2<-x2%*%solve(A)+b


#c
set.seed(123)
library(mvtnorm)
n<-100
mu<-matrix(c(3,1),nrow=2)
sigma<-matrix(c(4,1,1,2),ncol=2,byrow=T)
X<-rmvnorm(n,mu,sigma)
plot(X)

#d
mx<-apply(X,2,mean)
help(apply)
a1<-colMeans(X)
Sx<-cov(X)

#e
b<-c(3,1)
A<-matrix(c(1,2,3,1),nrow=2,byrow=T)
help(t)


