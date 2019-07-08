#a

#setwd("...")
help(c)
help(matrix)


#b
A <- matrix(c(2,1,5,-2,7,0,5,-8,-1),ncol=3,byrow=TRUE)

x1 <- c(8,-4,2)
x2 <- matrix(c(8,-4,2),nrow=1)
b <- c(3,10,-19)

y1 <- x1%*% solve(A) + b
y2 <- x2%*% solve(A) + b



# Note that when multiplying vectors and matrices, the safe way
# is to always use variables of the class matrix


#c
set.seed(123)

#install.packages("mvtnorm") # Run this only once

library(mvtnorm) # Run this after restarting RStudio

n <- 100
mu <- c(3,1)
Sigma <- matrix(c(4,1,1,2),byrow=T,ncol=2)

X <- rmvnorm(n,mu,Sigma)

plot(X)


#d 
mx <- apply(X,2,mean)
colMeans(X)
Sx <- cov(X)

sum(diag(Sx)) - sum(eigen(Sx)$values)
prod(eigen(Sx)$values) -det(Sx)


#e
b <- c(3,1)
A <- matrix(c(1,2,3,1),byrow=T,ncol=2)
Y <- sweep(X%*%t(A),2,b,"+")

#another way
ones = rep(1,n)
Y2 = X%*%t(A) + ones%*%t(b)
# When comparing if multivariate expressions are the same,
# use e.g. the Frobenius norm
norm(colMeans(Y) - A %*% colMeans(X) - b, type="F")
norm(cov(Y) - A %*% cov(X) %*% t(A), type="F")


#f

# Here, we make a conversion to type matrix,
# note that many of the basic matrix operations are not 
# available for variables of type data.frame
D1 <- Data1
pairs(D1)

center <- function(X){
  pairs(X)
  ave <- apply(X,2,mean)
  cent <- sweep(X,2,ave,"-")
  
  return(cent)  
}

C <- center(D1)
apply(C,2,mean)

cov(C)
cor(C)

eigen(cov(C))$values
eigen(cor(C))$values

eigen(cov(C))$vectors
eigen(cor(C))$vectors
a11<-dim(X)
a12<-length(X)



