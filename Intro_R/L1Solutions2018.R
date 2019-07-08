#Section 2
# Basic stuff

#setwd("path")
demo()
#etc...

# ctrl+enter works for windows also
# note that ctrl+r might not work in some versions of RStudio

#Section 3

a <- 10.5
b <- 10
class(a)
class(b)
is.integer(a)
is.integer(b)

############################
#Note that
b2 <- 1:10
is.integer(b2[1])
is.integer(b2)
b2[11] = pi

#Important because
tmp1 <- 1:10^6
tmp2 <- c(rep(NA,length(tmp1)))

for(i in 1:length(tmp1)){
  tmp2[i] <- 0 + i
}

class(tmp1)
class(tmp2)

object.size(tmp1) 
object.size(tmp2)
#tmp2 takes twice the memory
###########################

c <- as.integer(3)
d <- as.integer(3.7)

is.integer(c)
is.integer(d)
c
d

e <- complex(re=1,im=2)
# or e <- 1 +2i
f <- TRUE

name <- "Niko"
age <- 27


paste(name,"is aged",age, sep = "_")
#Alternatively, use sprintf() that works similarly as in C

as.character(a)

ls()
ls.str()

rm(list = ls())
rm(a)


#Section 4
1:30
seq(1,30,1)
seq(1,30)
30:1
c(1:20,19:1)

a <- 1:3
#or
a <- c(1,2,3)

rep(a,5)
rep(1:3,5)

rep(a,length=6+5+5)
c(rep(a,5), 1)

rep(1:3,times=c(10,20,30))


(2^(1:20))/(1:20)

paste("label", 1:30)

# R does the addition (makes the first vector of length 15)
# Be careful! Even if you can do something in R
# does not mean that you should
1:5 + 1:15
#same as
rep(1:5,3) + 1:15

# note that the following does not work since 15 is not a multiple
# of 6
1:6 + 1:15


tmp <- 1:25

sum((2^tmp)/tmp + 3^tmp/(tmp^2))

tmp2 <- 1:20
sum( tmp2^2 + 2*tmp2^3 )

set.seed(12)
x <- sample(0:500,250,replace=T)
y <- sample(0:500,250,replace=T)


# Ex 21
len <- length(x)

y[-1] - x[-len]

x[-c(len-1,len)] + 2*x[-c(1,len)]-x[-c(1,2)]

sum(exp(-x[-1])/(x[-len]+10))

y[y >100]

which(y>100)
y[which(y>100)]

xmean <- mean(x)

sqrt(abs(x-mean(x)))

sort(x,decreasing=T)

sort(x,decreasing=F)


x[c(F,T,F)] #fast way

x[seq(from=2,to=250,by=3)] #safe way

#
#cumprod gives a vector of cumulative products of the input vector
1 + sum(cumprod(seq(2,20,by=2)/seq(3,21,by=2)))


10/4 #Division
10%/%4 #Integer division. Uses floor function


#Special number values
1/0 #Infinity
sqrt(-1) #NaN, not a number
# missing values NA := not available



#Section 5
A <- matrix(c(1,1,3,5,2,6,-2,-1,-3),byrow=T,nrow=3)
View(A) # Try to click on the objects under Data on the right (environment)

A%*%A%*%A

# For larger matrix powers to make calculation faster
# you should always try to utilize some decomposition

# If no suitable decomposition available,
# larger matrix powers should be calculated via for-loop

A[,3] <- A[,1] + A[,2]

set.seed(12)
B <- matrix(rnorm(15*10),nrow=15)

t(B) %*% B

solve(t(B)%*%B)

colMeans(B)
rowMeans(B)

# The apply-family functions are very optimized in R
# learning to use them makes your life considerably easier

apply(B,2,mean)
apply(B,1,mean)


# function(arg1,arg2,..){  return(returnvalue)}
apply(B,1,function(x){sum(x>0.5)})

B_colsums <- colSums(B)

#outer(0:3, seq(7,15,length.out = 3), "+")
which(outer(B_colsums,B_colsums,"+")>6,arr.ind=T)
#for example, columns 5 and 2, 7 and 2 have sum greater than 6

A <- outer(0:4,0:4,"+")

tmp1 <- cbind(A,t(A))
tmp2 <- cbind(t(A),A)

#rbind requires common col-names
B <- rbind(tmp1,tmp2)

y <- c(7,-1,-3,5,17)

A <- abs(col(A)-row(A))+1
A = abs(row(A)-col(A))+1

x <- solve(A) %*% y

A %*% x #same as y, solution is correct


list1 <- list(1:3,matrix(1:9,nrow=3))

list1[[2]][1,2] <- 100 #remember the double brackets 
list1


#Section 6

# Return is not required in the following functions
# However, makes the code more readable for those with
# experience with other programming languages

fun1 <- function(x){
  return(x^(1:length(x)))
}

fun2 <- function(x){
  
  temp <- fun1(x)
  temp/(1:length(x))
}



fun3 <- function(x,n){
  1+ sum(   (x^(1:n))/(1:n)  )
}


#colMeans
fun.mean.vector <- function(A){
  n <- dim(A)[1]
  colSums(A)/n
}
fun.mean.vector==colMeans


#########
#fun.cov <- function(A){
  #m <- fun.mean.vector(A)
  
  #centred observations
  #Cent <- apply(A,1,"-",m)
  
  #n <- dim(A)[1]
  
  #1/(n-1) * Cent %*% t(Cent)
#}
#########

fun.cov <- function(A){
  m <- fun.mean.vector(A)
  
  #centred observations
  n = dim(A)[1]
  ones = rep(1,n)
  
  1/(n-1) * t(A-ones %*% t(m)) %*% (A-ones %*% t(m))
}
cov(A)==fun.cov(A)


#a%%b = a mod b
fun6 <- function(A){
  A[A%%2==1] <- 2*A[A%%2==1]
  return(A)
}
