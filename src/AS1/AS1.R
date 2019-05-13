#a
#the unbiased estimator of matrix X is the sample covariance matrix
fa<-function(X){
  n<-nrow(A)   
  X1<-apply(X,2,mean)  #calculate the means of Matrix X
  X2<-X-X1             #remove the means of Matrix X
  k<-1/(n-1)
  Y<- sweep(X2%*%t(X2),2,k,"*")  #calculate the sample matrix
  return(Y)
}

#b
fb<-function(A){
  A1<-solve(A)      #calculate the inverse of A
  B<-eigen(A1)      #calculate the vector
  C<-B$vectors %*% diag(sqrt(B$values)) %*% solve(B$vectors) #calculate the square
  return(C)
}


#c
fc<-function(A){
  B=diag(A)        #calculate the diagnol matrix
  C<-eigen(B)      #calculate the vector
  D<-C$vectors %*% diag(sqrt(C$values)) %*% solve(C$vectors) #calculate the square
  InvD=solve(D)   #inverse of matrix D
  E = InvD%*% S%*%InvD   #Diagonalization of matrix
  return(E)
}


