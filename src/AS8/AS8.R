getwd()

dec_data <- read.table("DECATHLON.txt",header=T,sep="\t")

X <- dec_data[,c(3,4,5,6,7,8,9,10,11,12)] 
Y <- dec_data[,c(13,14)] 

# (a)
XY <- as.matrix(cbind(X,Y))

R <- cov(XY)
R11 <- R[1:2,1:2]
R22 <- R[3:12,3:12]
R21 <- R[3:12,1:2]
R12 <- R[1:2,3:12]
R11.inv <- solve(R11)
R22.inv <- solve(R22)

#Non-zero eigenvalues of M1 and M2 are the same
M1 <- R11.inv %*% R12 %*% R22.inv %*% R21
M2 <- R22.inv %*% R21 %*% R11.inv %*% R12


# (sometimes R calculates the eigenvectors multiplied with -1)
va1 <- eigen(M1)$vectors[,1]
va2 <- eigen(M1)$vectors[,2]

vb1 <- eigen(M2)$vectors[,1]
vb2 <- eigen(M2)$vectors[,2]

vb1=as.numeric(vb1)
vb2=as.numeric(vb2)
# a1 = alpha1 , b1 = beta1
# Correct scaling: Whitening of data
a1 <- -va1/sqrt(va1%*%R11%*%va1)
a2 <- -va2/sqrt(va2%*%R11%*%va2)
b1 <- vb1/sqrt(vb1%*%R22%*%vb1)
b2 <- vb2/sqrt(vb2%*%R22%*%vb2)

#b)
fii1 <- XY[,1:2]%*%a1
fii2 <- XY[,1:2]%*%a2
eta1 <- XY[,3:12]%*%b1
eta2 <- XY[,3:12]%*%b2

round(cor(cbind(fii1,fii2,Re(eta1),Re(eta2))),2)

#canonical correlations
# g1 = 0.78
# g2 = -0.49

#c)

# Value = "Loss of value" = "How fast the value goes down"??

# u1 <- 0.0574*Height + 0.1079*Weight
# v1 <- -0.0037*R100m - 0.0007*Long Jump + 0.0085*Shot Put - 0.0011*High Jump - 0.0004*R400m
#       + 0.003*Hurdles + 0.0061*Discus Throw -0.0040*Pole Vault -0.0029*Javelin -0.0007*R1500

#games require throwing requires more strength

plot(fii1,Re(eta1),xlab="'Physical Strength'",ylab="'Stand and deliver games'",pch="")
text(fii1,Re(eta1),labels=dec_data$NAME)


#d)
# u2 <- -0.3710*Height + 0.2573*Weight
# v2 <- -0.0067*R100m - 0.0017*LongJump - 0.0052*Shot Put - 0.0014*High Jump + 0.0112*R400m
#     + 0.0110*Hurdles + 0.0118*DiscusThrow - 0.0014*Pole Vault + 0.0019*Javelin + 0.0082*R1500


plot(fii2,Re(eta2),xlab="'Stamina'",ylab="'Running Games'",pch="")
text(fii2,Re(eta2),labels=dec_data$NAME)