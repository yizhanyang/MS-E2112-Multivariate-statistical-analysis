getwd()

library(ca)
library(gplots)

data <- read.table("SMOKING.txt",header = T, row.names = 1)

data <- data[,-5]
data <- data[-6,]

D <- as.matrix(data)

# Graph 
# 1. convert the data as a table
dt <- as.table(as.matrix(D))
balloonplot(t(dt), main ="Smoking of Employees in a Company", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# standardizing the row and columns so that they add to 1
row_prof <- prop.table(D,1) #row profile 
col_prof <- prop.table(D,2) #col profile 

#Correspondence Analysis
smoking_ca <- ca(D,nd=NA)

#Summary and Plot
summary(smoking_ca)
plot(smoking_ca,arrows=c(T,T))

## AR Matrix and bubble plot

v1 <- margin.table(D,1) # Gives you the sum of all the rows 
v2 <- margin.table(D,2) # Gives you the sum of all columns

n1 = length(v1)
n2 = length(v2)

V1 = matrix(v1,ncol = 1)
V2 = matrix(v2,ncol=n2)

E = V1 %*% V2 / sum(D)

AR.matrix <- D/E # D = original data (number of observations)
# E = expected number of observations under independence

# Graph 
# 1. convert the data as a table
dt <- as.table(as.matrix(AR.matrix))
balloonplot(t(dt), main ="Attraction Replusion for Smoking data", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE, scale.method=c("diameter"),
            dotcol = c("red","skyblue","skyblue","red","skyblue","skyblue",
                       "red","red","red","skyblue","skyblue","skyblue","skyblue",
                       "red","red","red","red","grey","skyblue","skyblue"))

