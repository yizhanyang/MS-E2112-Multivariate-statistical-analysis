getwd()

data <- read.table("SMOKING.txt",header = T, row.names = 1)

data <- data[,-5]
data <- data[-6,]

D <- as.matrix(data)

# standardizing the row and columns so that they add to 1
prop.table(D,1) #row profile 
prop.table(D,2) #col profile 

v1 <- margin.table(D,1) # Gives you the sum of all the rows 
v2 <- margin.table(D,2) # Gives you the sum of all columns

n1 = length(v1)
n2 = length(v2)

V1 = matrix(v1,ncol = 1)
V2 = matrix(v2,ncol=n2)

E = V1 %*% V2 / sum(D)

AR.matrix <- D/E # D = original data (number of observations)
# E = expected number of observations under independence

round(AR.matrix,2)

library(ggplot2)
library(reshape)

melted <- melt(AR.matrix)
View(melted)
range(melted$value)
ggplot(melted, aes(x=X1, y=X2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = median(melinstall.ted$value), limit = c(0.5, 1.55),
                       name="AR value")


