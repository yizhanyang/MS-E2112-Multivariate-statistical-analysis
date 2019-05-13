setwd("D:/Courses/Multivariable Statisitcal Analysis/Project/")

library(ggplot2)
library(e1071)
library('MASS')

cancer <- read.csv("data.csv",header = T,sep = ",")
dim(cancer)
    
    
## permute the data
cancer <- cancer[sample(nrow(cancer)),]

data <- cancer[,1:9]
#### Plot the data###########################################################
#require(ggplot2)
library(data.table)
library(reshape2)  # for melt(...)
library(plyr)      # for .(...)

# Histogram
multi.hist <- function(x) {nvar <- dim(x)[2]  #number of variables
nsize=trunc(sqrt(nvar))+1   #size of graphic
old.par <- par(no.readonly = TRUE) # all par settings which can be changed
par(mfrow=c(3,3))       #set new graphic parameters
for (i in 1:nvar) {
  name=names(x)[i]                #get the names for the variables
  hist(x[,i],main=name,xlab=NULL,ylab=NULL,col=c("steelblue")) }  #draw the histograms for each variable
on.exit(par(old.par))   #set the graphic parameters back to the original
}
multi.hist(data)
#############################################################################
xx <- with(cancer, data.table(id=1:nrow(cancer), group= Age,	BMI,	Glucose,	Insulin,	HOMA,	Leptin,	Adiponectin,	Resistin,	MCP.1
))
# reshape for facetting with ggplot
yy <- melt(xx,id=1:2, variable.name="H", value.name="xval")
  yy <- data.table(yy,key="id,group")
ww <- yy[,list(V=H,yval=xval),key="id,group"]
zz <- yy[ww,allow.cartesian=T]
setkey(zz,H,V,group)
zz <- zz[,list(id, group, xval, yval, min.x=min(xval), min.y=min(yval),
               range.x=diff(range(xval)),range.y=diff(range(yval))),by="H,V"]
# points colored by group (=Class)
# density plots for each variable by group
d  <-  zz[H==V, list(x=density(xval)$x,
                     y=mean(min.y)+mean(range.y)*density(xval)$y/max(density(xval)$y)),
          by="H,V,group"]

a = d[subset(d,H=V)$id]

a = d[which(d$H == d$V)]

ggp = ggplot(zz)
ggp = ggp + geom_point(data = subset(zz,H!=V),aes(x=xval, y=yval, color=factor(group)), 
                                                               size=3, alpha=0.5) + 
  geom_line(data = a,aes(x=x,y=y,color=factor(group))) +
  facet_grid(V~H, scales="free")+
  scale_color_discrete(name="CancerType") +
  labs(x="", y="") +
  theme(panel.background = element_rect(colour = "black"))+
  theme_bw()
ggp

#applyk-means
library(factoextra)
library(ggplot2)
data$Age = as.numeric(data$Age)
hr = subset(data,data$left==1)
X = data
Y = hr[,7]
X.pca = prcomp(X,center=TRUE,scale.=TRUE)
print(X.pca$rotation)
print(summary(X.pca))
par(mfrow=c(1,2))
fviz_screeplot(X.pca,choice='eigenvalue')

fviz_pca_var(X.pca)
new=as.data.frame(predict(X.pca,newdata=X))

set.seed(100)
new.cluster<-kmeans(new,2,nstart=10)
ggplot(data=new,mapping = aes(x=PC1,y=PC2,color=cluster))+
  geom_point(shape=1)+
  ggtitle("Clusters of brest cancer(k=2)")+ 
  theme(plot.title = element_text(hjust = 0.5))
new$cluster<-as.factor(new.cluster$cluster)
par(mfrow=c(2,3))
hist(X[,1],main="satisfaction level",ylab=NULL,xlab=NULL,col=c("steelblue"))
hist(X[,2],main="last evaluation",ylab=NULL,xlab=NULL,col=c("steelblue"))
hist(X[,3],main="number of projects",ylab=NULL,xlab=NULL,col=c("steelblue"))
hist(X[,4],main="average monthly hours",ylab=NULL,xlab=NULL,col=c("steelblue"))
hist(X[,5],main="time spent at companies",ylab=NULL,xlab=NULL,col=c("steelblue"))

## Boxplot
norm.data <- data.frame(scale(data))
meltData <- melt(norm.data)

ggplot(meltData,aes(x=variable,y=value)) + geom_boxplot(outlier.colour = "green", fill="grey80") +
  theme(axis.text.x = element_text(face="bold",size=18),
        axis.text.y = element_text(face="bold",size=18),
        axis.title=element_text(size=18,face="bold"))
  


ggplot(meltData,aes(x=variable,y=value)) + geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 2500))

###### Box plot with outlier labels
#function that takes in vector of data and a coefficient,
#returns boolean vector if a certain point is an outlier or not

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

  group_by() 
  mutate(outlier = ifelse(is_outlier(drat), drat, as.numeric(NA))) %>%
  ggplot(., aes(x = factor(cyl), y = drat)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)

#plot
ggplot(dat,aes(x=group,y=value))+geom_boxplot()+geom_text(aes(label=label),hjust=-0.3)

############################################################################
ggplot(data = data, aes())
target <- data.frame(cancer[,10])



norm.data["Class"] <- target <- data.frame(cancer[,10])

##############################################################################################################
# Just for checking with normalized data. But same accuracy as unnormalized data.

cancer.lda.cv <- lda(Class~.,data=norm.data,CV=T)

result <- data.frame(est=cancer.lda.cv$class,truth=cancer$Class)

tab <- table(result)

tab

1-sum(diag(tab))/nrow(cancer)
##############################################################################################################

train_data <- cbind(data[1:86,],target[1:86,])
colnames(train_data)[10] <- c("target") 

test_data <- data[87:106,]
test_target <- data.frame(target[87:106,])

#colMeans(norm.data)  # faster version of apply(scaled.dat, 2, mean)
#apply(norm.data, 2, sd)

plot(norm.data)

mean_data <- colMeans(data)
cov_data = cov(data)
corr_data <- cor(data)
#library("corrplot", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")
library(corrplot)
corrplot(corr_data, method = "circle", type = "upper")

## LDA on the normalized data



train_data[,10] <- factor(train_data[,10])


cancer.lda <- lda(target~.,data=train_data)

names(cancer.lda)

cancer.lda

# this is vector 'a'
cancer.lda$scaling

cancer.lda$means

# new data

#newdata<- data.frame(Sepal.Length=6,Sepal.Width = 3,Petal.Length=4,Petal.Width=1)

test_pred <- predict(cancer.lda,newdata=test_data)$class

result <- data.frame(est=test_pred,truth=test_target)

tab <- table(result)
tab_dataframe <- data.frame(tab)

misclassification_rate <- 1-sum(diag(tab))/nrow(test_target)

###

cancer.lda.cv <- lda(Class~.,data=cancer,CV=T)

result <- data.frame(est=cancer.lda.cv$class,truth=cancer$Class)

tab <- table(result)

tab

1-sum(diag(tab))/nrow(cancer)

# 33.96226% error using the cross validation

