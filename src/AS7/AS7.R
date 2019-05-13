setwd(("D:/Courses/Multivariable Statisitcal Analysis/AS7"))
data <- read.table('WG93_full.txt',header=T,sep='\t')

reponse <- data[,c(1,2,3,4)]

tableA <- table(data$A)
tableB <- table(data$B)
tableC <- table(data$C)
tableD <- table(data$D)

labA <- round(100*tableA/sum(tableA),1)
labB <- round(100*tableB/sum(tableB),1)
labC <- round(100*tableC/sum(tableC),1)
labD <- round(100*tableD/sum(tableD),1)

pielabelsA <- paste(labA,"%",sep="")
pielabelsB <- paste(labB,"%",sep="")
pielabelsC <- paste(labC,"%",sep="")
pielabelsD <- paste(labD,"%",sep="")

cols <- c("green4","greenyellow","honeydew4","red","red4")
names_response <- c("strongly agree","agree","Neutral","disagree","strongly disagree")

pie(tableA,main="Believe science often but not in faith/feelings",col=cols,labels=pielabelsA,cex=0.8)
legend("topleft",names_response,cex=0.8,fill=cols)
pie(tableB,main="Overall modern science does more harm than good",col=cols,labels=pielabelsB,cex=0.8)
legend("topleft",names_response,cex=0.8,fill=cols)
pie(tableC,main="Any change to nature will make things worse",col=cols,labels=pielabelsC,cex=0.8)
legend("topleft",names_response,cex=0.8,fill=cols)
pie(tableA,main="Modern science will solve environment problems with no change",col=cols,labels=pielabelsD,cex=0.8)
legend("topleft",names_response,cex=0.8,fill=cols)


library(ca)
library(ggplot2)

data.mca <- mjca(data,lambda="indicator")

data.mca$factors ## check help for answers
data.mca$levels.n
data.mca$sv^2  ## square to get eigen values

((data.mca$sv[1]^2 + data.mca$sv[2]^2) / sum(data.mca$sv^2))

plot(data.mca)
names_response1 <- c("A:Believe Science not Faith","B:Modern Science destruction","C:Change damage Nature","D:Modern Science solve problems","1:strongly agree","2:agree","3:Neutral","4:disagree","5:strongly disagree")
legend("bottomright",names_response1,cex=0.8)


cats <- apply(data,2,function(x) nlevels(as.factor(x)))

data.vars <- data.frame(data.mca$colcoord,Variable=rep(names(cats),cats))

data.vars

rownames(data.vars) <- data.mca$levelnames


library(ca)




data.mca <- mjca(data,lambda="indicator")
# Setting lambda=indicator, the mca is performed like it is presented
# in the lecture slides
# you can use "nd=" here as in the last week

# Many of the objects here are exactly the same as last week,
# check the file R6.R from MyCourses
names(data.mca)


data.mca$factors
data.mca$levels.n
sum(data.mca$levels.n)
data.mca$sv^2

# Note that only 29% of the variation is explained by the first 2
# components. However, we still proceed to analyze the first two components.
(data.mca$sv[1]^2 + data.mca$sv[2]^2) / sum(data.mca$sv^2)

summary(data.mca)

plot(data.mca, arrows=c(T,T))
#Points allows us to add the observations to the plot

points(data.mca$rowcoord)

