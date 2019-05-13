setwd("D:/Courses/Multivariable Statisitcal Analysis/AS7/")
tea <- read.table("TEA.txt",header=T,sep="\t")
dim(tea)
View(tea)


c# A nice for loop to present the original variables
# barplots would also be a nice option here
# (note that a barplot is not the same thing as a histogram)

par(mfrow=c(2,3))

for(i in 1:dim(tea)[2]){

  tmp <- table(tea[,i])
  lab <- round(100*tmp/sum(tmp),1)

  pielabels <- paste(lab,"%",sep="")

  cols <- c("black","grey","green","red")

  title <- paste("Question",i,sep=" ")
  pie(tmp, main=title,col=cols,labels=pielabels,cex=0.8)

  legend("topleft",names(lab),fill=cols,cex=0.5)
}

par(mfrow=c(1,1))

#install.packages("ca") #The same package as last week
#install.packages("ggplot2")

library(ca)


help(mjca)


tea.mca <- mjca(tea,lambda="indicator")
# Setting lambda=indicator, the mca is performed like it is presented
# in the lecture slides
# you can use "nd=" here as in the last week

# Many of the objects here are exactly the same as last week,
# check the file R6.R from MyCourses
names(tea.mca)


tea.mca$factors
tea.mca$levels.n
sum(tea.mca$levels.n)
tea.mca$sv^2

# Note that only 29% of the variation is explained by the first 2
# components. However, we still proceed to analyze the first two components.
(tea.mca$sv[1]^2 + tea.mca$sv[2]^2) / sum(tea.mca$sv^2)

summary(tea.mca)

plot(tea.mca, arrows=c(T,T))
#Points allows us to add the observations to the plot

points(tea.mca$rowcoord) #Use text() like in previous exercises if you want to label the points


# Remember the angles are important:
# Between categories:
# less than 90 degrees = attaction
# more than 90 degrees = repulsion

# 90 degrees = independent

# Between individuals:
# less than 90 degrees = similar profile
# more than 90 degrees = profile differs

