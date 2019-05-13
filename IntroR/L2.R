# setwd("path")
getwd()
############
# Section 1
############

# ctrl + shitft + c = comment/uncomment

svec <- c("red","yellow")

stest <- c("red","yellow","blue","green","pink")

comb1 <- paste(stest,collapse="_")
comb2 <- paste(stest,collapse=" ")
comb3 <- paste(stest,collapse="")

comb1
comb2
comb3

strsplit(comb1,"_") #returns a list
strsplit(comb2," ")
strsplit(comb3,"")

#omits the quation marks
svec2 <- noquote(svec)
svec2

p <- c(pi,pi^2,pi^3)



special0 = "one \t two \n 3"
cat(special0)
#note that you need cat() here. 
special <- c("one \t two", "3 \n 4", "5")
cat(special)

pow <- 1:3

#Note that you need cat() to make the print work correctly
s <- sprintf("Pi to the power %d is %.3f \n",pow,p)
cat(s)

############
# Section 2
############

heights <- data.frame(height=runif(n=8,min=140,max=200),
                      gender=sample( c("female","male"),8, replace=TRUE, prob=c(0.6,0.4)))
#dollar sign
names(heights)
class(heights$gender)
class(heights$height)

heights[1,1] <- 150  #this works
heights[1,2] <- "unknown" #this does not work unless gender is not converted to character 
levels(heights$gender)

heights$gender = as.character(heights$gender)

# if the data is corrupted, simulate it again

heights <- data.frame(height=runif(n=8,min=140,max=200),
                      gender=sample( c("female","male"),8, replace=TRUE, prob=c(0.6,0.4)))

levels(heights$gender)[3] <- "unknown"
heights[1,2] <- "unknown"
heights

happy_choices <-c("depressed", "grumpy", "cheery", "ecstatic")
happy_values <-sample(happy_choices,10000,replace = TRUE)

happy_fac <-factor(happy_values,happy_choices)
happy_ord <-ordered(happy_values,happy_choices)
#shows first values and the ordering
head(happy_ord)

b <- rbeta(10^4,2,3)

bmin <- min(b)
bmax <- max(b)

groups <- cut(b,seq(from=bmin,to=bmax,length.out=6))

class(groups)
table(groups)

#gl generate levels

############
# Section 3
############

#install.packages("ggplot2")
#install.packages("xlsx")
library(ggplot2)
library(xlsx) #note that this does not work with Linux


cars_excel <- read.xlsx("mtcars.xlsx",1,row.names=1,header=T)
cars <- read.table("mtcars.txt",sep="\t",header=T,row.names=1)

t(cars) %*% cars # Note that this does not work
# Matrix operations such as multiplication or det() 
# are not available for data.frames

c.matrix <- as.matrix(cars)
temp = t(c.matrix) %*% (c.matrix) #This works

ncol(c.matrix)
dim(t(c.matrix) %*% c.matrix)

cyl6 <- cars[cars$cyl==6,]
cyl6


############
# Section 4
############

now <- Sys.time()
class(now)

now2 <- Sys.time()
now2 - now 

# see lubridate if problems
# Sys.setlocale(category = "LC_ALL", locale = "us")

moon <- read.table("moon.txt",header=F,row.names=1)
moon_str <- as.character(moon$V2)
moon_time <-strptime(moon_str,format="%H:%M:%S %d/%m/%Y",tz="UTC") # UTC = universal time


moon_time[1] - moon_time[2]
outer(moon_time, moon_time, "-")

#for the weekdays, use %A. For the name of the month, use %B
strftime(moon_time, "%A")
strftime(moon_time, "It was %H:%M on %A %d of %B, %Y.")


############
# Section 5
############

library(ggplot2)

plot(cars$wt,cars$mpg,main="Scatter Plot",xlab="wt", ylab="Miles per gallon (mpg)")
dim(cars)

pairs(cars)
plot(cars)

pie(table(cars$cyl), main="Pie chart")
plot(cars$wt,cars$mpg,main="Scatter Plot",xlab="wt", ylab="Miles per gallon (mpg)",xlim=c(0,6),ylim=c(10,40))

# Note that you need to click on the observations in your plot
identify(cars$wt,cars$mpg,labels=rownames(cars))
# Click "Finish" on the right side of your screen to stop identify

cormat <- cor(cars)
library(reshape2)

melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), 
                       name="Pearson\nCorrelation")

#aesthetics defined in ggplot() will be passed to all layers
ggplot(cars, aes(x=wt, y=mpg)) +
  #geom_point(data=-cars,aes(x=wt,y=mpg),size=1.5,shape=15,colour="blue")+
  geom_point(size=1.5,shape=16,colour="red")+
  xlab("wt") +
  ylab("Miles per Gallon (MPG)") +
  ggtitle("Title")

ggplot() + geom_point(data=cars,aes(x=wt,y=mpg),size=1.5,shape=15,colour="red")+
  #geom_point(data=-cars,aes(x=wt,y=mpg),size=1.5,shape=15,colour="blue")+
  xlab("wt") +
  ylab("Miles per Gallon (MPG)") +
  ggtitle("Title")

HW <- read.table("HW.txt",header=T,sep="\t")

ggplot(HW,aes(x=ageYear,y=heightIn,colour=sex))+ geom_point()

ggplot(HW,aes(x=ageYear,y=heightIn,shape=sex, colour=sex))+ geom_point()
#makes the same plot as
ggplot()+ geom_point(data=HW,aes(x=ageYear,y=heightIn,shape=sex, colour=sex))

HW$weightGroup <- cut(HW$weightLb,breaks=c(-Inf,100,Inf),labels=c("<= 100", ">100"))

ggplot(HW,aes(x=ageYear,y=heightIn,shape=sex,fill=weightGroup)) +
  geom_point(size=2.5) +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_manual(values=c(NA, "black"),
                    guide=guide_legend(override.aes=list(shape=21)))


sp <-ggplot(HW,aes(x=ageYear,y=heightIn))
#lm=linear model, level=confidence level, se=TRUE = show confidence level
#vjust/hjust = text positions
sp +geom_point() +stat_smooth(method=lm,level=0.95, se =TRUE)+geom_text(aes(label=rownames(HW)),size=4,vjust=0, hjust=0)
# gray is 95% confidence region


FT <- read.table("FT.txt",header=T,sep="\t")

w <- FT$waiting

ggplot(FT,aes(x=waiting)) +geom_histogram()

hist(FT$waiting, breaks = c(43,50,60,65,78,88,96))

ggplot(FT,aes(x=waiting)) +
  geom_histogram(binwidth=5,fill="white",colour="black")

ggplot(FT,aes(x=waiting)) +
  geom_histogram(bins = 15,fill="white",colour="black")

#diff calculates the differences. In this case, max(FT$waiting)- min(FT$waiting)
binsize <-diff(range(FT$waiting))/15
ggplot(FT,aes(x=waiting)) +
  geom_histogram(binwidth=binsize,fill="white",colour="black")



BP <- read.table("BP2.txt",header=T,sep=" ")
head(BP)

lapply(BP,class) 
# Columns 3 and 8 contain problematic values

# Sometimes, errors are fixed by forcing type changes
BP2 <- apply(BP,c(1,2),as.numeric) # produces 3 errors

# complete.cases() gives rows that do not contain NA values 
which(complete.cases(BP2)==FALSE)

# rows 139,180, 366 are problematic
BP2[c(139,180,366),]

# Elements (139,3), (180,3) and (366,8) have been converted automatically to NA

# They are in the original data as:
BP[c(139,180,366),c(3,8)]

# It seems that they are dates in the original data set
# These kinds of incorrect input values sometimes cause large problems in R
# --> Be very carefull when uploading your data.