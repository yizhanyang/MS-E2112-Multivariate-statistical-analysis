#Strings
#1
getwd()
svec <- c("red","yellow")
svec
str(svec)
class(svec)

#2
?paste
stest <- c("red","yellow","blue")
stest2 <- paste(stest,collapse = '')


#3
?strsplit
stest2
unlist(strsplit(stest2,'_'))
strsplit(stest2,'_')

#4
svec
noquote(svec)

#5
p <- c(pi,pi^2,pi^3)
p

#6
special0 <- c('one\ttwo\n3\n4')
cat(special0)
plot(1:4,1:4,main='Hello\nhow are you doing?')

#7
pow <- 1:3
p <- c(pi,pi^2,pi^3)
s <- sprintf('pi to the power %d is %.3f\n',pow,p)
cat(s)

#Factor
#8
heights <- data.frame(height=runif(n=8,min=140,max=200),
                      gender=sample(c('Famale','Male'),8,replace=T,prob=c(0.6,0.4)))
heights

#9
class(heights)
str(heights)

#10

heights[1,2] <- 'Unknown'

#11
levels(heights$gender)
levels(heights[,2])
levels(heights$gender)[3] <- 'unknown'
levels(heights$gender)
heights[1,2] <- 'unknown'
heights


#12
?ordered

happ<- (c('depressed','grumpy','cheery','ecstatic'))
happv <- sample(happ,1000,replace=T)
table(happv)
class(happv)
happf <- factor(happv,happ)
happf
class(happf)
as.factor(happv)
happ.ord <- ordered(happv,happ)
class(happ.ord)
happ.ord

#13
b <- rbeta(10^4,2,3)
hist(b)
groups <- cut(b,seq(from=min(b),to=max(b),length.out=6))
head(groups)


#14
table(groups)
barplot(table(groups))
pie(table(groups))

#15
install.packages('ggplot2')
library(ggplot2)

#16
list.files()
install.packages('xlsx')
library(xlsx)
cars_exel <- read.xlsx('mtcars.xlsx',1,row.names=1,header = T)
head(cars_exel)
str(cars_exel)

#17
getwd()
cars<- read.table('mtcars.txt',sep='\t',header=T,row.names = 1)
head(cars)

#18
typeof(cars)
as.matrix(cars)
t(as.matrix(cars))%*%as.matrix(cars)
det(cov(as.matrix(cars)))

#19
cars[cars$cyl==6,]
cars[cars$cyl>3,]

#or
subset(cars,cars$cyl==6)

##########
#section 4
#20
now <- Sys.time()
now
class(now)
str(now)
#21
now2 <- Sys.time()
now2
now2-now

#22
moon <- read.table('moon.txt',header = F,row.names = 1)
moon
dim(moon)
moon_str <- as.character(moon$V2)
moon_str
moon_time <- strptime(moon_str,format='%H:%M:%S %d/%m/%Y',
                      tz='UTC')
moon_time
class(moon_time)

#24
strftime(moon_time,'%A')
strftime(moon_time,'It was %H:%M:%S %d/%m/%Y')

?strptime

#section 5, Graphical tools
plot(cars$wt,cars$mpg)

#25
?pairs
pairs(cars)

#26
pie(cars$cyl)
pie(table(cars$cyl),
    main='pie chart of number of cyl',
    labels = c('4','6','8'),
    col=c(1,2,3))
table(cars$cyl)

#27
plot(cars$wt,cars$mpg,
     main = 'scatter plot',
     xlab='wt',ylab='consumption[mpg]',
     pch=21,col=1,bg=2,cex=1.5)

#28
plot(cars$wt,cars$mpg,
     main = 'scatter plot',
     xlab='wt',ylab='consumption[mpg]',
     pch=21,col=1,bg=2,cex=1.5,
     xlim=c(0,6),ylim = c(10,40))

#29
identify(cars$wt,cars$mpg,labels=rownames(cars))

#30
install.packages('reshape2')
library(reshape2)
heatmap(cor(cars))

cormat <- cor(cars)
cormat

melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat,aes(x=Var1,y=Var2,fill=value))+
         geom_tile()+
         scale_fill_gradient2(low='blue',high='red',mid='white',
                               midpoint=0,lim=c(-1,1),
                               name='pearson\ncorrelation')

#31
head(cars)
ggplot(data=cars,aes(x=wt,y=mpg))+
  geom_point(size=1.5,shape=16,col='red')+
  xlab('wt')+
  ylab('consumption[mpg]')+
  ggtitle('Fuel')

#32
HW <- read.table('HW.txt',header=T,sep = '\t')
head(HW)                              
str(HW)


)+
  geom_point

#33
HW$weightGroup <- cut(HW$weightLb,breaks = c(-Inf,100,Inf),
                      labels = c('<=100','>100'))
head(HW)
str(HW)
ggplot(data = HW,aes(x=ageYear,y=heightIn,shape=sex,
                     fill=weightGroup))+
  geom_point(size=2.5)+
  scale_shape_manual(values = c(21,24))+
  scale_fill_manual(values = c(NA,'black'),
                    guide=guide_legend(override.aes = list(shape=21)))
ccc <- HW$weightGroup

#34
sp <- ggplot(HW,aes(x=ageYear,y=heightIn))
sp+geom_point()+
  stat_smooth(method = lm,level=0.95,se=TRUE)

#36
FT <- read.table('FT.txt',header = T,sep='\t')
w <- FT$waiting
ggplot(FT,aes(x=waiting))+
  geom_histogram()

hist(FT$waiting,breaks = c(43,50,60,65,78,88,96))
hist(FT$waiting,breaks = 5)
hist(FT$waiting,breaks = 15)

#37
BP <- read.table('BP2.txt',header = T,sep='')
head(BP)
BP
str(BP)
BP2 <- apply(BP,c(1,2),as.numeric)
which(is.na(BP2),arr.ind = T)
str(BP2)