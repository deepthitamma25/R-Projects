# Question 1
q1_a<-27*(38-17)
q1_a
# Question 1(b)
q1_b<-log(147)
q1_b
# Question 1(c)
q1_c<-sqrt(436/12)
q1_c
# Question 2
a<-seq(5,160,by=5)
b<-seq(87,56,by=-1)
d<-a*b
# Question 2(a)
q2_a<-c(d[19],d[20],d[21])
q2_a
# Question 2(b)
q2_b<-d[d<2000]
q2_b
# Question 2(c)
q2_c<-length(d[d>6000])
q2_c
# Question 3
q3_a<-sum(d)
q3_a
# Question 3(b)
q3_b<-mean(d)
q3_b
# Question 3b(b)
q3b_b<-median(d)
q3b_b
# Question 3(c)
q3_c<-sd(d)
q3_c
# Question 4
data("cars")
q4<-head(cars)
q4
# Question 4(a)
q4_a<-hist(cars$dist,main="Histogram for Distance", xlab = "Distance",ylab="Frequency")
q4_a
# Question 4(b)
q4_b<-boxplot(cars$speed,main="Boxplot for Speed",xlab="Speed",ylab="Frequency")
q4_b
# Question 4(c)
q4_c<-plot(cars$dist,cars$speed, main="Scatter plot of Distance against Speed ", 
           xlab = "Distance",ylab = "Speed")
q4_c

# Part 2
library(mlbench)
data(Glass)
str(Glass)
library(lattice)
library(lattice)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(AppliedPredictiveModeling)
library(caret)
ggplot(Glass,aes(x=Glass$Na))+geom_histogram(fill="blue",col="black")+xlab("Na")+
  ggtitle("Histogram for Na")
ggplot(Glass,aes(x=Glass$Mg))+geom_histogram(fill="blue",col="black")+xlab("Mg")+
  ggtitle("Histogram for Mg")
ggplot(Glass,aes(x=Glass$Al))+geom_histogram(fill="blue",col="black")+xlab("Al")+
  ggtitle("Histogram for Al")
ggplot(Glass,aes(x=Glass$Si))+geom_histogram(fill="blue",col="black")+xlab("Si")+
  ggtitle("Histogram for Si")
ggplot(Glass,aes(x=Glass$K))+geom_histogram(fill="blue",col="black")+xlab("K")+
  ggtitle("Histogram for K")
ggplot(Glass,aes(x=Glass$Ca))+geom_histogram(fill="blue",col="black")+xlab("Ca")+
  ggtitle("Histogram for Ca")
ggplot(Glass,aes(x=Glass$Ba))+geom_histogram(fill="blue",col="black")+xlab("Ba")+
  ggtitle("Histogram for Ba")
ggplot(Glass,aes(x=Glass$Fe))+geom_histogram(fill="blue",col="black")+xlab("Fe")+
  ggtitle("Histogram for Fe")
# Pairwise Scatter Plots
pairs(Glass[1:9], pch = 21,main="Pairwise Scatterplots")
# Question 2 (a)
q2a<-Glass[1:9]
corr <- round(cor(q2a), 1)
ggcorrplot(corr)
cor(Glass[,-10])
# Question 2 (b)
q2b <- Glass[,1:9]
par(mfrow = c(3, 3))
for (i in 1:ncol(q2b)) 
  {
  plot(q2b[ ,i], ylab = names(q2b[i]), horizontal=T,
       main = paste(names(q2b[i]), "Scatterplot"), col="blue")
}
for (i in 1:ncol(q2b)) {
  p2dens <- density(q2b[,i], na.rm = TRUE)
  plot(p2dens, main = paste(names(q2b[i]), "Density"))
  polygon(p2dens, col="Blue")
}
# Question 2 (c)
q2c<-preProcess(Glass[,1:9],method=c("BoxCox","center","scale"))
G.trans<-predict(q2c,Glass[1:9])
str(G.trans)
str(Glass[1:9])

ggplot(G.trans,aes(x=G.trans$Na))+geom_histogram(fill="blue",col="black")+xlab("Na")+
  ggtitle("Transformed Histogram for Na")
ggplot(G.trans,aes(x=G.trans$Mg))+geom_histogram(fill="blue",col="black")+xlab("Mg")+
  ggtitle("Transformed Histogram for Mg")
ggplot(G.trans,aes(x=G.trans$Al))+geom_histogram(fill="blue",col="black")+xlab("Al")+
  ggtitle("Transformed Histogram for Al")
ggplot(G.trans,aes(x=G.trans$Si))+geom_histogram(fill="blue",col="black")+xlab("Si")+
  ggtitle("Transformed Histogram for Si")
ggplot(G.trans,aes(x=G.trans$K))+geom_histogram(fill="blue",col="black")+xlab("K")+
  ggtitle("Transformed Histogram for K")
ggplot(G.trans,aes(x=G.trans$Ca))+geom_histogram(fill="blue",col="black")+xlab("Ca")+
  ggtitle("Transformed Histogram for Ca")
ggplot(G.trans,aes(x=G.trans$Ba))+geom_histogram(fill="blue",col="black")+xlab("Ba")+
  ggtitle("Transformed Histogram for Ba")
ggplot(G.trans,aes(x=G.trans$Fe))+geom_histogram(fill="blue",col="black")+xlab("Fe")+
  ggtitle("Transformed Histogram for Fe")

#Part 3
library(mlbench)
data(Soybean)
str(Soybean)
library(rlang)
library(VIM)
library(mice)
library(dplyr)
#Question 3(a)
q3a<-Soybean[2:36]
par(mfrow = c(3, 5))
for (i in 1:ncol(q3a)) {
  smoothScatter(q3a[ ,i],col="blue" ,ylab = names(q3a[i]))
}
#Question 3(b)
aggr(Soybean, col=mdc(1:2), bars=TRUE, numbers=TRUE, sortVars=TRUE)
apply(Soybean[,-1],2,function(a){sum(is.na(a))})
Soybean$which_has_NaN_in_Samples<-apply(Soybean[,-1],1,function(x){sum(is.na(x))>0})
table(Soybean[,c(1,37)])
#Question 3(c)
p3a_imputes = mice(Soybean, m=5)
Imputed_data=complete(p3a_imputes,5)
aggr(Imputed_data, col=mdc(1:2), bars=TRUE, numbers=TRUE, sortVars=TRUE)

