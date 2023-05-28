mydata<-read.csv("Immunization.csv")
head(mydata)

#Cleaning the data
#Removing all the Null Values
newdata<-na.omit(mydata)
View(newdata)
View(mydata)
mydata$K_12_enrollment
plot(mydata$Grade_Levels)
#Scatter plots
devtools::install_github("kassambara/ggpubr")
library("ggpubr")
ggscatter(newdata, x = "Number_with_personal_exemption", y = "Number_with_religious_exemption", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number_with_personal_exemption", ylab = "Number_with_religious_exemption")
scatter.smooth(newdata$K_12_enrollment,newdata$Percent_with_any_exemption,
main="ercent_with_any_exemption vs Percent_with_any_exemption",span= 3/2, degree =1, 
xlab="K_12_enrollment",ylab="Percent_with_any_exemption",family = c("symmetric","gaussian"),
lpars = list(col="red",lwd=4,lty=5))
#Boxplot
boxplot(Number_complete_for_all_immunizations~Has_kindergarten,ylab="Number of total Immunizations",
        xlab="Students who have and don't have Kindergarten ",main="Boxplot by 
        kindergarten who has all immunizations",data=newdata)
quantile(newdata$Number_complete_for_all_immunizations,probs = c(0,0.25,0.75,1))
# Hypothesis Testing
mean(newdata$Number_with_any_exemption)
t.test(newdata$Number_with_any_exemption,mu=24,alternative = "less",conf.level = 0.95)


#Cleaning the data
#Removing all the Null Values
newdata<-na.omit(mydata)
View(newdata)
#Linear Regression
model1<-lm(Number_with_religious_membership_exemption~Number_with_religious_exemption,
           data=newdata)
model1
summary(model1)
plot(model1)

cor_var<-newdata[c(15:20)]
ggpairs(cor_var,title = "Correlogram with ggpairs")



#Correlation 
cor.test(newdata$Number_exempt_for_polio,newdata$Number_exempt_for_pertussis)
cor(newdata$Number_exempt_for_polio,newdata$Number_exempt_for_pertussis,
    method="pearson")
mydata.cor<-newdata[,c(4:8)]
mydata.cor
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(mydata.cor)

# K means clustering
df_data<-data.frame(newdata$Percent_complete_for_all_immunizations,newdata$
                      Percent_exempt_for_HepatitisB)
df_data
kmeans3<-kmeans(df_data,3)
kmeans3
result<-data.frame(newdata$Percent_complete_for_all_immunizations,newdata$
                     Percent_exempt_for_HepatitisB,kmeans3$cluster)
result
kmeans_plot<-ggplot(result,aes(result$newdata.Percent_complete_for_all_immunizations,result$
              newdata.Percent_exempt_for_HepatitisB,color=result$kmeans3.cluster))+geom_point()
kmeans_plot
# FOR K MEANS CLUSTERING 
#eliminating the columns which are categorical
z<-mydata[,-c(1:3,29:35)]
z
#Normalising 
m<-apply(z,2,mean)
m
s<-apply(z,2,sd)
z<-scale(z,m,s)
z
#K Means clustering 
km<-kmeans(z,5)
km
plot(K_12_enrollment~Percent_complete_for_all_immunizations,dataset,col=km$cluster)


