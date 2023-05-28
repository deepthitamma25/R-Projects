mydata<-read.csv("AirfaresData.csv")
head(mydata)

# PART A
# Seperating the numerical variables from the dataset for Correlation
numerical_variables = c("COUPON","NEW","HI","S_INCOME","E_INCOME","S_POP","E_POP","DISTANCE","PAX","FARE")
numerical_data = mydata[numerical_variables]
# Correlation Matrix
correlation=cor(numerical_data)
library(ggcorrplot)
corrplot(correlation, method = "number")
corrplot(correlation, method = "circle")
plot(numerical_data,main="Scatter Plots b/w Numerical Predictors")

# PART B
summaryVacation <-aggregate(FARE~VACATION, data=mydata,FUN=mean)
summaryVacation
summary_SouthWest <-aggregate(FARE~SW, data=mydata,FUN=mean)
summary_SouthWest
summary_SLOT <-aggregate(FARE~SLOT, data=mydata,FUN=mean)
summary_SLOT
summary_GATE <-aggregate(FARE~GATE, data=mydata,FUN=mean)
summary_GATE
cbind(summaryVacation,summary_SouthWest,summary_SLOT,summary_GATE)


#PART C
set.seed(12345)
train_index <- sample(1:nrow(mydata), size=0.6*nrow(mydata)) 
training_set <- mydata[train_index,]
validation <- mydata[-train_index,]
#Fitting the model
fit_train_model<-lm(FARE ~ DISTANCE + SW, data=mydata, subset=train_index)
#Summary of the Model
a<-summary(fit_train_model)
a
a$r.squared
a$adj.r.squared
plot(fit_train_model)
# To show only the first plot
plot(fit_train_model, which=1)                 
AIC(fit_train_model)
BIC(fit_train_model)
# computing the value MSE of validation set
mean((mydata$FARE-predict(fit_train_model, mydata))[-train_index]^2)


#PART D
#Backward Selection to build a linear regression Model
data<-mydata[,5:18]
model = lm(FARE~., data=data, subset=train_index)
summary(model)
backward_selection<-step(model, direction='backward')
b<-summary(backward_selection)
b$r.squared
coefficients(backward_selection)
BIC(backward_selection)
# Computing the Mean of the MSE Validation Set
mean((data$FARE-predict(backward_selection, data))[-train_index]^2)






