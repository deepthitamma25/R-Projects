require(ggplot2)
library(ROCR)
library(rpart)				        
library(rattle)				      	
library(rpart.plot)			    	
library(RColorBrewer)			  
library(party)					      
library(partykit)				      
library(caret)					      
library(tree)
library(randomForest)
churn_data = read.csv("Churn.csv",header=TRUE)
head(churn_data)
a1<-sum(churn_data$Churn=='1')
a1
a2<-sum(churn_data$Churn=='0')
a2
mean(churn_data$Churn)

#b
row <- nrow(churn_data)
set.seed(12345)
train_index <- sample(row, 0.6*row, replace=FALSE)
training_model <- churn_data[train_index,]
validation <- churn_data[-train_index,]
churn_validation <- churn_data$Churn[-train_index]
ggplot(training_model,aes(x=RoamMins,y=Churn)) +
        geom_point(aes(color=factor(ContractRenewal)))+
        ggtitle("Plot of RoamMins vs Churn")


#c
glm_model<-glm(Churn ~ AccountWeeks + DataUsage + CustServCalls + DayMins + DayCalls 
             + MonthlyCharge + OverageFee + RoamMins , 
             data=training_model, family=binomial)
summary(glm_model)
glm_model_prob<-predict(glm_model, validation, type ="response")
glm_model_prob [1:10]
valid_row <-nrow(validation)
glm_model_pred<-rep("0",valid_row)
glm_model_pred[glm_model_prob >0.5]="1"
table(glm_model_pred, churn_validation)


#d
glm_full_model<-glm(Churn ~ factor(ContractRenewal) + factor(DataPlan) 
              + AccountWeeks + DataUsage + CustServCalls + DayMins 
              + DayCalls + MonthlyCharge + OverageFee + RoamMins, 
              data=training_model, family=binomial)
summary(glm_full_model)
glm_full_model_prob = predict(glm_full_model, validation ,type ="response")
glm_full_model_pred<-rep("0",valid_row)
glm_full_model_pred[glm_full_model_prob >0.5]="1"
table(glm_full_model_pred, churn_validation)


#f
glm_scores <- prediction(glm_model_prob, churn_validation)
glm_num <- performance(glm_scores, "tpr", "fpr")
plot(glm_num,
     main="ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="red",  lwd = 3)
abline(0,1, lty = 300, col = "blue",  lwd = 3)
grid(col="black")
glm_auc <- performance(glm_scores, "auc")
as.numeric(glm_auc@y.values)  
glm_lift <- performance(glm_scores, measure="lift", x.measure="rpp")
plot(glm_lift,
     main="Lift Chart",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="green", lwd = 3)
abline(1,0,col="blue",  lwd = 3)
grid(col="black")

# for (d)
glm_scores_d <- prediction(glm_full_model_prob, churn_validation)
glm_perf <- performance(glm_scores_d, "tpr", "fpr")

plot(glm_perf,
     main="ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="red",  lwd = 3)
abline(0,1, lty = 300, col = "blue",  lwd = 3)
grid(col="black")
glm_auc_d <- performance(glm_scores_d, "auc")
as.numeric(glm_auc_d@y.values) 
glm_lift_d <- performance(glm_scores_d, measure="lift", x.measure="rpp")
plot(glm_lift_d,
     main="Lift Chart",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="green", lwd = 3)
abline(1,0,col="blue",  lwd = 3)
grid(col="black")




#2
data_web = read.csv("Web Robot.csv",header=TRUE)
head(data_web)
row<-nrow(data_web)
class(data_web$Robot)
xtabs(~Robot,data=data_web)

#b
set.seed(12345)
web_train_index <- sample(row, 0.6*row, replace=FALSE)
training_data <- data_web[web_train_index,]
test_validation = data_web[-web_train_index,]
robo_vali = data_web$Robot[-web_train_index]

robot_tree <- rpart(as.factor(Robot)~., training_data)
summary(robot_tree)
fancyRpartPlot(robot_tree)
robot_tree_pred <- predict(robot_tree, test_validation, type="class")
table(robot_tree_pred, robo_vali)

#d
set.seed(12345)
robo_rand_forest = randomForest(Robot ~ .,mtry=3, data=train_data)
varImpPlot(robo_rand_forest,type='2')
robo_predict <- predict(robo_rand_forest,test_data, type = 'class')
obs_prediction = rep("Not Robot",0.4*nrow(web_data))
obs_prediction[robo_predict>0.5]="Robot"
table(obs_prediction,test_data$Robot)


