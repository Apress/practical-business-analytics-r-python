#########################
### Objective: Predictive model using Logistic Regression
### Dataset: Attrtion_data.txt
### Data description: Data has following attributes:
#      Attrition represents whether the employee has exited the organization or is still in the organization (Yes for exit, and No for currently working in the organization).
#	     Yrs_Exp represents the experience of the employee at this time (in years).
#	     Work_Challenging represents whether the work assigned to the employee is challenging.
#	     Work_Envir represents whether the work environment is Excellent or Low.
#      compensation represents whether the compensation is Excellent or Low.
#      Tech_Exper represents whether the employee is technically expert (Excellent or Low).
#   Library: glm()
#   Date: 08/2/2022  
###################

attrition_data<-read.csv("attr_data.txt")
summary(attrition_data)

##glimse of data
head(attrition_data)
tail(attrition_data)

## build the model
attri_logit_model<-glm(Attrition~Yrs_Exp+Work_Challenging+Work_Envir+Compensation+Tech_Exper,
                       data=attrition_data,
                       family =binomial(link="logit"))

##Model 2 
## build the 2nd model without Tech_Expr variable 
## (refer to the book for the explanation)
attri_logit_model_2 <-glm(Attrition~Work_Challenging+Work_Envir+Compensation+Tech_Exper,
                       data=attrition_data,
                       family =binomial(link="logit"))

summary(attri_logit_model_2)

# Check CHi-square
anova(attri_logit_model_2, "PChiSq")

## compare the two models
anova(attri_logit_model_2, attri_logit_model,
      test="Chisq")

##Calculate psedo-R value
pseudo_R_Squared<-1-(attri_logit_model_2$deviance)/(attri_logit_model_2$null.deviance)
pseudo_R_Squared

###model deviance difference
deviance_diff<-attri_logit_model_2$null.deviance - attri_logit_model_2$deviance
deviance_diff
df_data<-nrow(attrition_data) - 1
df_data
df_residual<-attri_logit_model_2$df.residual
df_residual
p_value<-pchisq(deviance_diff, (df_data - df_residual),
                lower.tail=FALSE)
p_value

##Multi collinearity
library(car)
vif(attri_logit_model_2)

# If the ratio of the Residual Deviance of model to
# its residual degrees of freedom is greater than 1 then
# the model suffers from the issue of Overdispersion
# Let us check whether our Logistic Regression Model
# suffers from this issue
overdispl_indicator<-attri_logit_model_2$deviance/attri_logit_model_2$df.residual
overdispl_indicator

#As you can see the value is less than 1. Hence, our model
# does not suffer from the issue of overdispersion

#Split data into train and test
library(caret)
set.seed(1234)
# setting seed ensures the repeatability of the results on different trials
# We are going to partition data into train and test using
# createpartition() function from the caret package
# we use 80% of the data as train and 20% as test
Data_Partition<-createDataPartition(attrition_data$Attrition, p=0.8,list=FALSE)
Training_Data<-attrition_data[Data_Partition, ]
Test_Data<-attrition_data[-Data_Partition, ]
nrow(attrition_data)
nrow(Training_Data)
nrow(Test_Data)
summary(Training_Data)


## Model 3
## Create a logistic regression model using Training_Data set
#  We will not use Yrs_Exp variable as it is not significant.
#  We already explained what variables to consider in our previous discussion

train_logit_model<-glm(Attrition~Work_Challenging+Work_Envir+Compensation+Tech_Exper,
                       data=Training_Data,
                       family =binomial(link="logit"))
summary(train_logit_model)

## Example of prediction

predictor_1<-data.frame(Yrs_Exp=3, Work_Challenging="Yes", Work_Envir="Excellent",
                       Compensation="Excellent", Tech_Exper="Low")

predicted_1<-predict(attri_logit_model_2, newdata=predictor_1, type="response")
predicted_1

##The probability is very low which represents "NO"
#The model has predicted well in this case

    
##Predict Test data
summary(train_logit_model)
predicted<-predict(train_logit_model, newdata=Test_Data, type="response")
predicted
##Set a threshold for anything >0.5 it is 1 and anything less than 0.5, it is 0
# 0 means NO and 1 means YES
table(Test_Data$Attrition, predicted>0.5)

##Plotting ROC
library(ROCR)
prediction_object<-prediction(predicted, Test_Data$Attrition)
prediction_object
# Generate performance measures fro the above prediction values
perf<-performance(prediction_object, measure="tpr",x.measure="fpr")
plot(perf)

###################
library(glmnet)
#converting into a matrix as requred for the input.
x<-model.matrix(Attrition~Work_Challenging+Work_Envir+Compensation+Tech_Exper,
                data = attrition_data)
y<-attrition_data$Attrition
glmnet_fit<-glmnet(x,y,
          family="binomial",
          alpha=1,
          nlambda=100)
summary(glmnet_fit)

##plot 
plot(glmnet_fit,
     var="dev",
     label=TRUE)

##
print(glmnet_fit)

##Predict glmnet() model
predict(glmnet_fit, newx=x[1:4,], 
        type="class",
        s=-.05)

###Cross validation fit
cv.fit<-cv.glmnet(x,y,
                  family="binomial",
                  type.measure="class")
summary(cv.fit)
#Plot cv.fit model output
plot(cv.fit)

## We will read some of the important output of cv.fi
cv.fit$lambda.min
cv.fit$lambda.1se

##Get the coeffiicients of the model
coef(cv.fit, s="lambda.min")

# predict new data
predict(cv.fit, newx=x[1:6,],s="lambda.min", type="class")

