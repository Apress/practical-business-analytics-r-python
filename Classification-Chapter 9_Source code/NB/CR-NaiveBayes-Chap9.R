###############################################################
# Algorithm: Naive Bayes Classification                       # 
# Dataset: data-CreditApproval.csv                            #
# Description:                                                #
#  Bank woud like to build an AI model to                     # 
#  predict applicant's loan approval decision                 #
#  The data consists of target variable-Loan approval,        #
#  and three independent variables - Age, CreditRatings,      #
#  and Income                                                 #
#  Model: The objective is to build a predictive model        #
#  using Naive Bayes algorithm                                # 
################################################################

#Step1: Set working directory
setwd("E:/Umesh-MAY2022/Personal-May2022/BA2ndEdition/2ndEdition/Book Chapters/Chapter 9 - Pred-Classification/Code/NB")
#step 2: Read data
data_df<-read.csv("data-CreditApproval.csv")
#Step 3: Data preprocessing
head(data_df)
str(data_df)

# Check data types, missing values, etc. conduct any transformation if necessary
is.na(data_df)
sum(is.na(data_df))
sum(is.na(data_df$Income))
sum(duplicated(data_df))

##Step 4: Explore data. In this case we will look into how data
# is distributed into different categories.
levels(data_df$Approval)
table(data_df$Approval)
prop.table(table(data_df$Approval))

table(data_df$Age)
prop.table(table(data_df$Age))

table(data_df$Income)
prop.table(table(data_df$Income))

table(data_df$CreditRating)
prop.table(table(data_df$CreditRating))

##You can also plot the same. 
#In this case it gives similar information.


# Step 5: Divide data into training and test datasets. 
# We will use caret() library package to perfrom this task

library(caret)
set.seed(1234)
data_partition<-createDataPartition(y=data_df$Approval, 
                                    p=0.8, 
                                    list=FALSE)
train<-data_df[data_partition,]
test<-data_df[-data_partition,]

nrow(data_df)
nrow(train)
nrow(test)
head(train)

#Step 6: Create a Naive Bayes model using training data
# We will use 'e107' library
library(e1071)
nb_model <- naiveBayes(Approval~Income+CreditRating+Age,
                       data=train)
#Step 7: Predict test data
nb_model
nb_pred<-predict(nb_model,test)  
nb_pred

#Step 8: Meausure the performance of your model
library(caret)
table(nb_pred, test$Approval)
confusionMatrix(as.factor(nb_pred), test$Approval)

###ROC curve and Area under the curve
library(ROSE)
roc.curve(test$Approval, nb_pred)

