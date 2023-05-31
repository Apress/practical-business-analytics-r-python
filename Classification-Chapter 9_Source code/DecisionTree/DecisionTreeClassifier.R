###############################################################
# Algorithm: Decision Trees                                   # 
# Dataset: AttritionData.csv                                  #
# Objective:                                                  #
#        Company HR department would like to predict          #
#        employee Attrition. It has captured various          #
#        parameters of emloyees and other relavent parameters.#
# Data Description: Data contains 10 independent variables.   #
#  Attrition, YrsExp, WorkChallenging, 	WorkEnvir,	          #
#  Compensation,	TechExper,	maritalstatus,	                #
#  education,	children,	ownhouse,	loan                        #
#  Model: Build a predictive model                            #
#         using Decision Tree algorithm                       #         # 
###############################################################

#Step1: Set working directory
setwd("E:/Umesh-MAY2022/Personal-May2022/BA2ndEdition/2ndEdition/Book Chapters/Chapter 9 - Pred-Classification/Code/DecisionTree")
#step 2: Read data
attrition_df<-read.csv("attrdataDecisionTree.csv")
#Step 3: Data preprocessing and data preparation
head(attrition_df)
str(attrition_df)

##Step 4: Explore data. In this case we will look into how data
# is distributed into different categories.
table(attrition_df$Attrition)
prop.table(table(attrition_df$Attrition))

prop.table(table(attrition_df$WorkChallenging))
prop.table(table(attrition_df$WorkEnvir))
prop.table(table(attrition_df$Compensation))


# Step 5: Divide data into training and test datasets. 
# We will use caret() library package to perfrom this task

library(caret)
set.seed(1234)
data_partition<-createDataPartition(y=attrition_df$Attrition, 
                                    p=0.8, 
                                    list=FALSE)
train<-attrition_df[data_partition,]
test<-attrition_df[-data_partition,]

nrow(attrition_df)
nrow(train)
nrow(test)
head(train)

#Step 6: Create a Decision Tree model using training data
# We will use rpart() library
#rpart: Recursive Partitioning and Regression Trees
#By default, rpart uses gini impurity 
# to select splits when performing classification
library(rpart)
equation = Attrition~YrsExp+WorkChallenging+WorkEnvir+Compensation+TechExper+maritalstatus+education+children+ownhouse+loan
attr_tree<-rpart(formula = equation, 
                 data = train,
                 method = 'class',
                minsplit=2,
                cp=-1,
                 minbucket = 2,
                parms = list(split = 'gini'))

summary(attr_tree)
attr_tree$variable.importance
##Plotting TREE
library(rattle)
library(rpart.plot)
rpart.plot(attr_tree)

# You can also plot the tree using the following library
# library(RColorBrewer)
# plot mytree
# fancyRpartPlot(attr_tree, caption = NULL)

################################################
##Print the variable of importance:
# From the rpart documentation, 
# "An overall measure of variable importance 
# is the sum of the goodness of split measures 
# for each split for which it was 
# the primary variable."
#################################################


#Step 7: Predict test data
tree_pred<-predict(attr_tree,newdata=test,
                   type = 'class',
                   prob = TRUE)
#Step 8: Meausure the performance of your model
library(caret)
table(tree_pred, test$Attrition)
confusionMatrix(as.factor(tree_pred), test$Attrition)

## When rpart grows a tree it performs 10-fold cross validation 
# on the data. 
# Use printcp() to see the cross validation results.

printcp(attr_tree)

## The rel error of each iteration of the tree 
## is the fraction of mislabeled elements in the 
## iteration relative to the fraction of mislabeled 
## elements in the root.

##Now, we will prune the tree
pruned_tree<-prune(attr_tree,cp=0.05)
rpart.plot(pruned_tree)

##Predict using pruned tree
pred_prun<-predict(newdata=test, pruned_tree, type='class')

##Measure performance
confusionMatrix(as.factor(pred_prun), test$Attrition)

##ROC Curve
library("ROCR")
pred_prob <- predict(pruned_tree,newdata=test, type = "prob")[, 2] 

pred2_prob = prediction(pred_prob, test$Attrition) 
plot(performance(pred2_prob, "tpr", "fpr"),col='red',
     main="ROC: Predicting Attrition")
auc = performance(pred2_prob, 'auc')
slot(auc, 'y.values')




