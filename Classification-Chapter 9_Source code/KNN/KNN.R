###########################################
#   KNN Classifier                        #
#   Library and poackage: class()         #
#   Dataset: Loan approval;               #
#   Data has 3 columns, Age,              #
#   Purchase Amount and APproval          #
#   Date:   May 24, 2022                  #
# The Objective of this KNN model is to   #
# classify and predict Loan Approval.     #
# Approval is the target(response) variable# 
# and Age, PurchaseAmount are independent  #
# variable                                 #
#                                          #
###########################################

#### K-NN Classifier
library(class) # loading KNN classifier Lbrary

##Step 1: Read the data from file
##Set working directory
setwd("E:/Umesh-MAY2022/Personal-May2022/BA2ndEdition/2ndEdition/Book Chapters/Chapter 9 - Pred-Classification/Code")
# Read the Training data
knndata<-read.csv("knn-data.csv",header=TRUE, sep=",")
##Read the Test data
knntest<-read.csv("knntestdata.csv",header=TRUE,sep=",")
head(knndata)


##Checking the data type to make sure if any transformation is required
##APproval variable is a categorical and R automatically reads it as a "factor"
## In R Factor is categorical variable
str(knndata)
str(knntest)

## Read KNN library documentation to see the input parameters required using help(knn).
## For this library function, input training data should not have the response variable
# Hence separate the response variable
train_data=knndata[,-3]
# Seprate the response variable from the test data
test_data = knntest[,-3]

## Number of categories/classes
cls<-factor(knndata[,3])
cls  ## There are Two classes (Yes and NO)


## To calculate the test accuracy of the model, we need to know the class of the test data
##Separating test data categories/class
actual<-factor(knntest[,3])
actual

## Creating K-NN Model using knn() function
##The Objective of this KNN model is to classify and predict Loan Approval (Approval)
##Approval is the target(response) variable and Age, PurchaseAmount are independent variable
library(class)
knnmod<-knn(train=train_data,test=test_data,cl=cls,k=3,
            prob=FALSE)

##Predict the new test data
predicted<-knnmod

## Calculate the classification error
check_error = function(actual, predicted) {
  mean(actual != predicted)
}

check_error(actual,predicted)


# Caclulate accuracy and other parameters using caret functions
library(e1071)
report<-confusionMatrix (actual, predicted)
report

##How do we choose  k? 
#east classification error


##Since Age and Purchase amount are different scales, KNN performacne may be limited
##Hence I will transform both variables to same scale using "scale()" function in R

#Try different values of k
ks = 1:10 ##Try k values from 1 to 10
##Storing errors in an array
store_error = rep(x=0, times = length(ks))

for (i in seq_along(ks)) {
  predicted = knn(train = train_data,
             test = test_data,
              cl = cls,
              k = ks[i])
store_error[i] = check_error(actual, predicted)
}

##Plot Error Vs K-values graph

plot(store_error, 
     type='b', 
     col = "blue", 
     cex = 1, pch = 20,
     xlab = "K - values",
     ylab = "classification error",
     main = "Error rate vs K-Values")

