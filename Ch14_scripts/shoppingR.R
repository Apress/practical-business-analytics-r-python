######################################################
# Objective: To find customer buying patterns from the 
#           retail transactional data and deriving 
#           association rules
# Algorithm: Apriori Assoicaiton rule (Unsupervised technique)
# Dataset: shoping.csv
# Data Description:Data set contains 1000 transactional records.
#                  Each customer transaction  has transaction ID. 
#                  Each customer has purchased different items. 
#                  There are 14 different items. 
#                  1 means, customer purchased the irem, 
#                  0 means customer did not purchase items.
#                  The objective is to find the association rules
#                  from this transactional database.
# Library package: arules() - apriori algorithm library package
#                 dplyr(), tidyr(0) for data preprocessong
#############

library("arules")
library("arulesViz")
library(dplyr)
library(tidyr)
library(tidyverse)

#setwd("D:\umesh\shoppingcsv.csv")
marys <- read.csv("marys.csv")
head(marys)

#####################
## Data Pre-processing

## Check data types of each variable and convert to appripriate 
#  data types
##
str(marys)
sum(is.na(marys))

# apriori() function accepts logical values and hence convert data to Logical
marys_1 <- marys %>% mutate_if(is.numeric,as.logical)
marys_2<-subset(marys_1, select = -c(Trans..Id))
#str(marys_2)
head(marys_2)

#Find frequent itemsets and association rules by applying apripori() algorithm 
#mby setting support and confidence limits
rules<-apriori(marys_2,
               parameter = list(minlen=3, support=0.5, conf=0.7))

#Inspect the top 10 rules
rules.sorted <- sort(rules, by="lift")
inspect(head(rules.sorted, n=10, by="lift"))




