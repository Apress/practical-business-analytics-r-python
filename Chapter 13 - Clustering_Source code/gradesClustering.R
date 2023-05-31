#####################################################
#   K-Measn Clustering                              #
#   Library and poackage: cluster()                 #
#   Dataset: Students Grade Assignment              #
#   Data has 5 columns, StudentID, Quiz 1, Quiz 2,  #            #
#   Quiz3,QUiz4 and Quiz5                           #
#   Date:   June 18, 2022                           #
#   Objective:   The K-Means clustering model aims     #
#   to create clusters based on students' performance  #  
#   in the different components of assignments         #
#   and exams - Quiz1, Quiz2, Quiz3, Quiz4 
#   and QUiz5                                       # 
#                                                   #
#####################################################

#### K-Means Clustering
# loading K-Means clustering Lbrary
library(cluster) 

##Step 1: Read the data from file
##Set working directory
setwd("E:/Umesh-MAY2022/Personal-May2022/BA2ndEdition/2ndEdition/Book Chapters/Chapter 13 - Clustering/scripts")
# Read the Training data
grades_df<-read.csv("grades.csv",header=TRUE, sep=",")
##Read the Test data
head(grades_df)


##Checking the data type to make sure if any transformation is required
str(grades_df)

##Removing 'StudentID' column, that is not required for the clustering analysis
grades_df2<-grades_df[-c(1)]
head(grades_df2)

##Scale the data
# Standardize the Data
grades_df2<-scale(grades_df2)
head(grades_df2)
# Computing k-means clustering in R with the kmeans function. 
# We will group the grades data into three clusters (centers = 3). 
# The kmeans function has several input parameters
# adding nstart = 25 will generate 25 initial configurations.
# This approach is often recommended.

km_model<-kmeans(grades_df2, centers=3, nstart=25)

#Model summary
summary(km_model)
# Cluster details of the data points
km_model$cluster
# Centers of each cluster for each variables
km_model$centers

##Plotting cluster
library(factoextra) 

# We plot cluster using fviz_cluster(). 
## If there are more than two dimensions (variables) fviz_cluster 
# will perform principal component analysis (PCA) and plot the data points
# First two of the principal components explain the
# majority of the variance.

fviz_cluster(km_model, data = grades_df2)


###We will find the optimal cluster value k using Elbow method
# Fortunately, the "Elbow method" is supported by 
# a single function (fviz_nbclust):
set.seed(123)
fviz_nbclust(grades_df, kmeans, method = "wss")

##Using "silhoutee" method
fviz_nbclust(df, kmeans, method = "silhouette")


## The results shows 2 or 3 is the most optimal k-value
# We will computer using k=2
km_model_k2<-kmeans(grades_df2, centers=2, nstart=25)
fviz_cluster(km_model_k5, data = grades_df2)
# Cluster details of the data points
km_model_k2$cluster

##### Hierachical clustering
dist_obs_grades<-dist(grades_df2, method="euclidian")
cluster_hier<-hclust(dist_obs_grades,method="average")

##Find the best cluser value using NbClust

install.packages("NbClust")
library(NbClust)
help(NbClust)
optimal_cluster<-NbClust(data=grades_df2, distance="euclidean",
                         min.nc=3, max.nc=15,
                         method="average")

require(factoextra)
fviz_dend(x = cluster_hier,
          rect = TRUE, 
          cex = 0.5, lwd = 0.6,
          k = 5,
          k_colors = c("purple","red", 
          "green3", "blue", "magenta"),
          rect_border = "gray", 
          rect_fill = FALSE)

###
