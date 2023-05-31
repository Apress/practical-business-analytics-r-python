################################################################# 
## Objective: In this exercise we will demostrate some the
## basics of data Data Mungling, Fix variable names,
## Create new variables, Deal with missing data,
## transfroms of variable types (coercing), 
## Our experience is 90% of your effort will often be spent
## performing these basic operations on data before
## creating models.
## We will demosntarte some of the common R functions
## such as apply(),sapply(), split() and ggplot()
## Writing own functions
## Use for and IFELSE statements
##  Dataset: SummerOlympicMedalsList.csv
##  Dataset contains the list of medals won by different coutries during 1896-2008
### Library oackage: dplyr()
#############################################

##Read data as a R datframe
medal_df<-read.csv("Summer_Olympic_medallists_1896-2008.csv")
head(medal_df)
tail(medal_df,8)
##Check the data size (number of rows and columns)
nrow(medal_df)
ncol(medal_df)

##Selecting Random N samples (90%) from data
library(dplyr)
sample_data<-sample_frac(medal_df, 0.90)
sample_data<-as.data.frame(sample_data)
nrow(sample_data)

head(sample_data)
tail(sample_data)

##Check the data type of each column and transform to approproiate data type
str(sample_data)

sample_data$Silvers<-as.integer(sample_data$Silvers)
str(sample_data)


###Selecting set of variables only
head(sample_data)
X3 = select(sample_data, Total, Golds)
head(X3)

X4 = select(sample_data, -NOC.CODE, - Total)
head(X4)

##Renaming a variable
X5 = rename(sample_data, GoldMedal=Golds)
colnames(X5)


#Get the column name of selected columns
colnames(sample_data)[6]
colnames(sample_data)[3]

######
###gsub function ####
testName<-"This_is_a_test"
testName
gsub("_","",testName)


## base R alternatives to dplyr::filter()
## Filter data with a certain 'string' name
head(sample_data[sample_data$NOC.CODE == 'PRK', ])
## get data with certain 'codition'
head(subset(sample_data, Golds > 10 ))

##Check data types and data informaition
str(sample_data)

## Using dplyr::filter()
#filter( ) Function using 'dplyr'
copying_to_anothervariable<-filter(sample_data, Silvers > 15)
head(copying_to_anothervariable)

##Check is data has any NAs
sum(is.na(sample_data))

#Using apply() function to fnd

# percentage of missing values in each column

apply(sample_data, 2, function(col)sum(is.na(col))/length(col))

#Using apply() function to identifying 
# the rows with NAs
rownames(sample_data)[apply(sample_data, 2, anyNA)]



##Impute the missing value with mean value
## Check for NAs in 'Total' column and replace with 'mean'.

for(i in 1:ncol(sample_data))
  {
  sample_data[is.na(sample_data[,i]), i] <- mean(sample_data[,i], na.rm = TRUE)
}

## Imputing NA with mean() value on a single column

sample_data$Total[is.na(sample_data$Total)] <- mean(sample_data$Total, na.rm = TRUE)
head(sample_data)


#remove all observations with NAs
sample_data %>% na.omit()


####
#duplicated() - remove duplicates
head(sample_data[!duplicated(sample_data$Golds), ])


##Remove duplicate rows using distinct()
X1 = distinct(sample_data)
count(X1)


##Remove duplicates only on one column
X2 = distinct(sample_data, Country)
count(X2)

##Column names to Uppercase
#names(sample_data)<-toupper(names(sample_data))
#head(sample_data)
colnames(medal_df)

####Lapply()
lapply(medal_df['Golds'],mean, na.rm = TRUE)
lapply(select(medal_df, -Country,-NOC.CODE,-Silvers),mean, na.rm = TRUE)


##SAPPLY()
sapply(medal_df['Golds'],mean, na.rm = TRUE)
sapply(select(medal_df, -Country,-NOC.CODE,-Silvers),mean, na.rm = TRUE)

#Split() dataset into 5 groups based on "Total" medals 
c2 <- split(medal_df, medal_df$Total)
head(c2)

##Write function to select medals greater than "abc"

myfunc_count<- function(df, number)
{
  head(df[df$Golds > number, ])
  
}

myfunc_count(medal_df, 25)



##Plots using ggplot() library function

#### find patterns, to suggest modeling stratety
library(ggplot2)
ggplot(data=medal_df) + 
  geom_col(aes(x=NOC.CODE, y=Total), colour='blue')

ggplot(data=medal_df) + 
  geom_histogram(aes(x=Total), fill='green')


ggplot(data=medal_df) +
  geom_boxplot(aes(y=Bronzes), 
               fill = 'orange')


## MAPS - very basics
library(maps)
map("world")
lat<-runif(40,-180,180);
lon<-runif(40,-90,90)
points(lat,lon,col="red",pch=19)


