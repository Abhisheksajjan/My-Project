#Load the data in R
trainingset = read.csv("train.csv",header = T)

#Objective: Linear Regression is used to obtain the linear relationship 
#between the dependent variable and independent variable so as to predict the value of target variable(dependent)
#when independent(predictor) variables are known

dim(trainingset)
View(trainingset)
 
# The taining set has 700 rows and 2 columns.X is the independent variable whereas Y is the dependent variable

#check for Missing values:

apply(trainingset,2,function(x){sum(is.na(x))})

#Y has 1 missing value
#Create a new data with the present data and delete or make some values as NA
#Choose the best method and see which suits the best
#New data
Missing_values = trainingset

View(Missing_values)

Missing_values[3,2] #17.21866 We know the value,now manually update it to NA
Missing_values[3,2] = NA

#Statistical Method
Missing_values$y[is.na(Missing_values$y)] = mean(Missing_values$y,na.rm = T)
Missing_values[3,2] # 49.98675

      
Missing_values$y[is.na(Missing_values$y)] = median(Missing_values$y,na.rm = T)
Missing_values[3,2] # 49.21


library(DMwR)
Missing_values = knnImputation(Missing_values)
# Getting an error hence creating dummycolumn

z = c(1:700)
Missing_values = cbind(Missing_values,z)


library(DMwR)
Missing_values = knnImputation(Missing_values)
Missing_values[3,2] # 19.201163
# From above observation Knn Imputation is the best method
#rm Missing values 
#Best Method --> Knn Imputation
 rm(Missing_values)
 
trainingset$y[is.na(trainingset$y)]

library(DMwR)

trainingset = knnImputation(trainingset)
# create a dummy column
z = c(1:700)
trainingset = cbind(trainingset,z)

library(DMwR)
trainingset = knnImputation(trainingset)
# Remove the dummy column
trainingset = trainingset[,-3]

# OUTLIERS
# Boxplot for X
boxplot(trainingset$x, main='Y', sub=paste('Outliers: ', boxplot.stats(trainingset$x)$out))
which (trainingset$x == 3530.15736917)
trainingset = trainingset[-(which (trainingset$x == 3530.15736917)),] # deleted the observation.

# Boxplot for Y
boxplot(trainingset$y,main = 'Y',sub = paste('Outliers :',boxplot.stats(trainingset$y)$out))
#No outliers


par(mfrow = c(1,2))
boxplot(trainingset$x, main='X', sub=paste('Outliers: ', boxplot.stats(trainingset$x)$out))
boxplot(trainingset$y,main = 'Y',sub = paste('Outliers :',boxplot.stats(trainingset$y)$out))
# Now Both boxplots shows no outliers and distribution is not skewed.

#Feature Extraction -- To select the relevant variable
library(corrplot)
 
corrplot(cor(trainingset),order ="hclust", main = 'Correlation')
 
cor(trainingset$x,trainingset$y) # 0.999 --> postively correlated

#Hypothesis--Null hyothesis:- A small change in x variable will not induce any change 
#in the dependent variable i.e. H0: b1=0
#Alternate Hyothesis H1 ,b1 is not equal to 0

#Linear Regression model
lm_model = lm(y ~ .,data = trainingset)
summary(lm_model)
#p value is less than 0.05 so we reject the null hypothesis
#R2 --> It is the proprtion of variance in dependent variable which can be explained by
#independent variable

