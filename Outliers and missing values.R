#Outliers
getwd()
setwd("D:/Edwisior/Level 2")

df = mtcars
dim(df)
ds = df$qsec

boxplot(ds,data = ds)
boxplot.stats(ds)

ds = ds[!ds%in%boxplot.stats(ds)$out]
df[1,2]

 #Missing values
 data = iris
 #data[3,4] = 0.2
 
data[3,4] = NA
 
data[3,4]

apply(data,2,function(x){sum(is.na(x))})

mean(data$Petal.Width,na.rm = T)
#mean = 1.02

median(data$Petal.Width,na.rm = T)
#median = 1.3
library(DMwR)
data1 = knnImputation(data)
data1[3,4]
#0.198
data$Petal.Width[is.na(data$Petal.Width)]
