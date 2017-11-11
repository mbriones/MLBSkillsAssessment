
library(ggplot2)
library(dplyr)
library(corrplot)

#Read dataset 1
Data1 = read.csv("ds1.csv", header = TRUE)

#How are the data distributed? 
par(mfrow=c(2,4))
hist(Data1$x1, main="Hist of x1")
hist(Data1$x2, main="Hist of x2")
hist(Data1$x3, main="Hist of x3")
hist(Data1$x5, main="Hist of x5")
hist(Data1$x6, main="Hist of x6")
hist(Data1$ya, main="Hist of ya")
hist(Data1$yb, main="Hist of yb")
hist(Data1$yc, main="Hist of yc")

#What are some of the summary statistics?
summary(Data1)

#Are there any missing values?
length(which(is.na(Data1)))

#Find correlations amongst all variables
Data1.num = select(Data1, x1, x2, x3, x5, x6, ya, yb, yc)
cor(Data1.num)

#Find the strongest connections between the variables
corrplot(cor(Data1.num), method = "ellipse")

#Predicting yb
ybmodel = lm(yb ~ x1, data = Data1)
summary(ybmodel)

ybmodel2 = lm(yb ~ x1 + x3, data = Data1)
summary(ybmodel2)

#Predicting ya
yamodel = lm(ya ~ x2, data = Data1)
summary(yamodel)

yamodel2 = lm(ya ~ x1 + x2, data = Data1)
summary(yamodel2)

yamodel3 = lm(ya ~ x1 + x2 + x3, data = Data1)
summary(yamodel3)

#Predicting yc (No good predictor, save for last)


anova(yamodel, yamodel2, yamodel3)
anova(ybmodel, ybmodel2)

#Import Dataset 2
Data2 = read.csv("ds2.csv", header = TRUE)

length(which(is.na(Data2)) == TRUE)

summary(Data2)

#Histograms of data
par(mfrow=c(2,5))
hist(Data2$X1, main = "Hist X1")
hist(Data2$X2, main = "Hist X2")
hist(Data2$X3, main = "Hist X3")
hist(Data2$X4, main = "Hist X4")
hist(Data2$X5, main = "Hist X5")
hist(Data2$X6, main = "Hist X6")
hist(Data2$X7, main = "Hist X7")
hist(Data2$X8, main = "Hist X8")
hist(Data2$X9, main = "Hist X9")
hist(Data2$X10, main = "Hist X10")

#Data Cluster
Data2data = select(Data2, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)
Data2Cluster = hclust(dist(Data2data))
plot(Data2Cluster, abline(h=70))

Data2ClusterCut = cutree(Data2Cluster, 3)
table(Data2ClusterCut)

# vary parameters for most readable graph
library(cluster) 
clusplot(Data2data, Data2ClusterCut, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(Data2data, Data2ClusterCut)