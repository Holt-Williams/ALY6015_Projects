## Scripts for Data Mining Project


## Libraries
library(glmnet)
library(tidyverse)
library(broom)
library(psych)
library(ggthemes)
library(gridExtra)
library(printr)
library(tidymodels)
library(dplyr)
library(forcats)
# Find data Set
red<-read.csv("winequality-red.csv", sep=";")
white<-read.csv("winequality-white.csv", sep=";")

## Add type
red$type<-"red"
white$type<-"white"

## Make Factors, Just combine then do this. The type can be filterred instead of having seperate objects
# red$qualityFact<-as.factor(red$quality)
# red$qualityFact<-fct_collapse(red$qualityFact, Bad = c("3", "4"), Med = c("5","6","7"), Great=c("8","9"))
# 
# white$qualityFact<-as.factor(white$quality)
# white$qualityFact<-fct_collapse(white$qualityFact, Bad = c("3", "4"), Med = c("5","6","7"), Great=c("8","9"))

#Combine
wine<-rbind(red,white)
wine$type<-as.factor(wine$type)
table(wine$quality)

## Make Quality Factor, Moved to before combinning 
wine$qualityFact<-as.factor(wine$quality)
wine$qualityFact<-fct_collapse(wine$qualityFact, Bad = c("3", "4"), Med = c("5","6","7"), Great=c("8","9"))
table(wine$qualityFact)


# Complete the following steps and write a report to record your work, results and analysis. 
# 
# Install and load the *factoextra and **NbClust packages. 
# Select an appropriate data set in R or the MASS library and use the sample(), ctree() and predict() functions to build a decision tree and plot it. 
# Determine the appropriate number of clusters and produce a k-means cluster. Explain your findings. 
# Produce a density-based cluster and explain your findings. 
# *The factoextra package is used to determine the optimal number clusters for a given clustering methods and for data visualization 
# 
# **The NcClust package provides 30 indices for determining the relevant number of clusters and the best clustering scheme from the different results obtained by varying all combinations of number of clusters, distance measures, and clustering methods. It can simultaneously compute all the indices and determine the number of clusters in a single function call. 


## Follow Instructions
library(factoextra)
library(NbClust)
library(party)



# Ctree / Classification / Type
set.seed(1234)
ind <- sample(2, nrow(wine), replace=T, prob=c(0.7, 0.3))
wine.train <- wine[ind==1,-c(12,14)]
wine.test <- wine[ind==2, -c(12,14)]

wine.form<- type ~ .
wine.ctree <- ctree(wine.form,data=wine.train)
plot(wine.ctree)

pred<- predict(wine.ctree, newdata=wine.test)
table(pred, wine.test$type)


# Ctree / Classification / quality
###  
set.seed(1234)
ind <- sample(2, nrow(wine), replace=T, prob=c(0.7, 0.3))
wine.trainqf <- wine[ind==1,-c(12,13)]
wine.testqf <- wine[ind==2, -c(12,13)]
wine.formqf<- qualityFact ~ .
wine.ctreeqf <- ctree(wine.formqf,data=wine.trainqf)
pred<- predict(wine.ctreeqf, newdata=wine.testqf)
table(pred, wine.testqf$qualityFact)

## Tidymodels version, with stratification to ensure equal quality fact across test and train
set.seed(1234)
library(tidymodels)
data_split <- initial_split(wine[-c(12,13)], strat=qualityFact)
train_data <- training(data_split)
test_data  <- testing(data_split)
wine.formqf<- qualityFact ~ .
wine.ctreeqf <- ctree(wine.formqf,data=train_data)
pred<- predict(wine.ctreeqf, newdata=test_data)
table(pred, test_data$qualityFact)


## Clustering

wine2<- wine[,c(1:11,13)]

wine2$type<- NULL

wine.kmeans<- kmeans(wine2,2)
table(wine$type, wine.kmeans$cluster)


## Kmeans QualityFact
wine_qf<- wine[,c(1:11,14)]
wine_qf$qualityFact<- NULL

wine.kmeansqf<- kmeans(wine_qf,3)
table(wine$qualityFact, wine.kmeansqf$cluster)


## Kmeans Quality
wine_q<- wine[,c(1:12)]
wine_q$quality<- NULL

wine.kmeansq<- kmeans(wine_q,7)
table(wine$quality, wine.kmeansq$cluster)







## Density Based Clustering
# library(fpc)
# library(factoextra)
library(dbscan)
# Group Type
wine3<- wine[,c(1:12)]
ds<- dbscan(wine3, eps=20, MinPts = 800)
table(ds$cluster, wine$type)
fviz_cluster(ds, data = wine3, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

dbscan::kNNdistplot(wine3, k =  100)
abline(h = 30, lty = 2)



# Group Quality
wine_Qualfact<- wine[,c(1:11)]## Remove quality and type
ds2<- dbscan(wine_Qualfact, eps=9, MinPts = 60)
table(ds2$cluster, wine$qualityFact)
plotcluster(wine_Qualfact, ds2$cluster)
fviz_cluster(ds2, data = wine_Qualfact, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

dbscan::kNNdistplot(wine_Qualfact, k =  5)
abline(h = 10, lty = 2)



#### Principal Component Analyusis

## actual PCA
set.seed(1234)
winePCA<-princomp(wine[c(1:11)])
summary(winePCA)
winePCA$loadings
df<-wine
df$pca1<-winePCA$scores[,1]
df$pca2<-winePCA$scores[,2]
df$pca3<-winePCA$scores[,3]
## PCA Decision Tree for type

library(factoextra)
fviz_contrib(winePCA,choice="var", axes=1:2)
dim_desc(winePCA)



ind <- sample(2, nrow(df), replace=T, prob=c(0.7, 0.3))
wine.train_pca <- df[ind==1,-c(12,14)]
wine.test_pca <- df[ind==2, -c(12,14)]

wine.form_pca<- type ~ pca1+pca2+pca3
wine.ctree_pca <- ctree(wine.form_pca,data=wine.train_pca)
plot(wine.ctree_pca)

pred_pca<- predict(wine.ctree_pca, newdata=wine.test_pca)
table(pred_pca, wine.test_pca$type)








## EDA Tests, make sure "MASS" is not loaded
wine%>%describe(quant=c(.25,.75),omit=TRUE)%>%select(n, sd,mean, median, min, max, Q0.25, Q0.75)%>%round(digits=2)
table(wine$type)
table(wine$qualityFact)
table(wine$type,wine$qualityFact)

describe(wine)

## Graphs
wine%>%ggplot()+
  geom_bar(aes(x=qualityFact, fill=type),color="black", position=position_dodge())+
  theme_tufte()+
  scale_fill_manual(values = c("red"="#7D0444","white"="#FBE79E"))+
  xlab("Quality Factor")+
  ylab(NULL)

library(ggcorrplot)
ggcorrplot(cor(wine[1:12]), ggtheme=theme_tufte(),colors = c("#FBE79E", "white", "#7D0444"))





