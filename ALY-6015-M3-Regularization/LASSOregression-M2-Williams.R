# #G. Holt Williams M3 - LASSO Regression in R Exercises
#link
# https://www.r-exercises.com/2017/06/12/lasso-regression-in-r-exercises/?utm_source=rss&utm_medium=rss&utm_campaign=lasso-regression-in-r-exercises&__cf_chl_jschl_tk__=495283dd9736cd00879db2e590b2631171026e14-1589977021-0-AaMiNRTwcUSUsmb415RNnY6uzgxrwztuJ_wYb5YnpqQR0rrXRfMhUb5qUNTgjxDFg9A9ySp-IM3SRqChco_uF43v7KMB7AuItzvzUVVVdzH3nX-IIycXr_FTZe9ChhrNg1h6YArjreyNpCBJ8Ix_85lcZy3DpPxK3IPVGQn269qNusWxItMj-PC2ckou1FGd-DfqDzVugG_1nMUo2HGmksg1dUtYVTZ3bYePW8D802d2MPrd6n_qNPQCQCnERy82zOwomgFr4mK9EBYeE3JgKLjb5xJMEbajbmO2WsIxaCybyhfjL4VWDu-Lo6KZnHVDvvB3lcdcw0q71PcFYmAtgeRBK6BcR7g6z3DUf-pklQvhanbBVCid3tGckYdf9-X2s36gFD8aowWSN4KrpmKcJce3_5JhDYlJMzhMNp08NB6TFtvN6ibe5HNbWID3JET6xw

library(tidyverse)
# Exercise 1
# Load the lars package and the diabetes dataset 
# (Efron, Hastie, Johnstone and Tibshirani (2003) “Least Angle Regression” Annals of Statistics). 
# This has patient level data on the progression of diabetes. 
# Next, load the glmnet package that will be used to implement LASSO.
library(lars)
library(glmnet)
data("diabetes")
attach(diabetes)

# Exercise 2
# The dataset has three matrices x, x2 and y. 
# While x has a smaller set of independent variables, x2 contains the full set with quadratic and interaction terms. 
# y is the dependent variable which is a quantitative measure of the progression of diabetes.
# It is a good idea to visually inspect the relationship of each of the predictors with the 
# dependent variable. Generate separate scatterplots with the line of best fit for all the predictors in x with y on the vertical axis. 
# Use a loop to automate the process.

#Do I need to include 1 in the first loop here? 
#The data in 1 and 3 look to be the same with 3 having additional data


for (i in 1:10){
  plot(x[,i],y, xlab =colnames(x)[i])
  abline(lm(y~x[,i]))
}




# Exercise 3
# Regress y on the predictors in x using OLS. We will use this result as benchmark for comparison.

bench<-lm(y ~ x, data=diabetes)
summary(bench)


# Exercise 4
# Use the glmnet function to plot the path of each of x's variable coefficients against the L1 norm of the beta vector. 
# This graph indicates at which stage each coefficient shrinks to zero.
glm1<-glmnet(x, y)
summary(glm1)
plot(glm1)



# Exercise 5
# Use the cv.glmnet function to get the cross validation curve and the value of lambda that minimizes the mean cross validation error.

cvglm1<-cv.glmnet(x, y, alpha=1, nlambda=1000)
summary(cvglm1)
plot(cvglm1)
cvglm1$lambda.min
# Exercise 6
# Using the minimum value of lambda from the previous exercise, get the estimated beta matrix. 
# Note that some coefficients have been shrunk to zero. 
# This indicates which predictors are important in explaining the variation in y.
fit<- glmnet( x=x, y=y, alpha=1, lambda=cvglm1$lambda.min)
summary(fit)
fit$beta

# Exercise 7
# To get a more parsimonious model we can use a higher value of lambda that is within one standard error of the minimum. 
# Use this value of lambda to get the beta coefficients. 
# Note that more coefficients are now shrunk to zero.
cvglm1$lambda.1se
fit<- glmnet( x=x, y=y, alpha=1, lambda=cvglm1$lambda.1se)
summary(fit)
fit$beta

# Exercise 8
# As mentioned earlier, x2 contains a wider variety of predictors. Using OLS, regress y on x2 and evaluate results.
second<-lm(y ~ x2, data=diabetes)
summary(second)

# Exercise 9
# Repeat exercise-4 for the new model.
glm2<-glmnet(x2, y)
summary(glm2)
plot(glm2)

# Exercise 10
# Repeat exercises 5 and 6 for the new model and see which coefficients are shrunk to zero. 
# This is an effective way to narrow down on important predictors when there are many candidates.

cvglm2<-cv.glmnet(x2, y, alpha=1, nlambda=1000)
summary(cvglm2)
plot(cvglm2)
cvglm2$lambda.min

fit2<- glmnet( x=x2, y=y, alpha=1, lambda=cvglm2$lambda.min)
summary(fit2)
fit2$beta
