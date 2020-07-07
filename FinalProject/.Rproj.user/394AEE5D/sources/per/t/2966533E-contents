#### Lasso Modelling

## cv.glm for for lambda
library(glmnet)
library(gridExtra)
library(tidymodels)



set.seed(1234)
cvglm1<-cv.glmnet(data.matrix(df[c(1:84)]),as.matrix(df$`_CARAVAN_Number_of_mobile_home_policies_0_-_1`),family = "binomial", nlambda=1000, alpha=1)
plot(cvglm1)

model.matrix(~. -Count,data=long)

Casual<-long[c(7,9:16)]%>%filter(Type=="Casual")
Casual<-model.matrix(~. -Count, Casual)
Casual_split<-initial_split(Casual, prop=3/4)
Casual_train<-training(Casual_split)
Casual_test<-testing(Casual_split)

Registered<-long[c(7,9:16)]%>%filter(Type=="Registered")
long_split<-initial_split(long, prop=3/4,strata = type)
train<-training(long_split)
test<-testing(long_split)

Total<-long[c(7,9:16)]%>%filter(Type=="Total")
long_split<-initial_split(long, prop=3/4,strata = type)
train<-training(long_split)
test<-testing(long_split)



cvglm_casual<-cv.glmnet(data.matrix(Casual_train[1:8]),as.matrix(Casual_train[9]), nlambda=1000, alpha=1)
plot(cvglm_casual, sub="Casual", xvar="lambda")
cvglm_Registered<-cv.glmnet(data.matrix(Registered[1:8]),as.matrix(Registered[9]), nlambda=1000, alpha=1)
plot(cvglm_Registered, sub="Registered")
cvglm_Total<-cv.glmnet(data.matrix(Total[1:8]),as.matrix(Total[9]), nlambda=1000, alpha=1)
plot(cvglm_Total, sub="Total")


lamb<-tidy(c(cvglm_casual$lambda.min,cvglm_Registered$lambda.min,cvglm_Total$lambda.min))
lamb

# Models
casual_fit<-glmnet(data.matrix(Casual_train[1:8]),as.matrix(Casual_train[9]), lambda=cvglm_casual$lambda.1se, alpha=1)
str(casual_fit$beta)
predict(cvglm_casual, newx=Casual_test, s=cvglm_casual$lambda.1se)


tidy(glmnet(data.matrix(Registered[1:8]),as.matrix(Registered[9]), lambda=cvglm_Registered$lambda.1se, alpha=1))
tidy(glmnet(data.matrix(Total[1:8]),as.matrix(Total[9]), lambda=cvglm_Total$lambda.1se, alpha=1))
x$beta



