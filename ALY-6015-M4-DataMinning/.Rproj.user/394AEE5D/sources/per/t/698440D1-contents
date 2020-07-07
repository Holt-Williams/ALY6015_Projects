##Data
##Source: http://archive.ics.uci.edu/ml/datasets/Insurance+Company+Benchmark+%28COIL+2000%29

#Data Set Information:
  
#  Information about customers consists of 86 variables and includes product usage data and socio-demographic data derived from zip area codes. The data was supplied by the Dutch data mining company Sentient Machine Research and is based on a real world business problem. The training set contains over 5000 descriptions of customers, including the information of whether or not they have a caravan insurance policy. A test set contains 4000 customers of whom only the organisers know if they have a caravan insurance policy.
library(glmnet)
library(tidyverse)
df<-read.table("ticdata2000.txt", header=FALSE)

dictionary<-read.csv("dictionary.txt")
var<- (dictionary$DATA.DICTIONARY[2:87])
# x<-df[,1:85]
# y<-as.integer(df$"86 CARAVAN Number of mobile home policies 0 - 1")
#Change factors as required
df$V1<-factor(df$V1)
df$V4<-factor(df$V4)
df$V6<-factor(df$V6)
df$V44<-factor(df$V44)
df$V86<-factor(df$V86)
df<-df[c(1:4,6:86)]
test<-df
var<-str_sub(var,3)
var<-gsub(" ","_", var)
colnames(df)<-var[c(1:4,6:86)] 
head(df)

##Models 
bench<-glm(df$`_CARAVAN_Number_of_mobile_home_policies_0_-_1` ~ ., data=df, family = "binomial")
summary(bench)
coeff<-as.data.frame(summary(bench)$coef)%>%rownames_to_column()
coeff%>%filter(coeff[,5]<0.05)

tidy(bench)%>%filter(p.value<0.1)


glm1<-glmnet(as.matrix(df[c(1:84)]),as.matrix(df$`_CARAVAN_Number_of_mobile_home_policies_0_-_1`),family = "binomial")
summary(glm1)
plot(glm1)

cvglm1000<-cv.glmnet(data.matrix(df[c(1:84)]),as.matrix(df$`_CARAVAN_Number_of_mobile_home_policies_0_-_1`),family = "binomial", nlambda=1000, alpha=1)
plot(cvglm1000)
cvglm1000$lambda.min

cvglm100<-cv.glmnet(data.matrix(df[c(1:84)]),as.matrix(df$`_CARAVAN_Number_of_mobile_home_policies_0_-_1`),family = "binomial", nlambda=100, alpha=1)
plot(cvglm100)
cvglm100$lambda.min


minValue<- glmnet(data.matrix(df[c(1:84)]),as.matrix(df$`_CARAVAN_Number_of_mobile_home_policies_0_-_1`),family = "binomial", lambda=cvglm1$lambda.min, alpha=1)
View(tidy(minValue))
minValue$beta


tLL <- minValue$nulldev - deviance(minValue)
k <- minValue$df
n <- minValue$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc

BIC<-log(n)*k - tLL
BIC

cvglm1$lambda.1se
lambda1SE<- glmnet(data.matrix(df[c(1:84)]),as.matrix(df$`_CARAVAN_Number_of_mobile_home_policies_0_-_1`),family = "binomial", lambda=0.01456348, alpha=1)
summary(lambda1SE)
lambda1SE$beta

tLL <- lambda1SE$nulldev - deviance(lambda1SE)
k <- lambda1SE$df
n <- lambda1SE$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc

BIC<-log(n)*k - tLL
BIC



test<-cbind(bench$coefficients,glm_min$beta,glm_1se$beta)
tidy(test)
 
bench$beta

bc<-tibble(tidy(minValue$beta))
gc<-tibble(tidy(lambda1SE$beta))

View(left_join(bc[-2],gc[-2], by='row'))
class(bench$coefficients)
tidy(bench$coefficients)
class(bc)
tail(bc)
gc

#Tidy models test
#install.packages("tidymodels")
library(tidymodels)
library(tidyverse)

set.seed(555)
ins<-read.table("ticdata2000.txt", header=FALSE)
ins$V1<-factor(ins$V1)
ins$V4<-factor(ins$V4)
ins$V6<-factor(ins$V6)
ins$V44<-factor(ins$V44)
ins$V86<-factor(ins$V86)
data_split <- initial_split(ins, strat=V86)
train_data <- training(data_split)
test_data  <- testing(data_split)

#Dataset Cleaning recipe
#Need to figure out how to make factors here
ins_rec <- 
  recipe(V86 ~ ., data = train_data)%>%
  update_role(V5, new_role="ID")%>%
  step_dummy(all_nominal(), -all_outcomes())%>%
  step_zv(all_predictors())

#Set the model
lr_mod <- 
  logistic_reg(mixture=1, penalty=0.01456348) %>% 
  set_engine("glmnet")%>%
  set_mode("classification")%>%
  translate()

#Tunes lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

#Set the 'workflow', which means sets teh steps things happen
ins_wflow <- 
  workflow() %>% 
  add_model(lr_mod)%>%
  add_recipe(ins_rec)

ins_boot <- bootstraps(train_data, strata = V86)

lasso_grid <- tune_grid(
  ins_wflow %>% add_model(lr_mod),
  resamples = ins_boot,
  grid = lambda_grid)

  
  lasso_grid %>%
  collect_metrics()
  
  highest_accuracy<-lasso_grid %>%
    select_best("accuracy")
  
  final_lasso <- finalize_workflow(
    ins_wflow %>% add_model(lr_mod),
    highest_accuracy)

#fit the model to the data
ins_fit <- 
  final_lasso %>% 
  fit(data = train_data)

ins_fit<-
  ins_wflow%>%
  fit(data=train_data)

ins_fit %>% 
  pull_workflow_fit() %>% 
  pluck("fit")%>% coef(s=0.01456348)

final_lasso%>%pull_workflow_fit()

summary(ins_fit)

#Show the outcomes
ins_fit %>% 
  pull_workflow_fit() %>% 
  vi(lambda=highest_accuracy$penalty)%>%
  tidy()%>%View()
ins_fit

#Predict outcomes on the test_data
predict(ins_fit, test_data)
nrow(test_data)
ins_pred <- 
  predict(ins_fit, test_data)  %>% 
  bind_cols(test_data %>% select(V86))

ins_pred %>% 
  roc_curve(truth = V86, .pred_0) %>% 
  autoplot()

ins_pred %>% 
  roc_auc(truth = V86, .pred_0)

ins_pred%>%accuracy(truth = V86, .pred_class)




