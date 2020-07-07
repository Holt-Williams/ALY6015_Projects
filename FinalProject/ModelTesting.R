

#3 Linear modelling
library(tidymodels)
library(tidyverse)
library(glmnet)
library(broom)

#### This code Nests and unnests with the models
# long<-hour%>%gather("type","count",15:17)
# long$type<-as.factor(long$type)
### Nest and mapping models
set.seed(1234)
test<- long%>%group_by(Type)%>%nest()


## Define Models
yr_model <- function(df) {
  lm(Count ~ Year, data = df)
}
season_model <- function(df) {
  lm(Count ~ season, data = df)
}
mnth_model <- function(df) {
  lm(Count ~ Month, data = df)
} 
Weekday_model <- function(df) {
  lm(Count ~ Weekday, data = df)
} 
hr_model <- function(df) {
  lm(Count ~ hr, data = df)
} 

ALL_model <- function(df) {
  lm(Count ~ Year+season+Month+Weekday+hr, data = df)
} 

Registerred_model <- function(df) {
  lm(Count ~ Year+season+Month+workingday+hr+weathersit+atemp+windspeed, data = df)
}

non_time <- function(df) {
  lm(Count ~ weathersit+temp+atemp+hum+windspeed, data = df)
} 

non_time <- function(df) {
  cv.glm(Count ~ weathersit+temp+atemp+hum+windspeed, data = df)
} 

## Run and attach models to list dataframe, A function would probably work here
test <- test%>% 
  mutate(yr_model = map(data, yr_model))
test <- test%>% 
  mutate(season_model = map(data, season_model))
test <- test%>% 
  mutate(mnth_model = map(data, mnth_model))
test <- test%>% 
  mutate(Weekday_model = map(data, Weekday_model))
test <- test%>% 
  mutate(hr_model = map(data, hr_model))
test <- test%>% 
  mutate(ALL_model = map(data, ALL_model))
test <- test%>% 
  mutate(Registerred_model = map(data, Registerred_model))
test <- test%>% 
  mutate(non_time = map(data, non_time))


test

tab<-tibble()


model_table<-test%>%mutate(
  All = map(ALL_model, broom::glance),
  Year= map(yr_model, broom::glance),
  Season= map(season_model, broom::glance),
  Month= map(mnth_model, broom::glance),
  Weekday= map(Weekday_model, broom::glance),
  Hour= map(hr_model, broom::glance)
)
output_table<-model_table[9:14]%>%unnest(c(All,Year,Season,Month,Weekday,Hour),names_sep=".", .drop = TRUE)%>%
  select(matches("r.squared"),matches("AIC"),matches("BIC"))

tidy(t(output_table))%>%arrange(.rownames)





  test %>% 
  mutate(glance = map(ALL_model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)%>%select(matches("\\d$")r.squared,adj.r.squared, AIC, BIC)

tidy1 <- test %>% 
  mutate(tidy = map(season_model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE)%>%select(term,estimate, p.value)




glance <- test %>% 
  mutate(glance = map(ALL_model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
tidy1 <- test %>% 
  mutate(tidy = map(hr_model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE)%>%select(term,estimate, p.value)

values<-as.data.frame(tidy1)
max(values$p.value)
View(values)




glance%>%ggplot(aes(x=r.squared, y=type))+geom_col(position = "dodge2")
glance$r.squared

## Second Model
wkday_model <- function(df) {
  lm(count ~ Weekday, data = df)
} 

test <- test%>% 
  mutate(weekday_model = map(data, wkday_model))
glance <- test %>% 
  mutate(glance = map(weekday_model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
tidy1 <- test %>% 
  mutate(tidy = map(weekday_model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE)

## Third Model - weatehr
wthr_model <- function(df) {
  lm(count ~ weathersit, data = df)
} 
test <- test%>% 
  mutate(weather_model = map(data, wthr_model))
glance <- test %>% 
  mutate(glance = map(weather_model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
tidy1 <- test %>% 
  mutate(tidy = map(weather_model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE)





##### 
long_split<-initial_split(long, prop=3/4,strata = type)
train<-training(long_split)
test<-testing(long_split)

lm_mod<-
  linear_reg()%>%
    set_engine("lm")

cnt_lm_fit<-
  lm_mod%>%
  fit(count~ weekday + mnth, data=train[which(train[,15] =="casual")])

regs_lm_fit<-
  lm_mod%>%
  fit(count~ weekday + mnth, data=train[train$type=="registered"])

casual_lm_fit<-
  lm_mod%>%
  fit(count~ weekday + mnth, data=train[train$type=="casual"])


tidy(casual_lm_fit)
tidy(regs_lm_fit)
tidy(cnt_lm_fit)





pred<-predict(lm_fit,
              new_data = test)

pred%>%collect_metrics()












## Below this doesn't amek sense

## Auto ARIMA Riderhsip Data
library(forecast)
library(tidyverse)

all_data<-hour$cnt
register_data<-hour$registered
Casual_data<-hour$casual

hour%>%group_by(weekday)%>%ggplot(aes(x=cnt, group=hr))+geom_boxplot()+coord_flip()
hour%>%group_by(weekday)%>%ggplot(aes(x=registered, group=hr))+geom_boxplot()+coord_flip()
hour%>%group_by(weekday)%>%ggplot(aes(x=casual, group=hr))+geom_boxplot()+coord_flip()

hour%>%group_by()




ts<-ts(df$cnt,frequency=8760,start = c(2011, 1))
autoplot(ts) +
  ylab("Ridership by Hour") 

ts%>%mstl()%>%autoplot()


auto.

 all<-auto.arima(ts)

plot(forecast(all,h=100))

tb<-tbats(ts)
autoplot(forecast(tb, h=100))

