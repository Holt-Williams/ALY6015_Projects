### Weather Modelling

library(glmnet)
set.seed(1234)


test2<- long%>%group_by(Type)%>%nest()
# Define Models
ALL_model <- function(df) {
  lm(Count ~ Holiday+workingday+weathersit+Temperature+atemp+Humidity+Windspeed, data = df)
} 

test2 <- test2%>% 
  mutate(ALL_model = map(data, ALL_model))%>%mutate(
    All = map(ALL_model, broom::glance))

test2

model_table<-test2%>%mutate(
  All = map(ALL_model, broom::glance))

# R^2, AIC, BIC
test2 %>% 
  mutate(glance = map(ALL_model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

test2 %>% 
  mutate(tidy = map(ALL_model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE)


library(MASS)
library(leaps)

casual_m<-lm(Count ~ Holiday+workingday+weathersit+Temperature+atemp+Humidity+Windspeed,data=long)

Casual<-long[c(3:16)]%>%filter(Type=="Casual")


stepped<-stats::step(lm(log(Count)~ . , data=Casual[c(5,7:12,14)]),direction="both")

summary(stepped)
stepped$anova

stats::step(lm(log(Count)~ . , data=Casual[c(5,7:12,14)]),direction="both")

summary(lm(Count ~ . -instant -dteday, data=long))

hclust()



