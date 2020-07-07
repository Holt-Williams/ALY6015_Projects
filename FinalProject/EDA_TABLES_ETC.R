

#### EDA, Tables, Etc,
## This script creates the tables required for the project
library(psych)
library(tidyverse)

hour[2:17]%>%
  describe(quant=c(.25,.75),omit=TRUE)%>%
  select(n, sd,mean, median, min, max, Q0.25, Q0.75)%>%round(digits=2)

c(min(hour$dteday), max(hour$dteday))

hour%>%
  group_by(season)%>% 
  summarise(Casual=sum(casual),Registered=sum(registered),Total=sum(cnt))
hour%>%
  group_by(yr)%>% 
  summarise(Casual=sum(casual),Registered=sum(registered),Total=sum(cnt))
