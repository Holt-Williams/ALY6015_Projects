## Load Librairies
library(tidyverse)

## load data
hour<-read.csv("hour.csv")


## Clean and create factor variables
cols<-colnames(hour)[3:10]
hour<-hour%>%mutate_at(cols, funs(factor(.)))
hour$dteday<-as.Date(hour$dteday)
levels(hour$season)<-c("Winter","Spring","Summer","Fall")
levels(hour$yr)<-c("2011","2012")
levels(hour$mnth)<-c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")
levels(hour$weekday)<-c("Sun","Mon","Tues","Weds","Thurs", "Fri", "Sat")
levels(hour$weathersit)<-c("Clear, Few clouds, Partly cloudy, Partly cloudy","Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist","Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds","Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog")
