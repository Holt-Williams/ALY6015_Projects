## Basic EDA

## Load Libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)

### Line graph of daily users
hour%>%group_by(dteday)%>%mutate(sum=sum(cnt),reg=sum(registered), cas=sum(casual))%>%ggplot()+
  geom_line(aes(x=dteday, y=reg, color="red"))+
  geom_line(aes(x=dteday, y=cas, color="Blue"))+
  scale_x_date(date_labels="%Y %b %d",
               limit=c(as.Date("2012-01-01"),as.Date("2013-01-01")))+
  theme_tufte()


hour%>%ggplot(aes(fill=yr,y=cnt,x=mnth))+
  geom_bar(position="dodge", stat="identity")

hour%>%group_by(yr,mnth)%>%summarise(cnt=mean(cnt))%>%ungroup%>%ggplot(aes(x=mnth,y=cnt))+
  geom_line()+
  geom_point(aes(color=yr))





### Boxplots


long%>%
  group_by(type,season)%>%
  mutate(sum=sum(count))%>%
  ggplot(aes(x=sum, color=season, group=type))+
  geom_boxplot()



long%>%
  group_by(type,dteday)%>%
  mutate(sum=sum(count))%>%
  ggplot(aes(x=sum, fill=season))+
  geom_boxplot()+
  facet_grid(~type)+
  coord_flip()+
  theme_tufte()+
  xlab("Ridership")









## HEatmap test, Doesn't Work

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 

p <-ggplot(hour,aes(day,hour,fill=temp))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hrly Temps C",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
