## G. Holt Williams ALY6015 M2 Project: Inferential Statistics

# # Use one of the real world example data sets from R 
# (not previously used in the R practiceassignment) 
# or a dataset you have found, and at least two of the 
# tests and R functions covered in the practice assignment 
# to conduct a hypothesis test then report your findings
# # and give proper conclusion(s).

# You will report your work and findings in a 1000 word paper.
# Use the following supporting materials for R syntax, data sets and tools, along with other
# resources found in this module or that you find on your own.
# • Using T-Tests in R from Department of Statistics at UC Berkley
# • Test of equal or given proportions from R Documentation
# • F-Test: Compare Two Variances in R from STHDA (Statistical tools for highthroughput data analysis)
library(MASS)
library(tidyverse)
library(ggthemes)
data("cabbages")
head(cabbages)

cabbages%>%group_by()%>%summarize(n=n())
cabbages%>%group_by(Cult)%>%summarize(n=n())
cabbages%>%group_by(Date)%>%summarize(n=n())

cabbages%>%
  ggplot(aes(HeadWt))+
  geom_histogram(binwidth = .25)
cabbages%>%
  ggplot(aes(HeadWt, col=Cult))+
  geom_density()+ 
  theme_tufte()


cabbages%>%
  ggplot(aes(VitC))+
  geom_histogram(binwidth=5)
cabbages%>%
  ggplot(aes(VitC, col=Cult))+
  geom_density()+ 
  theme_tufte()

cabbages%>%select(Cult,HeadWt,VitC)%>%
  ggplot(aes(HeadWt, fill=Cult))+
  geom_boxplot()+
  coord_flip()+ 
  theme_tufte()

cabbages%>%select(Cult,HeadWt,VitC)%>%
  ggplot(aes(VitC, fill=Cult))+
  geom_boxplot()+
  coord_flip()+ 
  theme_tufte()

#test the diffrences in Cultivar by Headweight
t.test(HeadWt~Cult ,  data=cabbages)

#test the diffrences in Cultivar by VitC
t.test(VitC~Cult ,  data=cabbages)

#F-test:
var.test(HeadWt~Cult ,  data=cabbages)
