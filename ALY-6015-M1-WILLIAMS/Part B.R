# G. Holt Williams ALY6015 M1 Project: Descriptive Statistics and Regression Analysis with R

# Part B

# Use the “Rubber” and “oddbooks” data sets, or choose two use other appropriate data sets,
# in R. Then use the functions in section 5.4 of “Using R for Data Analysis and Graphics” to
# build multiple regression models.
# In addition, you need to install the DAAG package before you can complete this part of the
# assignment. Follow the steps below:
# 1. Load the MASS and ggplot2 libraries and use the “Rubber” data set
library(MASS)
library(ggplot2)

data("Rubber")

# 2. Load the DAAG library and use the “oddblocks” data set
library(DAAG)
data("oddbooks")

# 3. Build multiple regression models using summary(), log(), lm() and ggcorrplot()
library(ggcorrplot)

#Rubber Dataset

head(Rubber)
summary(Rubber)

Rubber%>%gather()%>%
  ggplot(aes(sample=value))+
  geom_qq()+
  facet_wrap(~key, scales='free')+
  labs( title = " ",
        subtitle = "Density plots")+ 
  xlab(label= NULL)+
  ylab(label = "Count")+
  theme_tufte()
View(Rubber)

Rubber%>%gather()%>%
  ggplot(aes(value))+
  geom_density(bins=10)+
  facet_wrap(~key, scales='free')+
  labs( title = "Trees Data Set: ",
        subtitle = "Histograms: Bins=10")+ 
  xlab(label= NULL)+
  ylab(label = "Count")

Rubber%>%
  ggplot(aes(log(tens)))+
  geom_density()+
  labs( title = "Trees Data Set: ",
        subtitle = "Histograms: Bins=10")+ 
  xlab(label= NULL)+
  ylab(label = "Count")

Rubber%>%
  ggplot(aes(tens))+
  geom_density()+
  labs( title = "Trees Data Set: ",
        subtitle = "Histograms: Bins=10")+ 
  xlab(label= NULL)+
  ylab(label = "Count")


Rubber%>%
  ggplot(aes(x=hard, y=loss))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se=FALSE)+
  labs( title = " ",
        subtitle = "Scatter Plot with Linear Model")+ 
  xlab(label= "Hard")+
  ylab(label = "Loss")+
  theme_tufte()
  
Rubber%>%
  ggplot(aes(x=tens, y=loss))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se=FALSE)+
  labs( title = " ",
        subtitle = "Scatter Plot with Linear Model")+ 
    xlab(label= "Tens")+
    ylab(label = "Loss")+
    theme_tufte()



RubberModel<- lm(log(Rubber$los)~Rubber$hard + log(Rubber$tens))
summary(RubberModel)

#Correlation plot
ggcorrplot(round(cor(Rubber),2), lab=TRUE)




#Oddbooks Dataset
summary(oddbooks)
nrow(oddbooks)
head(oddbooks)

#Oddbooks EDA
oddbooks%>%gather()%>%
  ggplot(aes(x=value))+
  geom_density()+
  facet_wrap(~key, scales='free')+
  labs( title = " ",
        subtitle = "Histograms")+ 
  xlab(label= NULL)+
  ylab(label = "Count")+
  theme_tufte()

oddbooks%>%gather()%>%
  ggplot(aes(x=log(value)))+
  geom_density()+
  facet_wrap(~key, scales='free')+
  labs( title = " ",
        subtitle = "Histograms: Logged values")+ 
  xlab(label= NULL)+
  ylab(label = NULL)+
  theme_tufte()



oddbooksModel<- lm(oddbooks$weight~ oddbooks$thick+oddbooks$height+oddbooks$breadth )
summary(oddbooksModel)

oddbooksModel<- lm(log(oddbooks$weight)~oddbooks$thick+log(oddbooks$breadth))
summary(oddbooksModel)




#Correlation plot
ggcorrplot(round(cor(oddbooks),2), lab=TRUE)
