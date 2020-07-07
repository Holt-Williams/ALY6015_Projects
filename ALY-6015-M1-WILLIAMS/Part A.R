# G. Holt Williams ALY6015 M1 Project: Descriptive Statistics and Regression Analysis with R

#Part A

# 
# Use the “Trees” data or another data set that is part of R. Then, use the functions in sections
# 2.5, 3.5 and 3.6 of “Using R for Data Analysis and Graphics” to describe data numerically
# and construct the graphs to describe data graphically. Follow the steps below.

# 1. Invoke R and use the “Tree” dataset
#Dataset
data("trees")

# 2. Find the 5 summary numbers in the data

summary(trees)
head(trees)

# 3. Graph a straight line regression

trees%>%
  ggplot(aes(Girth,Height))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se=FALSE)+
  labs( title = "Trees Data Set: ",
  subtitle = "Height Vs Girth")+ 
  xlab(label= "Height (ft)")+
  ylab(label = "Girth (IN)")

# 4. Create Histograms and density plots

#histogram
trees%>%gather()%>%
  ggplot(aes(value))+
  geom_histogram(bins=10)+
  facet_wrap(~key, scales='free')+
  labs( title = "Trees Data Set: ",
        subtitle = "Histograms: Bins=10")+ 
  xlab(label= NULL)+
  ylab(label = "Count")

#density Plot
trees%>%gather()%>%
  ggplot(aes(value))+
  geom_density()+
  facet_wrap(~key, scales='free')+
  labs( title = "Trees Data Set: ",
        subtitle = "Density plots")+ 
  xlab(label= NULL)+
  ylab(label = "Count")


# 5. Create Boxplots
#boxplots
trees%>%gather()%>%
  ggplot(aes(value))+
  geom_boxplot()+coord_flip()+
  facet_wrap(~key, scales='free')+
  labs( title = "Trees Data Set: ",
        subtitle = "Boxplots")+ 
  xlab(label= NULL)+
  ylab(label = "Count")

# 6. Normal probability plots

trees%>%gather()%>%
  ggplot(aes(sample=value))+
  geom_qq()+
  facet_wrap(~key, scales='free')+
  labs( title = "Trees Data Set: ",
        subtitle = "Density plots")+ 
  xlab(label= NULL)+
  ylab(label = "Count")


