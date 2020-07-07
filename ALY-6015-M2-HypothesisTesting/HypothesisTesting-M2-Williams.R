## G. Holt Williams ALY6015 M2 Project: Hypothesis Testing with R



# 
# First, load the MASS library in R.
# A. Package ‘MASS’ which provides a description of the datasets available in the MASS
# package
library(MASS)
library(tidyverse)
library(broom)

# Then, complete the following analysis of the identified data from the library.
# B. One-sample t-test:
#   Use the “chem” dataset to answer the question, “is the flour production company
# producing whole meal flour with greater than 1 part per million copper in it?”

data(chem)
summary(chem)
str(chem)

stargazer(tidy(t.test(chem)))

# C. Two-sample t-test:
#   Use the “cats” dataset to answer the question, “do male and female cat samples have
# the same body weight?”
# Hint: one way to get separate vectors for male and female cat body weight is to use
# the subset function as follows: “male <-subset(cats, subset=(cats$Sex=M))”
data(cats)
head(cats)
t.test(Bwt~Sex ,  data=cats)

# D. Paired t-test:
#   Use the “shoes” dataset to answer the question, “did material A wear better than
# material B?”
data(shoes)
head(shoes)
df<-data.frame(c(shoes[1],shoes[2]))
df<- df%>%gather()

t.test(value~key ,  data=df)

# E. Test of equal or given proportions:
#   Use the “bacteria” data set to answer the question, “did the drug treatment have a
# significant effect of the presence of the bacteria compared with the placebo?”
data(bacteria)
head(bacteria)
str(bacteria)
table<-bacteria%>%select(y,ap)%>%table()
str(table)
colnames(table)<-c("Active", "Placebo")
rownames(table)<- c("No", "Yes")
prop.test(table[2,], n=c(124,96))

# F. F-test:
#   Use the “cats” data set to test for the variance of the body weight in male and female
# cats.
data(cats)
head(cats)
str(cats)
var.test(Bwt~Sex ,  data=cats)


cats%>%summarise(N=n(), Mean=mean(), Median=median(), SD=sd(), Var=var(), Min=min(), Max=max())%>%round(digits=2)

test<- funs(N= n,
            Min = min, 
            q25 = quantile(., 0.25), 
            Median = median, 
            q75 = quantile(., 0.75), 
            Max = max,
            Mean = mean, 
            SD = sd)
install.packages("psych")
library(psych)

cats$Sex%>%table()
 cats%>%describe(quant=c(.25,.75),omit=TRUE)%>%select(n, sd,mean, median, min, max, Q0.25, Q0.75)
 cats$Sex%>%prop.table()
as.data.frame(describe(cats))

cats%>%select(Bwt,Hwt)%>%summarise_all(list(min = min, max = max))

mutate

library(broom)
library(purrr)

t1<-t.test(Bwt~Sex ,  data=cats)
str(t1)
tidy(t1)
View(tidy(t1))
class(test)



