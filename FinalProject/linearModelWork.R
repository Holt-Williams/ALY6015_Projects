

### Linear Models


colnames(hour)

df<-hour[,c(2:14,17)]

first<- lm(cnt~., data=df)
summary(first)
