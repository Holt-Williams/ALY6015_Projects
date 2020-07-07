##### Back to Forecastign TIme series

library(forecast)


Casual<-long[c(15:16)]%>%filter(Type=="Casual")
Casual<-Casual[2]

Registered<-long[c(15:16)]%>%filter(Type=="Registered")
Registered<-Registered[2]

Total<-long[c(15:16)]%>%filter(Type=="Total")
Total<-Total[2]

casual_ts<-ts(Casual)
Registered_ts<-ts(Registered)
Total_ts<-ts(Total)

casual_arima<-auto.arima(ts[1:720])
autoplot(forecast(casual_arima, h=100))+
  coord_cartesian(xlim = c(17360, 17480))

casual_tbat <- tbats(casual_ts)
autoplot(forecast(casual_tbat, h=100))+
  coord_cartesian(xlim = c(17360, 17480))

acf(casual_arima$residuals)

3*24

nrow(Casual)

library(zoo)
x<-zoo(long[c(2,16)], seq(from = as.Date(min(long$dteday)), to = as.Date(max(long$dteday)), by = 1/24))

auto.arima(x[,2])


Casual_day<-long%>%group_by(Type,dteday,hr)%>%summarise(Count=Count)%>%filter(Type=="Casual")


Casual_day<-zoo(Casual_day, seq(from = as.Date(min(long$dteday)), to = as.Date(max(long$dteday)), by = 1))
Casual_arima<-auto.arima(Casual_day[,3])
autoplot(forecast(Casual_arima, h=90))+
  coord_cartesian(xlim = c(nrow(Casual_day)-200, nrow(Casual_day)+30))























