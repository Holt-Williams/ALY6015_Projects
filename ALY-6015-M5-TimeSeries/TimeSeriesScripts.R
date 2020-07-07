library(tidyverse)
library(ggfortify)
install.packages("ggfortify")
### Time Series Analysis Scripts

##dataset
data("wineind")
df<-wineind
dfts<-wineind


dfts<- ts(df,frequency=1, start=c(1875))
dfts

plot.ts(dfts)


## test log of ts
plot.ts(log(dfts))
## No Diffrence

### Decomposing
library("TTR")

## Simple moving average (Average of x years)
plot.ts(SMA(dfts,freq))


## Forecasting with Expopnential Smoothing

holtdfts<-HoltWinters(dfts, beta=FALSE, gamma=FALSE)


holtdfts$fitted

plot(holtdfts)
holtdfts$SSE

# Now forecast after  the last year
library(forecast)
holtdfts2<- forecast(dfts,h=15)
holtdfts2

plot(holtdfts2)


acf(holtdfts2$residuals, lag.max=20)

Box.test(holtdfts2$residuals, lag=20, type="Ljung-Box")

plot.ts(holtdfts2$residuals)



### From a lil book fo R
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


plotForecastErrors(holtdfts2$residuals)







### ARMIA


## Diffrence time series, brings it toward th emean

dfdiff<-diff(dfts,diffrences=1)
plot.ts(dfdiff)

acf(dfdiff, lag.max=20)             # plot a correlogram
acf(dfdiff, lag.max=20, plot=FALSE) # get the autocorrelation values


pacf(dfdiff, lag.max=20)             # plot a partial correlogram
pacf(dfdiff, lag.max=20, plot=FALSE)


auto.arima(dfts)
## says ARIMA(0,1,0) is best

dftsarima <- arima(dfts, order=c(0,1,0)) # fit an ARIMA(0,1,0) model
dftsarima

## Forecast with ARIMA
dftsarimaforecast<-forecast(dftsarima, h=5)

plot(dftsarimaforecast)

acf(dftsarimaforecast$residuals, lag.max=20)
Box.test(dftsarimaforecast$residuals, lag=20, type="Ljung-Box")

plot.ts(dftsarimaforecast$residuals)
plotForecastErrors(dftsarimaforecast$residuals)

