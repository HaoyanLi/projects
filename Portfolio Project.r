data = read.csv("DSO545.csv")
library(lubridate)
data$Date = mdy(data$Date)
library(ggplot2)
library(tseries)
library(forecast)
ggplot(data, aes(x = Date, y = data$INDEX))+
  geom_line()+
  geom_line(aes(y = data$INDEX_HIGH,color = "High"))+
  geom_line(aes(y = data$INDEX_LOW,color = "Low"))+
  ggtitle("Index")+
  ylab("Price")+
  theme_bw()

par(mfrow=c(1,2))
acf(data$INDEX)
pacf(data$INDEX)
# If we try to forecast future price of our portfolio through index price, the regression
# will be inaccurate since price data is non-stationary.

## Start with the regression analysis and forecasting of your portfolio returns. 
# Use the CAPM and three-factor CAPM (Fama-French) models to estimate the coefficients 
# of the models and use them for forecasting. 

rp = diff(log(data$INDEX))
rm = diff(log(data$mkt))
rf = data$TNX/1200
rp = ts(rp, start = 2013.2, frequency = 12)
rm = ts(rm, start = 2013.2, frequency = 12)

ggtsdisplay(rp[1:59], plot.type="scatter")+
  ggtitle("Index Return")

ggtsdisplay(rm[1:59], plot.type="scatter")+
  ggtitle("Market Return")

## test whether stationary or not
rpp = rp[1:59] - rf[2:60]
rmp = rm[1:59] - rf[2:60]
library(tseries)
par(mfrow=c(1,2))
Acf(rpp)
Pacf(rpp)
adf.test(rpp)

par(mfrow=c(1,2))
Acf(rmp)
Pacf(rmp)
adf.test(rmp)

# Both are stationary.

# CAPM
capm = lm(rpp~rmp)
summary(capm)

# Fama-French
smb = data$SMB[2:60]
hml = data$HML[2:60]
data1 = data.frame(data[-1,],rp,rm,rf[-1],rmp,rpp)
ff = lm(rpp[1:59]~rmp[1:59]+smb+hml,data = data1)
summary(ff)

# Do a three-period-ahead forecasting of the portfolio returns for 
# January 2nd, 2018 to end of March, 2018. Write confidence interval.  
# Compare the forecasted values of risk premium for your portfolio with the actual values.  
# Compare the forecasted returns to your portfolio with the actual returns.

# Forecast with CAPM
rpp = rp - rf[2:60]
rmp = rm - rf[2:60]
pred1 = predict(capm, newdata=data1[57:59,], se.fit = TRUE)
pred1

ci=c(pred1$fit-1.96*pred1$se.fit, pred1$fit+1.96*pred1$se.fit)
ci



############################################
# 5. Do Na??ve, MA(3), MA(5), ES, Holt, and Holt-Winters forecasting of your portfolio returns 
# and do a three-period-ahead forecasting of the portfolio returns for each forecast. 
# Estimate the accuracy statistics.
return = diff(log(data$INDEX))[1:59]
return = ts(return, start = 2013.1, frequency = 12)

naive(return, 3)
plot(naive(return, 3))
accuracy(naive(return, 3))

# MA3
ma3 = ma(return, order=3)
plot(return, main="Index_Return-MA3")
lines(ma(return, order=3), col="red")
f_ma3=forecast(ma3,h=3)
f_ma3$mean
accuracy(f_ma3)

# MA5
ma5 <- ma(return, order=5)
plot(return, main="Index_Return-MA5")
lines(ma(return, order=5), col="red")
f_ma5=forecast(ma5,h=3)
f_ma5$mean
accuracy(f_ma5)

library(TTR)
wma = WMA(return, n=3, wts = 1:3)
plot(return, main="Index_Return-WMA3")
lines(WMA(return, n=3, wts = 1:3), col="red")
f_wma=forecast(wma,h=3)
accuracy(f_wma)
f_wma$mean


# simple exponential smoothing forecasting of the price
fit1 <- ses(return, alpha=0.2, initial="simple", h=3)
fit2 <- ses(return, alpha=0.6, initial="simple", h=3)
fit3 <- ses(return, h=3)
fit1
fit2
fit3

ggplot(data1[1:59,], aes(x = Date[1:59], y = data1$rp[1:59]))+
  geom_line()+
  geom_line(aes(y = fitted(fit1),color = "alpha=0.2"))+
  geom_line(aes(y = fitted(fit2),color = "alpha=0.6"))+
  geom_line(aes(y = fitted(fit3), color = "alpha=0.98"))+
  ggtitle("Simple exponential smoothing")

accuracy(fit1)
accuracy(fit2)
accuracy(fit3)


# Do an ARIMA model of your portfolio returns and use it for three-period ahead 
# forecasting of the returns to portfolio. Write confidence interval. Estimate the accuracy statistics.
library(forecast)
return = diff(log(data$INDEX))[1:59]
return = ts(return, start = 2013.1, frequency = 12)
Acf(return)
Pacf(return)
adf.test(return)
fit_arima = auto.arima(return)

fcastreturn = forecast(fit_arima, h=3)
plot(fcastreturn, type="l")


## 11. Test whether your portfolio conforms to the efficient market hypothesis.
price = data$INDEX[2:57]
price_lag = data$INDEX[1:56]

dp = price - price_lag
emh = lm(dp~price_lag)
summary(emh)

# conforms EMH

#
return_VTI <- diff(log(data$VTI))
return_FFIDX <- diff(log(data$FFIDX))
data2 = data.frame(data[-1,],rp,rm,return_VTI,return_FFIDX)
ggplot(data2, aes(x = Date, y = rp))+
  geom_line()+
  geom_line(aes(y = return_VTI,color = "Vanguard"))+
  ggtitle("Vanguard VS Portfolio")+
  ylab("Return")+
  theme_bw()

ggplot(data2, aes(x = Date, y = rp))+
  geom_line()+
  geom_line(aes(y = return_FFIDX,color = "Fidelity"))+
  ggtitle("Fidelity VS Portfolio")+
  ylab("Return")+
  theme_bw()




