library(forecast)
library(zoo)

DublinWeather.df <- read.csv("DublinWeather.csv")
View(DublinWeather.df)
summary(DublinWeather.df)

#plotting timeseries
DublinWeather.ts <- ts(DublinWeather.df$wdsp, start = c(2019,
                       as.numeric(format(definingdays[1], "%j"))), freq = 365)

plot(DublinWeather.ts, xlim = c(2019, 2021.8),#plotting the timeseries
     main = "2021 Daily Forecast of Windspeed",
     ylim = c(0,25))


#DublinWeatherComp <- decompose(DublinWeather.ts, type = c("additive"))
DublinWeatherComp <- decompose(DublinWeather.ts, type = c("multiplicative"))
plot(DublinWeatherComp)


#setting dates to date format
definingdays <- seq(as.Date("2019-01-01"), as.Date("2021-10-30"), by = "day")

#splitting the data
train.ts <- window(DublinWeather.ts, start = c(2019, 1), end = c(2021, 266))
valid.ts <- window(DublinWeather.ts, start = c(2021, 267), end = c(2021, 274))


#applying rolling mean 
DublinWeather.ma <- rollmean(train.ts, k=4, align = "right")
DublinWeather.ma

#moving average
DublinWeather.ma <- rollmean(train.ts, k=4, align = "right")
plot(DublinWeather.ma, ylim = c(0,20))

DublinWeather.ma_pred <- predict(DublinWeather.ma, h=7)
DublinWeather.ma_pred <- forecast(DublinWeather.ma, h=7)


plot(DublinWeather.ts, xlim = c(2021, 2021.8),#plotting the timeseries
     main = "2021 Daily Forecast of September Windspeed (in knot)",
     ylim = c(0,25))
abline(v=2021.7266, col="grey", lty=2)
legend(x = "topright",          # Position
       legend = c("Actuals", "Moving Average"),
       lty = c(1,1,1,1,1),           # Line types
       col = c("black", "blue"),         
       lwd = 2)
lines(DublinWeather.ma_pred$fitted, col = "blue")
lines(DublinWeather.ma_pred$mean, col = "blue")
#lines(DublinWeather.ma_valid_pred, col = "blue")

DublinWeather.ma_valid <- rollmean(valid.ts, k=4, align = "right")
DublinWeather.ma_valid_pred <- predict(DublinWeather.ma_valid)
accuracy(DublinWeather.ma_pred, valid.ts)

#arima model
wdsp_arima <- auto.arima(train.ts)
arima_pred <- forecast(wdsp_arima, h=7)
#plot(wdsp_arima, ylim = c(0,20))
#plot(arima_pred)

plot(DublinWeather.ts, xlim = c(2021, 2021.8),#plotting the timeseries
     main = "2021 Daily Forecast of September Windspeed (in knot)",
     ylim = c(0,25))
abline(v=2021.7266, col="grey", lty=2)
legend(x = "topright",          # Position
       legend = c("Actuals", "ARIMA"),
       lty = c(1,1,1,1,1),           # Line types
       col = c("black", "red"),         
       lwd = 2)
lines(wdsp_arima$fitted, col = "red")
lines(arima_pred$mean, col = "red")
accuracy(arima_pred, valid.ts)
accuracy

#regression model
wdsp.lm <- tslm(train.ts ~season)
pred_tslm <- forecast(wdsp.lm, h=7)
#plot(pred_tslm)

plot(DublinWeather.ts, xlim = c(2021, 2021.8),#plotting the timeseries
     main = "2021 Daily Forecast of September Windspeed (in knot)",
     ylim = c(0,25))
abline(v=2021.7266, col="grey", lty=2)
legend(x = "topright",          # Position
       legend = c("Actuals", "Linear"),
       lty = c(1,1,1,1,1),           # Line types
       col = c("black", "orange"),         
       lwd = 2)
lines(wdsp.lm$fitted, col = "orange")
lines(pred_tslm$mean, col = "orange")
accuracy(forecast(pred_tslm), valid.ts)
wdsp.lm$residuals

#exponential smoothing
wdsp.ets <- ets(train.ts, model = "ANN")
ets.pred <- forecast(wdsp.ets, h = 7)

plot(DublinWeather.ts, xlim = c(2021, 2021.8),#plotting the timeseries
     main = "2021 Daily Forecast of September Windspeed (in knot)",
     ylim = c(0,25))
abline(v=2021.7266, col="grey", lty=2)
legend(x = "topright",          # Position
       legend = c("Actuals", "ETS"),
       lty = c(1,1,1,1,1),           # Line types
       col = c("black", "green"),         
       lwd = 2)
lines(wdsp.ets$fitted, col = "green")
lines(ets.pred$mean, col = "green")
plot(ets.pred)
accuracy(ets.pred, valid.ts)

lines(valid.ts)

#accuracy of the models
accuracy(DublinWeather.ma_pred, valid.ts)#moving average
accuracy(arima_pred, valid.ts)#arima model
accuracy(forecast(pred_tslm), valid.ts)#tslm regression model
accuracy(ets.pred, valid.ts)#ets model
