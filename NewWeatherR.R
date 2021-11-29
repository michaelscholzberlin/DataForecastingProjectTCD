library(forecast)
library(zoo)

#reading file
weather.data <- read.csv("DublinWeather.csv")
View(weather.data)
weather.data2 <- weather.data[2:1004,]

#setting dates to date format
defining.days <-  seq(as.Date("2019-01-01"), as.Date("2021-09-30"), by = "day")
View(defining.days)

#creating time series
rain.ts <- ts(weather.data2$rain,
              start = c(2019, as.numeric(format(defining.days[1], "%j"))),
              frequency = 365)
summary(rain.ts)
sd(rain.ts)
var(rain.ts)

#splitting the data
train.ts <- window(rain.ts, start = c(2019, 1), end = c(2021, 266))
valid.ts <- window(rain.ts, start = c(2021, 267), end = c(2021, 273))


#changing margins to avoid error
par("mar")
par(mar=c(4,4,4,4))

decompose <- decompose(rain.ts, type="additive")
plot(decompose)


#plotting time series
plot(rain.ts, 
     main = "2019-2021 Daily Rainfall",
     xlab = "Date",
     ylab = "Rain (in mm)", 
     bty = "l")

#applying rolling mean
rain.ma <- rollmean(rain.ts, k=2, align = "right")
rain.ma

#plotting the data
plot(train.ts, 
     main = "2021 Daily Forecast of Last September Week Rainfall (in mm)",
     ylim = c(0,30),
     xlim = c(2021.5,2021.75))
abline(v=2021.7287, col="grey", lty=2)
legend(x = "topright",          # Position
       legend = c("Actuals", "ARIMA", "ETS", "Linear", "2 Day MA (Naive)"), 
       lty = c(1,1,1,1,1),           # Line types
       col = c("black", "red", "green", "orange", "blue"),         
       lwd = 2) 
lines(rain_arima$fitted, col="red")
lines(arima_pred$mean, col= "red", lty = 2)
lines(ets.pred$fitted, col="green")
lines(ets.pred$mean, col="green", lty = 2)
lines(rain.movingaverage, col="blue")
lines(movingaverageprediction2, col="blue", lty = 2)
lines(rain.lm$fitted.values, col = "orange")
lines(prediction_tslm$mean, col = "orange" , lty = 2)

lines(valid.ts)

#MA
rain.full.ma <- rollmean(train.ts, k=2, align = "right")
plot(rain.full.ma, ylim = c(0,20))
rain.full.ma_pred <- predict(rain.full.ma, h=7)
rain.full.ma_valid <- rollmean(valid.ts, k=2, align = "right")
rain.full.ma_valid_pred <- predict(rain.full.ma_valid)
ma.pred <- forecast(rain.full.ma, h=30)
ma_acc <- accuracy(ma.pred, valid.ts)

rain.movingaverage <- ma(train.ts, 2, centre =FALSE)
predict.movingaverage <- forecast(rain.movingaverage, h=7, model = NULL)
summary(predict.movingaverage)

movingaverageprediction <- rep(0.0125, 7)
movingaverageprediction2 <- cbind(c(2021.7288,2021.7315,2021.7342,2021.7370,2021.7397,2021.7425,2021.7452),movingaverageprediction)

ma_acc <- accuracy(movingaverageprediction , valid.ts)


#arima
rain_arima <- auto.arima(train.ts)
arima_pred <- forecast(rain_arima, h=7)
plot(train.ts, ylim = c(0,20))
lines(rain_arima, col="blue")
arima_acc <- accuracy(arima_pred, valid.ts)
arima_acc


#linear regression model
rain.lm <- tslm(train.ts ~ trend + season)
prediction_tslm <- forecast(rain.lm, h=7)
accuracy(forecast(rain.lm), valid.ts)
rain.lm$residuals



#exponential smoothing.
rain.ets <- ets(train.ts, model = "ANN")
ets.pred <- forecast(rain.ets, h=7)
accuracy(ets.pred, valid.ts)
