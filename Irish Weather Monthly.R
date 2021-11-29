library(forecast)
library(zoo)

#reading and converting file to 2021 only
weather.data <- read.csv("GlasnevinMonthly.csv")
View(weather.data)
weather.data2 <- weather.data[889:971,]
max(weather.data$date)

#creating time series
rain.ts <- ts(weather.data2$rain,
              start = c(2015,1),
              end = c(2021,10), freq = 12)
View(rain.ts)
summary(rain.ts)
sd(rain.ts)

components.ts <- decompose(rain.ts)
plot(components.ts)

#changing margins to avoid error
par("mar")
par(mar=c(4,4,4,4))

#plotting time series
plot(rain.ts, 
     xlab = "Date",
     ylab = "Rain (in mm)", 
     bty = "l")

#splitting the data
train.ts <- window(rain.ts, start = c(2015, 1), end = c(2021,7))
valid.ts <- window(rain.ts, start = c(2021, 8), end = c(2021, 10))

#applying rolling mean
rain.ma <- rollmean(rain.ts, k=2, align = "right")
rain.ma

#plotting the data
plot(train.ts, 
     main = "2021 Monthly Forecast of Aug-Oct Rainfall (in mm)",
     ylim = c(0,200),
     xlim = c(2019,2022))
abline(v=2021.58333, col="grey", lty=2)
legend(x = "topright",          # Position
       legend = c("Actuals", "ARIMA", "ETS","Linear", "2 Mth MA"), 
       lty = c(1,1, 1,1,1),           # Line types
       col = c("black", "red", "green","orange", "blue"),         
       lwd = 2) 
lines(rain_arima$fitted, col="red")
lines(arima_pred$mean, col= "red", lty=2)
lines(ets.pred$fitted, col="green")
lines(ets.pred$mean, col="green", lty=2)
lines(prediction_tslm$fitted, col="orange")
lines(prediction_tslm$mean, col="orange", lty=2)
lines(rain.movingaverage, col="blue")
lines(predict.movingaverage$mean, col="blue", lty=2)


lines(rain.ts)

#MA
rain.full.ma <- rollmean(train.ts, k=2, align = "right")
plot(rain.full.ma, ylim = c(0,20))
rain.full.ma_pred <- predict(rain.full.ma, h=5)
rain.full.ma_valid <- rollmean(valid.ts, k=2, align = "right")
rain.full.ma_valid_pred <- predict(rain.full.ma_valid)
pred.ma <- forecast(rain.full.ma, h=3)
accurary.ma <- accuracy(pred.ma,valid.ts)

rain.movingaverage <- ma(train.ts, 2, centre =FALSE)
predict.movingaverage <- forecast(rain.movingaverage, h=3)

#arima
rain_arima <- auto.arima(train.ts, D=1)
rain_arima$fitted
arima_pred <- forecast(rain_arima, h=3)
plot(train.ts, ylim = c(0,20))
lines(rain_arima, col="blue")
arima_acc <- accuracy(arima_pred, valid.ts)
arima_acc


acf(train.ts,lag.max=13)


#linear regression model
rain.lm <- tslm(train.ts ~ season)
prediction_tslm <- forecast(rain.lm, h=3)
accuracy(forecast(prediction_tslm), valid.ts)
rain.lm$residuals

rain.ets$fitted

#exponential smoothing.
rain.ets <- ets(train.ts, model = "ZZA")
rain.ets$fitted
ets.pred <- forecast(rain.ets, h=3)
accuracy(ets.pred, valid.ts)
?ets
