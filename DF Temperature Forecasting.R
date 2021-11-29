library(forecast)
library(MLmetrics)

weather <- read.csv("/Users/mohit/Desktop/Trinity College/BA- Semester 1 /Data Forecasting/Group Assignment/New Data/DublinWeather.csv")

##Required changes -- NEW CODING

plot(new_x, ylab = "Temperature", 
     main = "Day Level Temperature of Dublin",
     ylim = c(-6,26),
     xlim = c(2019,2022), col="Skyblue")
lines(new_y, col= "darkblue")

#abline(v=2021.6, col="orange", lty=2)

legend(x = "topright",          # Position
       legend = c("Mini Temp", "Maxi Temp"), 
       lty = c(1,1),           # Line types
       col = c("Skyblue", "Darkblue"),         
       lwd = 2)

##############################################################################

inds <- seq(as.Date("2019-01-01"), as.Date("2021-09-30"), by = "day") 
attach(weather)
new_x <- ts(mintp,frequency = 365,start = c(2019,as.numeric(format(inds[1], "%j"))))
#plot(new_x)
data= new_x
length(data)
#Create samples
training <- window(data, start = c(2019,1), end = c(2021,243))
validation <- window(data, start = c(2021,244), end= c(2021,274))
length(training) +length(validation)
#View(weather)

########################################################################
#Naive Method
naive = snaive(training, h=length(validation))
#MAPE(naive$mean, validation) * 100
MAE(naive$mean, validation)

#plot(data, col="blue", xlab="Year", ylab="Minimum Temperature", main="Seasonal Naive Forecast", type='l')
#lines(naive$mean, col="red", lwd=2)

plot(data, ylab = "Temperature", 
     main = "Model Forecast for Minimum Temperature",
     ylim = c(-6,20),
     xlim = c(2021,2021.8), col="Skyblue")
lines(naive$mean, col= "violet")

abline(v=2021.66, col="gray", lty=2)

legend(x = "topright",          # Position
       legend = c("Actual Data", "Naive Forecast"), 
       lty = c(1,2),           # Line types
       col = c("Skyblue", "orange"),         
       lwd = 2)


residual_naive <- (validation-naive$mean)
plot(residual_naive, ylab = "Temperature", 
     main = "Residual Graph",
     ylim = c(-8,15),
     #xlim = c(2021,2021.8),
     col="violet")
#plot(residual_naive)

########################################################################
########################################################################
#Exponential Smoothing 
ets_model = ets(training, allow.multiplicative.trend = FALSE)
summary(ets_model)

#New work
exp_sm <- stlf(training,h=length(validation),method="ets")
exp_sm$mean
length(validation)

ets_forecast = forecast(ets_model, h=length(validation))
MAE(exp_sm$mean, validation)

plot(data, xlim=c(2021,2022),col="skyblue", xlab="Year", ylab="Temperature", main="Exponential Smoothing", type='l')
lines(exp_sm$mean, col="green", lwd=2)
abline(v=2021.66, col="gray", lty=2)
legend(x = "topright",          # Position
       legend = c("Actual Data", "ets forecast"), 
       lty = c(1,2),           # Line types
       col = c("Skyblue", "orange"),         
       lwd = 2)


residual_ets <- (validation - ets_forecast$mean)
plot(residual_ets, col="green")
lines(residual_ets, col="green")
########################################################################
#ARIMA / SARIMA Model
arima_optimal = auto.arima(training)

ari_for <- stlf(training,h=length(validation),method="arima")

plot(data, xlim = c(2021, 2021.8),col="skyblue", xlab="Year", ylab="Temperature", main="Arima", type='l')
lines(ari_for$mean, col="red", lwd=2)

legend(x = "topleft",          # Position
       legend = c("Actual Data", "Naive","ets","ARIMA"), 
       lty = c(1,2,2,2),           # Line types
       col = c("Skyblue", "violet", "green", "red"),         
       lwd = 2)


tsmod <- stlm(training, modelfunction = ar)
plot(forecast(tsmod, h = 90))


fit <- tslm(training ~ trend + season)
forecassttt <- (forecast(fit, h =90))
lines(forecassttt$mean, col = "orange")

library(astsa)

sarima_forecast = sarima.for(training, n.ahead=length(validation),
                             p=3,d=0,q=2,P=0,D=1,Q=0,S=365)

MAE(ari_for$mean, validation)


residual_arima <- (ari_for$mean - validation)
lines(residual_arima, col="red")
legend(x = "topleft",          # Position
       legend = c("Naive", "ets", "ARIMA"), 
       lty = c(2,2,2),           # Line types
       col = c("violet", "green","red" ),         
       lwd = 2)


##Mean of Naive and ARIMA forecasted
na_ar_for <- (naive$mean+ari_for$mean)/2

na_ar_res <- (residual_arima+ residual_naive)/2
lines(na_ar_res, col = "black")

MAE(na_ar_for, validation)



##Residual graph analysis for Minimum temperature
residual_naive <- (validation-naive$mean)
plot(residual_naive, ylab = "Temperature", 
     main = "Residual Graph for Minimum Temperature",
     ylim = c(-8,15),
     #xlim = c(2021,2021.8),
     col="violet")
lines(residual_ets, col="green")
lines(residual_arima, col="red")
lines(na_ar_res, col = "black")
legend(x = "topleft",          # Position
       legend = c("Naive", "ets", "ARIMA", "Mean of Naive+ARIMA"), 
       lty = c(2,2,2,2),           # Line types
       col = c("violet", "green","red","black" ),         
       lwd = 2)
####################################################################

####################################################################

#For maximum temperature
inds <- seq(as.Date("2019-01-01"), as.Date("2021-09-30"), by = "day") 
new_y <- ts(maxtp,frequency = 365,start = c(2019,as.numeric(format(inds[1], "%j"))))
plot(new_y)

data <- new_y

#Create samples
training <- window(data, start = c(2019,1), end = c(2021,243))
validation <- window(data, start = c(2021,244), end= c(2021,274))
length(training) +length(validation)
#View(weather)
########################################################################
#Naive Method
naive = snaive(training, h=length(validation))
MAPE(naive$mean, validation) * 100

plot(data, ylab = "Temperature", 
     main = "Model Forecast for Maximum Temperature",
     ylim = c(0,28),
     xlim = c(2021,2021.8), col="Skyblue")
lines(naive$mean, col= "violet")

abline(v=2021.66, col="gray", lty=2)

legend(x = "topright",          # Position
       legend = c("Actual Data", "Naive Forecast"), 
       lty = c(1,2),           # Line types
       col = c("Skyblue", "orange"),         
       lwd = 2)


residual_naive <- (validation-naive$mean)
plot(residual_naive, ylab = "Temperature", 
     main = "Residual Graph",
     ylim = c(-8,30),
     #xlim = c(2021,2021.8),
     col="violet")
#plot(residual_naive)



########################################################################
#Exponential Smoothing 
ets_model = ets(training, allow.multiplicative.trend = FALSE)
summary(ets_model)

#New work
exp_sm <- stlf(training,h=length(validation),method="ets")
exp_sm$mean
length(validation)

ets_forecast = forecast(ets_model, h=length(validation))
MAE(exp_sm$mean, validation[1:90]) *100

plot(data, xlim=c(2021,2022),col="skyblue", xlab="Year", ylab="Temperature", main="Exponential Smoothing", type='l')
lines(exp_sm$mean, col="green", lwd=2)
abline(v=2021.66, col="gray", lty=2)
legend(x = "topright",          # Position
       legend = c("Actual Data", "ets forecast"), 
       lty = c(1,2),           # Line types
       col = c("Skyblue", "orange"),         
       lwd = 2)


residual_ets <- (validation - ets_forecast$mean)
lines(residual_ets, col="green")
########################################################################

#ARIMA / SARIMA Model
arima_optimal = auto.arima(training)

ari_for <- stlf(training,h=length(validation),method="arima")

plot(data, xlim = c(2021, 2021.8),col="skyblue", xlab="Year", ylab="Temperature", main="Arima", type='l')
lines(ari_for$mean, col="red", lwd=2)

legend(x = "topleft",          # Position
       legend = c("Actual Data", "Naive","ets","ARIMA"), 
       lty = c(1,2,2,2),           # Line types
       col = c("Skyblue", "violet", "green", "red"),         
       lwd = 2)


tsmod <- stlm(training, modelfunction = ar)
plot(forecast(tsmod, h = 90))


fit <- tslm(training ~ trend + season)
forecassttt <- (forecast(fit, h =90))
lines(forecassttt$mean, col = "orange")

library(astsa)

sarima_forecast = sarima.for(training, n.ahead=length(validation),
                             p=3,d=0,q=2,P=0,D=1,Q=0,S=365)

MAE(sarima_forecast$pred, validation) 


residual_arima <- (ari_for$mean - validation)
lines(residual_arima, col="red")
legend(x = "topleft",          # Position
       legend = c("Naive", "ets", "ARIMA"), 
       lty = c(2,2,2),           # Line types
       col = c("violet", "green","red" ),         
       lwd = 2)

##Mean of Naive and ARIMA forecasted
na_ar_for <- (naive$mean+ari_for$mean)/2

na_ar_res <- (residual_arima+ residual_naive)/2
lines(na_ar_res, col = "black")

MAE(na_ar_for, validation)

##Residual graph analysis for Maximum temperature
residual_naive <- (validation-naive$mean)
plot(residual_naive, ylab = "Temperature", 
     main = "Residual Graph for Maximum Temperature",
     ylim = c(-8,15),
     #xlim = c(2021,2021.8),
     col="violet")
lines(residual_ets, col="green")
lines(residual_arima, col="red")
lines(na_ar_res, col = "black")
legend(x = "topleft",          # Position
       legend = c("Naive", "ets", "ARIMA", "Mean of Naive+ARIMA"), 
       lty = c(2,2,2,2),           # Line types
       col = c("violet", "green","red","black" ),         
       lwd = 2)
######################## -------- ########################
####################### Thank you #######################

