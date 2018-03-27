train <- read.csv("C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/AV Hiring Hack/train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/AV Hiring Hack/test.csv", header = TRUE, stringsAsFactors = FALSE)

extract <- subset(train, select=c(1,2,8))
extract$datetime <- as.POSIXct(extract$datetime)
sapply(extract,class)

install.packages("forecast")      #zoo package will also get installed
install.packages("xts")

firstHour <- 24*(as.Date("2013-07-01 00:00:00")-as.Date("2013-1-1 00:00:00"))
secondhour <- 24*(as.Date("2017-06-23 23:00:00")-as.Date("2017-1-1 00:00:00"))
fit <- ts(extract$electricity_consumption, start=c(2013,firstHour), end=c(2017,secondhour), frequency=24*365)
plot(fit)

fit_stl <- stl(fit,s.window = "periodic")   
plot(fit_stl)                           #there is a trend

#Choosing a forecasting model
library(xts)
xts_dataframe <- xts(extract$electricity_consumption, order.by = extract$datetime)
head(xts_dataframe)

#ets
library(dplyr)
fit_ets = ets(xts_dataframe)
library(forecast)
fc__ets = forecast(fit_ets)
plot(fc__ets)     #Not a proper visualization of graph
fc_ets <- as.data.frame(fc__ets)
time1 <- as.data.frame(seq(tail(extract$datetime,1), length.out = nrow(fc_ets), by = "hour"))
output_ets <- as.data.frame(cbind(time1[,1],fc_ets))
print(output_ets)
accuracy(fit_ets$fitted,xts_dataframe)

#auto.arima
fit_arima <- auto.arima(xts_dataframe)
fc__arima <- forecast(fit_arima)
plot(fc__arima)   #Not a proper visualization of graph
fc_arima <- as.data.frame(fc__arima)
time2 <- as.data.frame(seq(tail(extract$datetime,1), length.out = nrow(fc_arima), by = "hour"))
output_arima <- as.data.frame(cbind(time2[,1],fc_arima))
print(output_arima)
accuracy(fit_arima$fitted,xts_dataframe)

barplot(c(ETS=fit_ets$aic, ARIMA=fit_arima$aic),
        col="light blue",
        ylab="AIC")



