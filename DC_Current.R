train <- read.csv(file = "C:/Users/Gopal/Desktop/Materials/Data_Scientist/DC_CURRENT.csv", header = TRUE, stringsAsFactors = FALSE)

apply(is.na(train),2,sum)

install.packages("imputeTS")
install.packages("tibble")
install.packages("tseries")
install.packages("xts")
install.packages("TTR")
library(imputeTS)

imputed <- na.kalman(train$Current.DC)

train$Current_DC <- imputed
apply(is.na(train),2,sum)
train$Current.DC <- NULL
train$Current_DC <- round(train$Current_DC)

train$Time <- as.POSIXct(train$Time, format = "%d/%m/%Y %H:%M:%S")
class(train$Time)

train$Current_DC <- as.numeric(train$Current_DC)
class(train$Current_DC)

lapply(train,class)

extract1 <- subset(train, Current_DC==0)
extract2 <- subset(train, Current_DC!=0)

means <- aggregate(extract2$Current_DC, 
                   list(Time=cut(extract2$Time, "1 min")),
                   mean)
means$Current.DC <- round(means$x)
means$x <- NULL
means$Time <- as.POSIXct(means$Time, format = "%Y-%m-%d %H:%M:%S")
class(means$Time)

fit <- ts(extract2$Current_DC, start=min(means$Current.DC), frequency=24*60)
plot(fit)


fit_stl <- stl(fit,s.window = "periodic")   
plot(fit_stl)
library(forecast)
fc_stl <- forecast(fit_stl, h= 1440*5)
plot(fc_stl)
fc_stldf <- as.data.frame(fc_stl)
time1 <- as.data.frame(seq(tail(means$Time,1), length.out = nrow(fc_stldf), by = "min"))
output_stl <- as.data.frame(cbind(time1[,1],fc_stldf))
print(output_stl)



