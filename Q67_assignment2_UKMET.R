
library(forecast)
library(zoo)
library(dplyr)
library(reshape2)
library(lubridate)
library(readxl)

#-------------------------------DATA READ & PREPARATION------------------------------

setwd("P:\\Courses\\MMA 867 - Anton Ovchinnikov Predictitve Analytics\\Assignments\\Ass2- Team1\\Data\\CRU-global")
met_data <- read_excel("HadCRUT4_median_TS_monthly.xlsx")
pred <- read_excel("pred.xlsx")
str(pred)
pred$Date <- as.Date(pred$Date)

str(met_data)


#Drop confidence interval variables
met_data1 <- met_data %>% select(c("Year", "Month", "Temp"))

# converting anomalies to actual temperature
met_data1 <- mutate(met_data1, temperature_degrees = met_data1$Temp + 14, temperature_fahrenheit = met_data1$Temp + 57.2) 

#Convert to date format
met_data1$Date <- mdy(paste(met_data1$Month,"-01-",met_data1$Year,sep=""))

#Arrange by date so that TS can be formed & filter until 2007 data 
met_data1 <- met_data1  %>% arrange(Date) %>% dplyr::filter(Date<'2007-01-01')

#-------------------------------Question 6 : Predictions for 2007-2017-----------------------------
met_ts <- ts(met_data1$temperature_degrees,start=c(1850,1), end= c(2006,12), frequency=12) 
fit <- decompose(met_ts)
plot(fit)

met_msts <- msts(met_data1$temperature_degrees, seasonal.periods = c(12,120))
fit1 <- decompose(met_msts) 
monthplot(met_msts)
plot(cycle(met_msts))
plot(fit1) 

#MSTS Auto Arima (3,1,1)(0,1,0)[120]
met_arima <- arima(met_msts, order=c(3,1,1), seasonal = c(0,1,0))
met_arima_pred <- forecast(met_arima,132)
plot(met_arima_pred, xlab = 'Year', ylab = "Predicted Climate")
met_predicted <- data.frame(met_arima_pred)


#MSTS Auto Arima (3,1,1)(0,1,0)[120]
f_auto_arima_met <- function(y, h) forecast(Arima(y), order = c(3,1,1), seasonal=c(0,1,0), h = h) 
errors_auto.arima_met <- tsCV(met_msts, f_auto_arima_met, h=1, window=132)
mape_auto.arima_met <-mean(abs(errors_auto.arima_met/met_msts), na.rm=TRUE)*100 #[1] 0.9482007
mae_auto.arima_met <-mean(abs(errors_auto.arima_met), na.rm=TRUE)*100 #[1] 13.14862

met_pred_months <- cbind(pred,met_predicted)

write.csv(met_pred_months,"met_prediction6.csv", row.names = FALSE)

#-------------------------------Question 7 : Predictions for 2009-2019-----------------------------

#Drop confidence interval variables
met_data2 <- met_data %>% select(c("Year", "Month", "Temp"))

# converting anomalies to actual temperature
met_data2 <- mutate(met_data2, temperature_degrees = met_data2$Temp + 14, temperature_fahrenheit = met_data2$Temp + 57.2) 

#Convert to date format
met_data2$Date <- mdy(paste(met_data2$Month,"-01-",met_data2$Year,sep=""))

#Arrange by date so that TS can be formed & filter until 2007 data 
met_data2 <- met_data2  %>% arrange(Date) %>% dplyr::filter(Date<'2009-01-01')

met_msts2 <- msts(met_data2$temperature_degrees, seasonal.periods = c(12,120))
fit2 <- decompose(met_msts2) 
plot(fit2) 

#MSTS Auto Arima (3,1,1)(0,1,0)[120]
met_arima2 <- arima(met_msts2, order=c(3,1,1), seasonal = c(0,1,0))
met_arima_pred2 <- forecast(met_arima2,132)
plot(met_arima_pred2, xlab = 'Year', ylab = "Predicted Climate")
met_predicted2 <- data.frame(met_arima_pred2)


#MSTS Auto Arima (3,1,1)(0,1,0)[120]
f_auto_arima_met <- function(y, h) forecast(Arima(y), order = c(3,1,1), seasonal=c(0,1,0), h = h) 
errors_auto.arima_met2 <- tsCV(met_msts2, f_auto_arima_met, h=1, window=132)
mape_auto.arima_met2 <-mean(abs(errors_auto.arima_met2/met_msts2), na.rm=TRUE)*100 #[1] 0.9441627
mae_auto.arima_met2 <-mean(abs(errors_auto.arima_met2), na.rm=TRUE)*100 #[1] 13.09789

pred2 <- read_excel("pred2.xlsx")
str(pred2)
pred2$Date <- as.Date(pred2$Date)

met_pred_months2 <- cbind(pred2,met_predicted2)

write.csv(met_pred_months2,"met_predictionq7.csv", row.names = FALSE)

