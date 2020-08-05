

library(forecast)
library(zoo)
library(dplyr)
library(reshape2)
library(lubridate)

getwd()
data <- read.csv("GLB.Ts+dSST.csv", skip=1, header = TRUE)
str(data)
#Drop seasonal variables
drop <- c("J.D","D.N","DJF","MAM","JJA","SON")
nasa1 <- data %>% select(-c(drop))

#For 2020 data which has **** values , need to be in character before we reshape
nasa1 <- nasa1 %>% mutate_if(is.factor,as.character)

#Reshape/ unpivot data
nasa_monthly <- reshape2::melt(nasa1,id.var=c('Year'))  

#rename to month for readability
nasa_monthly <- rename(nasa_monthly, "month"="variable")

nasa_monthly1 <- nasa_monthly #Backup data

#Convert to date format
nasa_monthly$Date <- mdy(paste(nasa_monthly$month,"-01-",nasa_monthly$Year,sep=""))

#Arrange by date so that TS can be formed
nasa_monthly <- nasa_monthly %>% dplyr::filter(Date<'2020-04-01') %>% arrange(Date)

#change back value to numeric
nasa_monthly$value <- as.numeric(nasa_monthly$value)
##Time Series Models- basic

nasa_ts <- ts(nasa_monthly$value,start=c(1880,1), frequency=12) 

#Plot ts formed
plot(decompose(nasa_ts))
autoplot(decompose(nasa_ts))
monthplot(nasa_ts, labels=1:12, xlab="Months")
plot(cycle(nasa_ts))

nasa_ts_AAN <- ets(nasa_ts, model="AAN")
nasa_ts_AAZ <- ets(nasa_ts, model="AAZ", damped=FALSE)
nasa_ts_MMN <- ets(ts(nasa_monthly$value+14,start=c(1880,1), frequency=12), model="MMN", damped=FALSE)
nasa_ts_MMZ <- ets(ts(nasa_monthly$value+14,start=c(1880,1), frequency=12), model="MMZ", damped=FALSE)# seasonality change to automatic results in "N" for this data

# Create their prediction
nasa_ts_AAN_pred <- forecast(nasa_ts_AAN, h=120, level=c(0.8, 0.95))
nasa_ts_AAZ_pred <- forecast(nasa_ts_AAZ, h=120, level=c(0.8, 0.95))
nasa_ts_MMN_pred <- forecast(nasa_ts_MMN, h=120, level=c(0.8, 0.95))
nasa_ts_MMZ_pred <- forecast(nasa_ts_MMZ, h=120, level=c(0.8, 0.95))

par(mfrow=c(1,4)) # This command sets the plot window to show 1 row of 4 plots
plot(nasa_ts_AAN_pred, xlab="Year", ylab="Predicted Climate")
plot(nasa_ts_AAZ_pred, xlab="Year", ylab="Predicted Climate")
plot(nasa_ts_MMN_pred, xlab="Year", ylab="Predicted Climate")
plot(nasa_ts_MMZ_pred, xlab="Year", ylab="Predicted Climate")

#Ts- tbats

nasa_tbats <- tbats(nasa_msts)
plot(nasa_tbats)

nasa_tbats_pred <-forecast(nasa_tbats, h=120, level=c(0.8, 0.95))
par(mfrow=c(1,1))
plot(nasa_tbats_pred, xlab="Year", ylab="Predicted Climate")



#Auto.arima
nasa_ts_reg <- auto.arima(nasa_ts, trace=T,D=1, seasonal = TRUE) #Order of seasonal differencing=1

plot(forecast(nasa_ts_reg, 120))
# Best model: ARIMA(2,0,2)(2,1,0)[12] with drift 



# MSTS Multiple Seasonality 


#TBATS MSTS
nasa_msts <- msts(nasa_monthly$value , seasonal.periods=c(3,12,12*10))
plot(cycle(nasa_msts))
nasa_msts_tbats <- tbats(nasa_msts)
par(mfrow=c(1,1))
plot(nasa_msts_tbats)

nasa_msts_tbats$seasonal.periods
plot(decompose(nasa_msts, type="multiplicative")) #decompose using "classical" method, multiplicative 

stl <- stl(nasa_msts, s.window = 'periodic')
plot(stl)


