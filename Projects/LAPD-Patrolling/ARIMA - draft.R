data = read.csv(file = 'data_cleaning_2.csv', stringsAsFactors = F)

library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling
library(lubridate)
library(tidyverse)

# try to do the daily analysis
new_data = as.data.frame(table(data$DATE.OCC))

daily_raw = new_data[,2]

daily = ts(daily_raw , frequency = 365, start = c(2019,1,1))
plot.ts(daily)	

m6 <- auto.arima(daily)		# fits ARIMA(p,d,q) x (P, D, Q) automatically

m6.predict <- forecast:::forecast.Arima(m6, h = 365, level = c(68, 90))
plot(m6.predict)

data$DATE.OCC <- as.POSIXct(data$DATE.OCC,format='%m/%d/%y')
data <- mutate(data, ym = format(data$DATE.OCC, "%y-%m"))

ym1 = as.data.frame(table(data$ym))
ym1 = ym1[-c(40), ]
ym_raw = ym1[,2]

monthly = ts(ym_raw , frequency = 12, start = c(2019,1))
monthly

mon_const_var = BoxCox(monthly,lambda = BoxCox.lambda(monthly))
mon_const_var
autoplot(mon_const_var)

mon_const_var_no_seasonality = diff(x = mon_const_var,lag = 12) 
mon_const_var_no_seasonality
autoplot(mon_const_var_no_seasonality)

nsdiffs(mon_const_var)
ndiffs(mon_const_var_no_seasonality)

ggAcf(monthly)
ggAcf(mon_const_var)
ggAcf(mon_const_var_no_seasonality)

adf.test(mon_const_var_no_seasonality)
Pacf(monthly, lag.max = 12)


month1 <- diff(monthly,differences = 1)	
Pacf(month1, lag.max = 12)
ggAcf(month1)

dat = cbind(original = monthly,
            const_var = month1,
            no_seasonality = mon_const_var_no_seasonality)
library(ggthemes)
autoplot(dat,facets=T,colour = T)+ylab('')+xlab('Year')+theme_bw()


m7 <- auto.arima(monthly)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
m7
m7.predict <- forecast:::forecast.Arima(m7, h = 12, level = c(50, 90))
plot(m7.predict)


m8 <- Arima(monthly, order=c(0,1,1), seasonal = list(order = c(0,2,1), period = 12))
m8
m8.predict <- forecast:::forecast.Arima(m8, h = 12, level = c(50, 90))
plot(m8.predict)
