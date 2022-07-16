###############################################################################
# Homework 4
###############################################################################


# Verifications ---------------------------------------------------------------
R.Version()
packageVersion(pkg = "forecast")
packageVersion(pkg = "xts")


# Section 1 -------------------------------------------------------------------

# Set Up
library("quantmod")

# Question 1
# Read the file goog.RDS using the function readRDS(). If goog.RDS is in your 
# working directory, the following code will read the data in and assign it to 
# goog:
# goog = readRDS("goog.RDS")
# If goog.RDS is not in your working directory, set your working directory to 
# the location of the file using setwd() and then run the above code.
# What type of data structure is goog?
goog <- readRDS("goog.RDS")
class(goog)
View(goog)
#xts


# Question 2
# What was google stock price for June, 2010?
start(goog)
time(goog)
goog[time(goog) == "Jun 2010", ]
#221.0374


# Question 3
# Using the monthly stock price for google, what is the average stock price for 
# the year 2010?
mean(goog[time(goog) == c("Jan 2010", "Feb 2010", "Mar 2010", "Apr 2010", 
                          "May 2010", "Jun 2010", "Jul 2010", "Aug 2010",
                          "Sep 2010", "Oct 2010", "Nov 2010", "Dec 2010"), ])
#260.9768


# Question 4
# How many months of data are included in this dataset?
nmonths(goog)
#142


# Question 5
# With time-series data, the past is often a good predictor of the future. Let 
# us see if this is true for our data. What is the correlation between google 
# stock price and one-month lagged stock price? You can use lag.xts() to obtain 
# a one-month lag for google stock price. When computing correlation with cor(), 
# be sure to set use='complete.obs'.
cor(goog, lag.xts(goog), use = "complete.obs")
#0.9923893


# Question 6
# In order to have access to a wider array of forecasting models, we will 
# convert the data to a "ts" data type. Also, we will split the data into a 
# train and test sample, using the train sample to estimate a model and the test 
# sample to evaluate it. We will used data from Jan, 2007 to Dec, 2015 for the 
# train sample and the rest for the test sample. The code below will convert 
# goog to a "ts" object and split the data.
google <- ts(goog, start = c(2007,01), frequency = 12)
train <- window(google, start = c(2007,01), end = c(2015,12))
test <- window(google, start = c(2016,01), end = c(2018,10))
#How many months of data does train contain?
nmonths(train)
#ALSO works: length(train), nrow(train)
#108


# Question 7
# Autocorrelation examines correlation of a variable and its lagged values. 
# Construct a plot of autocorrelations for train using ggAcf() from the forecast 
# package. Which lag has the strongest autocorrelation?
library(forecast)
ggAcf(train)
#1


# Section 2 ---------------------------------------

# Set Up 
# Same data as section 1


# Question 1
# A very simple prediction, often the baseline in linear regression, is to use 
# the average. Use the average to make a prediction for the stock price over the 
# 34 months of the test sample. Let's call this average_model. What is the point 
# forecast of the stock price for October 2018?
average_model <- meanf(train, h = length(test)); average_model
#355.776


# Question 2
# Let us examine the accuracy of the above prediction from average_model on the 
# train sample. Specifically, what is the RMSE of the prediction in the train 
# sample? Hint: Use accuracy() from library(forecast)
accuracy(average_model)
#144.5429  


# Question 3
# What is the RMSE of the average_model on the test sample?
accuracy(average_model, x = test)
#588.3496


# Question 4
# Next, let us examine another simple prediction, one that assumes the future 
# will be the same as the last observation. Let's call this naive_model. Use 
# naive_model to construct a forecast for stock price over the next 34 months 
# of the test sample. What is the point forecast of the stock price for October 
# 2018?
naive_model <- naive(train, h = length(test)); naive_model
#758.88


# Question 5
# What is the RMSE of the naive_model on the test sample?
accuracy(naive_model, x = test)
#230.50860



# Section 3 ---------------------------------------

# Set Up
# Same data as section 1


# Question 1
# There are a number of exponential smoothing models that differ in how they 
# handle errors, trend, and seasonality. Let us fit an exponential smoothing 
# model using the following function: 
# Call this ets_model.
ets_model<- ets(train, model = 'AAA')
# The errors of ets_model are
# Additive


# Question 2
# The trend for ets_model is
# Additive


# Question 3
# What is AICc for ets_model?
summary(ets_model)
# 1255.624


# Question 4
# Do the residuals of the  ets_model look like white noise? To answer this 
# question, examine an Acf() or ggAcf() plot and the result of the Ljung-Box 
# test.
checkresiduals(ets_model)
#Box.test(checkresiduals(ets_model), type = "Ljung-Box")
# Residuals are not white noise


# Question 5
# Use ets_model to construct a forecast for stock price over the next 34 months 
# of the test sample. What is the point forecast of the stock price for October 
# 2018?
ets_model_forecast <- forecast(ets_model, h = length(test)); ets_model_forecast
#1028.4942


# Question 6
# What is the RMSE of the ets_model on the test sample?
accuracy(ets_model_forecast, x = test)
#102.20154



# Section 4 ---------------------------------------

# Set Up
# Same data as section 1


# Question 1
# Now, let's use an ARIMA model on the train data. Since, there are a large 
# number of parameters with which to define the ARIMA model, let use the 
# auto.arima() function to automatically determine the parameters.
# In the class example, we set d = 1, D = 1, stepwise = F, and approximation = F.  
# Here, in this exercise, do not set d, D, stepwise, or approximation. 
# Call this auto_arima_model. Use the codes below
auto_arima_model <- auto.arima(y = train); auto_arima_model
# How many ordinary autoregressive lag variables have been used in 
# auto_arima_model?
# 0


# Question 2
# What is the number of ordinary differences used in auto_arima_model?
# 1


# Question 3
# How many ordinary moving average lags have been used in auto_arima_model?
# 0


# Question 4
# How many seasonal autoregressive lag variables have been used in 
# auto_arima_model?
# 0


# Question 5
# Do the residuals look like white noise? To answer this question, examine an 
# Acf() or ggAcf() plot and the result of the Ljung-Box test.
checkresiduals(auto_arima_model)
#Box.test(checkresiduals(auto_arima_model), type = "Ljung-Box")
# Residuals resemble white noise


# Question 6
# Use auto_arima_model to construct a forecast for stock price over the next 34 
# months of the test sample. What is the point forecast of the stock price for 
# October 2018?
auto_arima_model_forecast <- forecast(auto_arima_model, h = length(test))
auto_arima_model_forecast
#920.8568


# Question 7
# What is the RMSE of auto_arima_model on the test sample?
accuracy(auto_arima_model_forecast, x = test)
#143.69172


# Question 8
# Let us see if we can improve our ARIMA model by a variance stabilizing 
# transformation. BoxCox.lambda() is a handy function for identifying the 
# optimal value of lambda to stabilize variance. What is the optimal value of 
# lambda?
BoxCox.lambda(train)
#0.4748787


# Question 9
#Rather than using auto.arima(), let us specify an ARIMA model as follows:
arima_model <- Arima(train,order = c(1,1,1), seasonal = c(3,1,0), lambda = BoxCox.lambda(train))
# Call this arima_model. What is the AICc for arima_model?
summary(arima_model)
#343.16


# Question 10
# Examine the results of Ljung-Box test, of the arima_model, to see if the 
# residuals resemble white noise.
Box.test(checkresiduals(arima_model), type = "Ljung-Box")
#Residuals resemble white noise


# Question 11
# Use arima_model to construct a forecast for stock price over the next 34 
# months of the test sample. What is the point forecast of the stock price for 
# October 2018?
arima_model_forecast <- forecast(arima_model, h = length(test))
arima_model_forecast
#1165.1694


# Question 12
# What is the RMSE of arima_model on the test sample?
accuracy(arima_model_forecast, x = test)
#56.19179

