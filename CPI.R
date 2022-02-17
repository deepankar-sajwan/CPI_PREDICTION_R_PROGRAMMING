
## Loading all the Libraries ##

library(forecast)
library(ggplot2)
library(readxl)
library(tidyverse)
library(tseries)
library(xts)
library(xlsx)
library(forecast)
library(readxl)
library(dplyr)
library(psych)

### Read the CPI XLSX file ###

cpi_data <- read.xlsx("FINAL_CPI_DATA_SET.xlsx", sheetIndex = 1) # Read the sheet named 'ALL_INDIA'

dim(cpi_data)

#view(cpi_data) # Use for displaying the selected data set

names(cpi_data) # To list the column names 

# descriptive = describe(as.data.frame(cpi_data)[3:12]) # For Showing descriptive statistics

write.xlsx(describe(as.data.frame(cpi_data)[3:12]), file = "CPI.xlsx", sheetName = "Descriptive", append = TRUE) # Have to run only 1 time for entire state

#head(cpi_data) # For reading the first 6 values of data set

#tail(cpi_data) # For reading the last 6 values of data set

boxplot(as.data.frame(cpi_data)[3:12]) # To show the outliers

is.na(as.data.frame(cpi_data)[1:10,3:12]) # To check that is there is any missing value or not.

write.xlsx(is.na(as.data.frame(cpi_data)[1:10,3:12]), file = "CPI.xlsx", sheetName = "Missing Values", append = TRUE) # Have to run only 1 time for entire state

### Do it for each commodity seperately ###

# Converting data into time series object

cpi_ts <- ts(cpi_data$MF_CPI, start= c(2013,1), end= c(2021,3), frequency = 12)

# Plot the time series

autoplot(cpi_ts)+
  ggtitle("India | Conusmer Price Index for Meat and fish")+
  xlab("Year")+
  ylab("CPI Index")

# Decomposition of time series plot sing STL to detect anomaly detection

cpi_decomposition=stl(cpi_ts, s.window = "p")

autoplot(cpi_decomposition, main = "Decomposition by STL method")

write.xlsx(head(as.data.frame(as.xts(cpi_decomposition$time.series))), file = "MF_CPI.xlsx", sheetName = "Decomposition", append = TRUE)

#Observations:
# 1. Seasonality keeps changing with time. Fluctuation are clearly vsisible and increase with time.
# 2. Increasing trend in series
# 3. There are some random variations.
# 4. There are 3 components - Trend , Seasonality and Residual (Random, Noise, Irregular, Remainder) error 

#Decompose Time series data into additive components

#cpi_ts_add=decompose(cpi_ts, type="additive")

#autoplot(cpi_ts_add, main = "Decomposition by Decompse additive method")

#Decompose Time series data into multiplicative components

#cpi_ts_multi=decompose(cpi_ts, type="multiplicative")

#autoplot(cpi_ts_multi, main = "Decomposition by Decompse multiplicative method")

### Methods to find stationarity of a data set ###

# 1. Checking stationarity by using Auto Correlation Function

lags=window(cpi_ts, start = c(2013,1))

ggAcf(lags) # ACF of Original time series shows downwards trend which says that it is non stationary.

## 2. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) ##

kpss.test(cpi_ts) #Its is non-stationary because the values is less than 0.05

## 3. Phillips-Perron test (PP) Test ##

## pp.test(cpi_ts) #Its is non-stationary because the values is greater than 0.05

#################################


### Converting Non Stationary to Stationary ###

# cpi_diff = diff(cpi_ts) # Taking difference

# autoplot(cpi_diff, main="First Order Differencing")

### TRAINING DATA AND TEST DATA ###

cpi_train= window(cpi_ts, end=c(2020,03), frequency=12) # Training Data Set
cpi_test= window(cpi_ts, start=c(2020,04),frequency=12) # Test Data Set

# check autocorrelation function

# acf(cpi_train)

# Check Partial Correlational function

# pacf(cpi_train)

### MODEL 1 - ARIMA MODEL ####

# Auto ARIMA Model

cpi_arima<-auto.arima(cpi_train, seasonal = TRUE, ic="bic") # Creating an arima object

#summary(cpi_arima) # Summary of the arima object

# Forecasting

cpi_arima_forecast=forecast(cpi_arima, h=12) # h in forecast signifies the duration of period for which we are forecasting

autoplot(cpi_arima_forecast) + 
  ggtitle("India | Consumer Price Index for Meat and fish using Auto ARIMA")+
  xlab("Year")+
  ylab("CPI Index")

write.xlsx(cpi_arima_forecast, file = "MF_CPI.xlsx", sheetName = "Arima Forecast", append = TRUE)

## Actual vs Forecast in ARIMA Model ##

cpi_bind_arima=cbind(cpi_test,as.data.frame(cpi_arima_forecast)[,1])

ts.plot(cpi_bind_arima,ylab="CPI Index", lwd=2, col= 2:4, main="India | CPI for Meat and fish: Actual vs Forecasted using Auto ARIMA")
legend('right', col= c(2:4), bty='n', legend=c('Actual', 'Forecasted'), fill = 2:4, cex=1)

error <- as.data.frame(cpi_arima_forecast)[,1] - cpi_test # Calculate Error - Forecasted Value - Test Value

cpi_table_arima = cbind(cpi_bind_arima,error) # Table Showing actual, forecasted and error values

colnames(cpi_table_arima) <- c("Actual Value", "Forecasted Value", "Error") # Rename column

write.xlsx(cpi_table_arima, file = "MF_CPI.xlsx", sheetName = "Arima - Actual Vs Forecasted", append = TRUE)

# Accuracy of the forecast with the actual values

#accuracy(cpi_arima_forecast, cpi_test)

# Check residuals and histograms

#checkresiduals(cpi_arima_forecast) 

#### ARIMA MODEL END ###

### MODEL 2 - Holt-Winter's additive method (Triple exponential smoothing) ###

cpi_hw=hw(cpi_train, h=12, seasonal = "a") # Creating Holt-winter's object

autoplot(cpi_hw) +
  ggtitle("India | Forecast CPI for Meat and fish using Holt-Winter's additive method")+
  xlab("Year")+
  ylab("CPI Index")

#cpi_hw$model # Defination of the model

write.xlsx(cpi_hw, file = "MF_CPI.xlsx", sheetName = "Holts Winter's Forecast", append = TRUE)

#accuracy(cpi_hw, cpi_test) # Accuracy of the forecast with the actual values

## Actual vs Forecast in Holt's Winter Model ##

cpi_bind_hw=cbind(cpi_test,as.data.frame(cpi_hw)[,1])

ts.plot(cpi_bind_hw, ylab="CPI Index", lwd=2, col= 2:4, main="India | CPI for Meat and fish: Actual vs Forecasted using Holt-Winter's additive method")
legend('right', col=c(2:4), bty='n', legend=c('Actual', 'Forecasted'), fill = 2:4, cex =1)

error <- as.data.frame(cpi_hw)[,1] - cpi_test # Calculate Error - Forecasted Value - Test Value

cpi_table_hw = cbind(cpi_bind_hw,error) # Table Showing actual, forecasted and error values

colnames(cpi_table_hw) <- c("Actual Value", "Forecasted Value", "Error") # Rename column

write.xlsx(cpi_table_hw, file = "MF_CPI.xlsx", sheetName = "Holts Winter's - Actual Vs Forecast", append = TRUE)

### Holt-Winter's additive method (Triple exponential smoothing) ends ###


### Model 3 - Neural Network Model ###

cpi_neural=nnetar(cpi_train)

# summary(cpi_neural) # Summary of the model

cpi_neural_forecast=forecast(cpi_neural, h=12)

autoplot(cpi_neural_forecast)+
  ggtitle("India | Forecast CPI for Meat and fish using Neural Network")+
  xlab("Year")+
  ylab("CPI Index")

write.xlsx(t(as.data.frame(cpi_neural_forecast)), file = "MF_CPI.xlsx", sheetName = "Neural Forecast", append = TRUE)

#accuracy(cpi_neural_forecast,cpi_test) # Accuracy of the forecast with the actual values

## Actual vs Forecast in Neural Network Model ##

cpi_neural_forecas1 = as.numeric(t(as.data.frame(cpi_neural_forecast))[,1:2])

cpi_neural_forecast2 = na.omit(cpi_neural_forecas1)

cpi_bind_neural=cbind(cpi_test,cpi_neural_forecast2)

ts.plot(cpi_bind_neural, ylab="CPI Index", lwd=2, col= 2:4, main="India | CPI for Meat and fish: Actual vs Forecasted using Neural Network")
legend('right', col=c(2:4), bty='n', legend=c('Actual', 'Forecasted'), fill = 2:4, cex = 1)

error <- cpi_neural_forecast2 - cpi_test # Calculate Error - Forecasted Value - Test Value

cpi_table_neural = cbind(cpi_bind_neural,error) # Table Showing actual, forecasted and error values

colnames(cpi_table_neural) <- c("Actual Value", "Forecasted Value", "Error") # Rename column

write.xlsx(cpi_table_neural, file = "MF_CPI.xlsx", sheetName = "Neural - Actual Vs Forecast", append = TRUE)

### Neural Network Model Ends ###


### ETS Model ###

cpi_ets <-ets(cpi_train)

cpi_ets_forecast=forecast(cpi_ets, h=12) # h in forecast signifies the duration of period for which we are forecasting

autoplot(cpi_ets_forecast, xlab="Time", ylab="CPI") + ggtitle("India | Forecast CPI for Meat and fish using ETS")

#summary(cpi_ets) # Summary of the model

write.xlsx(cpi_ets_forecast, file = "MF_CPI.xlsx", sheetName = "ETS Forecast", append = TRUE)

#accuracy(cpi_ets_forecast,cpi_test) # Accuracy of the forecast with the actual values

## Actual vs Forecast in ETS Model ##

cpi_bind_ets=cbind(cpi_test,as.data.frame(cpi_ets_forecast)[,1])

ts.plot(cpi_bind_ets, ylab="CPI Index", lwd=2, col= 2:4, main="India | CPI for Meat and fish: Actual vs Forecasted using ETS")
legend('right', col=c(2:4), bty='n', legend=c('Actual', 'Forecasted'), fill = 2:4, cex = 1)

error <- as.data.frame(cpi_ets_forecast)[,1] - cpi_test # Calculate Error - Forecasted Value - Test Value

cpi_table_ets = cbind(cpi_bind_ets,error) # Table Showing actual, forecasted and error values

colnames(cpi_table_ets) <- c("Actual Value", "Forecasted Value", "Error") # Rename column

write.xlsx(cpi_table_ets, file = "MF_CPI.xlsx", sheetName = "ETS - Actua Vs Forecast", append = TRUE)


### ETS Model Ends ###


### Comparative analysis of the forecasted values with the actual values ###

Model1 = as.data.frame(accuracy(cpi_arima_forecast, cpi_test)[2,1:8])
Model2 = as.data.frame(accuracy(cpi_hw, cpi_test)[2,1:8])
Model3 = as.data.frame(accuracy(cpi_neural_forecast, cpi_test)[2,1:8])
Model4 = as.data.frame(accuracy(cpi_ets_forecast, cpi_test)[2,1:8])

evaluate= cbind(Model1, Model2, Model3, Model4)

colnames(evaluate) <- c("ARIMA", "Holts Winter's", "Neural Network", "ETS") # Rename column

write.xlsx(evaluate, file = "CPI.xlsx", sheetName = "Evaluate_MF", append = TRUE)

##### END #####

