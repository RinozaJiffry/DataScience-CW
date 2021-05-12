library(tidyverse)
library(tseries)
library(readxl)
library(neuralnet)
library(quantmod)
library(xts)
library(funtimes)
library(dplyr)
library(MLmetrics)

ExchangeUSD <- read_excel("E:/Lectures/DataScience/cw/ExchangeUSD.xlsx") 


# data Exploration 

# YYYY/MM/DD Column Plotting
plot(ExchangeUSD$`YYYY/MM/DD`) 

#  USD/EUR Column Plotting
plot(ExchangeUSD$`USD/EUR`)   

# Convert a date column from a character string
ExchangeUSD$`YYYY/MM/DD` <- as.Date(ExchangeUSD$`YYYY/MM/DD`) 

# convert ExchangeUSD excel file to time series
time_series <- xts(ExchangeUSD$`USD/EUR`,ExchangeUSD$`YYYY/MM/DD`)

# Printing time series
time_series   

# target and predictor features
original_rate <-(time_series)
leg_rate <- stats::lag(original_rate,2)
all_rate <- cbind(original_rate,leg_rate)
colnames(all_rate) <- c('original_rate', 'leg_rate')
all_rate <- na.exclude(all_rate)

# Normalization 
normalization <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

normalized <- as.data.frame(lapply(all_rate,normalization))

# Training and testing ranges
training_rate <- window(all_rate, end = '2013-05-21')
testing_rate <- window(all_rate, start = '2013-05-21')

# Plotting the training data
plot(training_rate)  


set.seed(123)
# ANN Regression Fitting

#ternage active function used
nueralNetwork_fitting <- neuralnet(original_rate~leg_rate, data=training_rate, hidden=2, act.fct= tanh)
nueralNetwork_fitting$result.matrix

# Graphic Neural network
plot(nueralNetwork_fitting) 



# Test the accuracy of the model
temporary_test <- subset(testing_rate, select = 'leg_rate')
head(temporary_test)
nueralNetwork_fitting.results <- compute(nueralNetwork_fitting, temporary_test)
results <- data.frame(actual_result = testing_rate$original_rate, predicted_result= nueralNetwork_fitting.results$net.result)
results

predicted_result <- nueralNetwork_fitting.results$net.result
actual_result <- testing_rate$original_rate

# standard statistical indices
RMSE(predicted_result,actual_result) 
MAE(predicted_result,actual_result)
MAPE(predicted_result,actual_result)

kmeans(RMSE,MAE)
plot(actual_result,main = "Real vs predicted", col='red')

