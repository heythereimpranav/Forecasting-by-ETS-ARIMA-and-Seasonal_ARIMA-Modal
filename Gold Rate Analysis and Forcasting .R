library(fpp3)
library(tidyverse)
library(seasonal)
library(knitr)
library(forecast)


#1. Data
gold_rate_dataframe <- Gold_Dataset[, c("Date","Gold", "India")] 

gold_rate_dataframe <- gold_rate_dataframe%>%
  mutate(Date = yearmonth(Date))%>%
  as_tsibble(index = Date, key = c(Gold))


#------------------------------------------------------------------------------#

#2. Visualization

#Time Plot
gold_rate_dataframe %>% autoplot(India)+
  labs(y = "Gold rate in INR",
       title = "Gold rate in India")

#ACF Plot
gold_rate_dataframe %>%
  ACF(India, lag_max = 48) %>%
  autoplot() +
  labs(title="ACF Plot - Gold Rate in India")

#------------------------------------------------------------------------------#

#3. Transformation
#Decomposition
dcmp <- gold_rate_dataframe %>%
  model(
    classical = classical_decomposition(India, type = "additive"),
    stl = STL(India)
  )

#STL Decomposition
stlcomps <- dcmp %>% select(stl) %>% components(dcmp)
stlcomps %>% tail() 
stlcomps %>% autoplot()


# Create a histogram to check normality of remainder component
hist(stlcomps$remainder, breaks = 20, col = "skyblue",
     main = "Histogram of Gold currency rate in india",
     xlab = "Gold rate in INR")


#------------------------------------------------------------------------------#

# 4. Transformation

# Apply Box-Cox transformation to the data
lambda <- BoxCox.lambda(stlcomps$remainder)
transformed_data <- BoxCox(stlcomps$remainder, lambda = lambda)

# Create a histogram to check normality of transformed data
hist(transformed_data, breaks = 20, col = "skyblue", 
     main = "Histogram of Transformed Data", 
     xlab = "Gold rate in Indian Curancy")


#------------------------------------------------------------------------------#

# 5. identifying data is Stationary or not ?

#KPSS test to check if data is stationary or not
gold_rate_dataframe %>% features(India, unitroot_kpss)

## here we got p-value 0.01 which is lower than significant value 0.05 therefore
## data is not stationary we need to transform our data further by applying  
## difference function.

# Gold Rates in India with Differenced Data
gold_rate_dataframe <- gold_rate_dataframe %>%
  mutate(diff = difference(India))

#ACF plot
gold_rate_dataframe %>% ACF(diff) %>%
  autoplot() + labs(subtitle = "ACF Plot - Gold curancy Rate After Differncing")

#KPSS test to check if data after differencing is stationary or not
gold_rate_dataframe %>% features(diff, unitroot_kpss)


#------------------------------------------------------------------------------#

# 6. Forecasting

#Splitting the dataset into training set and test set. 
gold_rate_dataframe_train <- gold_rate_dataframe %>% filter(year(Date) >= 2004, year(Date) <= 2008)
gold_rate_dataframe_test <- gold_rate_dataframe %>% filter(year(Date) >= 2009, year(Date) <= 2010)


#------------------------------------------------------------------------------#
# 7. ETS Model

######################  Modal 1: ETS(A, N, N) ##################################
fit_AE_rate_1 <- gold_rate_dataframe_train %>%
  model(ETS(India ~ error("A") + trend("N") + season("N")))

fc_AE_rates_1 <- fit_AE_rate_1 %>% forecast(h = 20)

#Plot the forecast
fc_AE_rates_1 %>%
  autoplot(gold_rate_dataframe_train)

#Checking Accuracy of the model
accuracy(fc_AE_rates_1, gold_rate_dataframe_test) %>%
  select(.model, RMSE, MAE)

#Residual Plots
fit_AE_rate_1 %>% gg_tsresiduals()

#------------------------------------------------------------------------------#

########################  Modal 2: ETS(A, Ad, A)  ##############################
fit_AE_rate_2 <- gold_rate_dataframe_train %>%
  model(ETS(India ~ error("A") + trend("Ad") + season("A")))

fc_AE_rates_2 <- fit_AE_rate_2 %>% forecast(h = 20)

#Plot the forecast
fc_AE_rates_2 %>%
  autoplot(gold_rate_dataframe_train)

#Checking Accuracy of the model
accuracy(fc_AE_rates_2, gold_rate_dataframe_test) %>%
  select(.model, RMSE, MAE)

#Residual Plots
fit_AE_rate_2 %>% gg_tsresiduals()
#------------------------------------------------------------------------------#

# 8. ARIMA

#PACF plot
gold_rate_dataframe %>%
  PACF(diff) %>%
  autoplot() +
  labs(title="PACF Plot - INDIAN GOLD RATE in INR")

gold_rate_dataframe %>%
  ACF(diff) %>%
  autoplot() +
  labs(title="ACF Plot - INDIAN GOLD RATE in INR")

###################  Modal 3: ARIMA(1,1,0)   ###################################

AEC_fit_1 <- gold_rate_dataframe_train %>%
  model(ARIMA(diff ~ pdq(1, 1, 0)))
report(AEC_fit_1)

AEC_fc_1 <- AEC_fit_1 %>% forecast(h = 10)

#Plot the forecast
AEC_fc_1 %>%
  autoplot(gold_rate_dataframe_train)

#Checking Accuracy of the model
accuracy(AEC_fc_1, gold_rate_dataframe_test) %>%
  select(.model, RMSE, MAE)

#Residual Plots
AEC_fit_1 %>% gg_tsresiduals()

##################   Modal 4: ARIMA(0,1,0)   ###################################

AEC_fit_2 <- gold_rate_dataframe_train %>%
  model(ARIMA(diff ~ pdq(0, 1, 0)))
report(AEC_fit_2)

AEC_fc_4 <- AEC_fit_2 %>% forecast(h = 10)

# Plot the forecast
AEC_fc_4 %>%
  autoplot(gold_rate_dataframe_train)

# Checking Accuracy of the model
accuracy(AEC_fc_4, gold_rate_dataframe_test) %>%
  select(.model, RMSE, MAE)

# Residual Plots
AEC_fit_2 %>% gg_tsresiduals()


############# Modal 5: ARIMA(0,0,0)+(1,1,0)   ##################################

AEC_fit_3 <- gold_rate_dataframe_train %>%
  model(ARIMA(diff ~ pdq(0, 0, 0) + PDQ(1, 1, 0)))
report(AEC_fit_3)

AEC_fc_3 <- AEC_fit_3 %>% forecast(h = 10)

#Plot the forecast
AEC_fc_3 %>%
  autoplot(gold_rate_dataframe_train)

#Checking Accuracy of the model
accuracy(AEC_fc_3, gold_rate_dataframe_test) %>%
  select(.model, RMSE, MAE)

#Residual Plots
AEC_fit_3 %>% gg_tsresiduals()

#------------------------------------------------------------------------------#
