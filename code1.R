## We need some cleaning of dataset, and modify it for our use.

data <- read.csv("Dataset.csv")
data <- cbind(1:8643, data)
colnames(data)[1:3] <- c("Index", "Start","End")

data <- data[1:8640, ]
data <- as.data.frame(data)

## Handling missing values:
## If you have missing values represented as "NA," you can replace them with zeros using the na_replace() 
## function from the tidyr package. However, replacing missing values with zeros may not be appropriate 
## in all cases. It depends on the nature of the data and the analysis you want to perform. Alternatively, 
## you can choose to interpolate or impute missing values based on the context.




##################*********************************************************###########################
library(ggplot2)
library(tidyr)
library(dplyr)
library(imputeTS)
library(forecast)
library(SimTools)
library(mice)
library(zoo)
library(pracma)
#################**********************************************************##########################


## Replacing NA with 0

data_ <- mutate_all(data, ~replace_na(.,0))
chain <- data_[, 4:13]
chain <- as.matrix(chain)
traceplot(chain)
acfplot(chain, lag.max = 5000)


# Extract all the columns separately
pm10 <- data_$PM10..µg.m3.
pm2.5 <- data_$PM2.5..µg.m3.
NO <- data_$NO..µg.m3.
NO2 <- data_$NO2..µg.m3.
NOX <- data_$NOX..ppb.
CO <- data_$CO..mg.m3.
SO2 <- data_$SO2..µg.m3.
NH3 <- data_$NH3..µg.m3.
Ozone <- data_$Ozone..µg.m3.
Benzene <- data_$Benzene..µg.m3.


# Fit an ARIMA model for each column separately
arima_pm10 <- auto.arima(pm10)
arima_NO <- auto.arima(NO)
arima_NO2 <- auto.arima(NO2)
arima_NOX <- auto.arima(NOX)
arima_CO <- auto.arima(CO)
arima_SO2 <- auto.arima(SO2)
arima_NH3 <- auto.arima(NH3)
arima_Ozone <- auto.arima(Ozone)
arima_Benzene <- auto.arima(Benzene)
arima_pm2.5 <- auto.arima(pm2.5)

k = colnames(data)[4:13]
dat <- matrix(0,nrow= 10, ncol =4)
dat[1, ] <- c(k[1],paste("ARIMA",arimaorder(arima_pm10)[1],arimaorder(arima_pm10)[2],
                         arimaorder(arima_pm10)[3]), arima_pm10$aic, arima_pm10$bic)

dat[2, ] <- c(k[2],paste("ARIMA",arimaorder(arima_NO)[1],arimaorder(arima_NO)[2],
                         arimaorder(arima_NO)[3]), arima_NO$aic, arima_NO$bic)

dat[3, ] <- c(k[3],paste("ARIMA",arimaorder(arima_NO2)[1],arimaorder(arima_NO2)[2],
                         arimaorder(arima_NO2)[3]), arima_NO2$aic, arima_NO2$bic)

dat[4, ] <- c(k[4],paste("ARIMA",arimaorder(arima_NOX)[1],arimaorder(arima_NOX)[2],
                         arimaorder(arima_NOX)[3]), arima_NOX$aic, arima_NOX$bic)

dat[5, ] <- c(k[5],paste("ARIMA",arimaorder(arima_CO)[1],arimaorder(arima_CO)[2],
                         arimaorder(arima_CO)[3]), arima_CO$aic, arima_CO$bic)

dat[6, ] <- c(k[6],paste("ARIMA",arimaorder(arima_SO2)[1],arimaorder(arima_SO2)[2],
                         arimaorder(arima_SO2)[3]), arima_SO2$aic, arima_SO2$bic)

dat[7, ] <- c(k[7],paste("ARIMA",arimaorder(arima_NH3)[1],arimaorder(arima_NH3)[2],
                         arimaorder(arima_NH3)[3]), arima_NH3$aic, arima_NH3$bic)

dat[8, ] <- c(k[8],paste("ARIMA",arimaorder(arima_Ozone)[1],arimaorder(arima_Ozone)[2],
                         arimaorder(arima_Ozone)[3]), arima_Ozone$aic, arima_Ozone$bic)

dat[9, ] <- c(k[9],paste("ARIMA",arimaorder(arima_Benzene)[1],arimaorder(arima_Benzene)[2],
                         arimaorder(arima_Benzene)[3]), arima_Benzene$aic, arima_Benzene$bic)

dat[10, ] <- c(k[10],paste("ARIMA",arimaorder(arima_pm2.5)[1],arimaorder(arima_pm2.5)[2],
                         arimaorder(arima_pm2.5)[3]), arima_pm2.5$aic, arima_pm2.5$bic)

colnames(dat) <- c("Variable","Modeled Process","AIC","BIC")


# Obtain forecasts from the ARIMA model
forecast_pm <- forecast(arima_pm, h = 10)
forecast_NO <- forecast(arima_NO, h = 10)
forecast_NO2 <- forecast(arima_NO2, h = 10)
forecast_NOX <- forecast(arima_NOX, h = 10)
forecast_CO <- forecast(arima_CO, h = 10)
forecast_SO2 <- forecast(arima_SO2, h = 10)
forecast_NH3 <- forecast(arima_NH3, h = 10)
forecast_Ozone <- forecast(arima_Ozone, h = 10)
forecast_Benzene <- forecast(arima_Benzene, h = 10)
###############################################################################################
###############################################################################################
###############################################################################################




# Interpolate NA values using Linear interpolation
idata_ <- na_interpolation(data, option = "linear", method = "row")
traceplot(as.matrix(idata[,4:13]))
idata_ <- na_interpolation(data, option = "linear", method = "column")
traceplot(as.matrix(idata[,4:13]))

acfplot(as.matrix(idata[,4:13]), lag.max = 5000)
# Interpolate NA values using spline interpolation
spline_interpolation <- na_interpolation(data, opyion = "spline")
traceplot(as.matrix(spline_interpolation[,4:13]))
# Interpolate NA values using Stineman interpolation
stinmn_interpolation <- na_interpolation(data, method = "stine")
traceplot(as.matrix(stinmn_interpolation[,4:13]))
# Compare the interpolated results for pm2.5
pm10 <- idata_$PM10..µg.m3.
pm2.5 <- idata_$PM2.5..µg.m3.
NO <- idata_$NO..µg.m3.
NO2 <- idata_$NO2..µg.m3.
NOX <- idata_$NOX..ppb.
CO <- idata_$CO..mg.m3.
SO2 <- idata_$SO2..µg.m3.
NH3 <- idata_$NH3..µg.m3.
Ozone <- idata_$Ozone..µg.m3.
Benzene <- idata_$Benzene..µg.m3.


# Fit an ARIMA model for each column separately
arima_pm10 <- auto.arima(pm10)
arima_NO <- auto.arima(NO)
arima_NO2 <- auto.arima(NO2)
arima_NOX <- auto.arima(NOX)
arima_CO <- auto.arima(CO)
arima_SO2 <- auto.arima(SO2)
arima_NH3 <- auto.arima(NH3)
arima_Ozone <- auto.arima(Ozone)
arima_Benzene <- auto.arima(Benzene)
arima_pm2.5 <- auto.arima(pm2.5)

k = colnames(data)[4:13]
idat <- matrix(0,nrow= 10, ncol =4)
idat[1, ] <- c(k[1],paste("ARIMA",arimaorder(arima_pm10)[1],arimaorder(arima_pm10)[2],
                         arimaorder(arima_pm10)[3]), arima_pm10$aic, arima_pm10$bic)

idat[2, ] <- c(k[2],paste("ARIMA",arimaorder(arima_NO)[1],arimaorder(arima_NO)[2],
                         arimaorder(arima_NO)[3]), arima_NO$aic, arima_NO$bic)

idat[3, ] <- c(k[3],paste("ARIMA",arimaorder(arima_NO2)[1],arimaorder(arima_NO2)[2],
                         arimaorder(arima_NO2)[3]), arima_NO2$aic, arima_NO2$bic)

idat[4, ] <- c(k[4],paste("ARIMA",arimaorder(arima_NOX)[1],arimaorder(arima_NOX)[2],
                         arimaorder(arima_NOX)[3]), arima_NOX$aic, arima_NOX$bic)

idat[5, ] <- c(k[5],paste("ARIMA",arimaorder(arima_CO)[1],arimaorder(arima_CO)[2],
                         arimaorder(arima_CO)[3]), arima_CO$aic, arima_CO$bic)

idat[6, ] <- c(k[6],paste("ARIMA",arimaorder(arima_SO2)[1],arimaorder(arima_SO2)[2],
                         arimaorder(arima_SO2)[3]), arima_SO2$aic, arima_SO2$bic)

idat[7, ] <- c(k[7],paste("ARIMA",arimaorder(arima_NH3)[1],arimaorder(arima_NH3)[2],
                         arimaorder(arima_NH3)[3]), arima_NH3$aic, arima_NH3$bic)

idat[8, ] <- c(k[8],paste("ARIMA",arimaorder(arima_Ozone)[1],arimaorder(arima_Ozone)[2],
                         arimaorder(arima_Ozone)[3]), arima_Ozone$aic, arima_Ozone$bic)

idat[9, ] <- c(k[9],paste("ARIMA",arimaorder(arima_Benzene)[1],arimaorder(arima_Benzene)[2],
                         arimaorder(arima_Benzene)[3]), arima_Benzene$aic, arima_Benzene$bic)

idat[10, ] <- c(k[10],paste("ARIMA",arimaorder(arima_pm2.5)[1],arimaorder(arima_pm2.5)[2],
                           arimaorder(arima_pm2.5)[3]), arima_pm2.5$aic, arima_pm2.5$bic)

colnames(idat) <- c("Variable","Modeled Process","AIC","BIC")

# Plot the result
# Number of period we want to forecast
n <- 100
residuals <- residuals(arima_NO2)
acf(residuals,lag.max = 1000, main="ACF Plot of Residuals of ARIMA model of NO2")
pacf(residuals,lag.max = 1000, main="PACF Plot of Residuals of ARIMA model of NO2")

# Splitting the data
train <- head(Cl(pm2.5), length(Cl(pm2.5))-n)
test <- tail(Cl(pm2.5), n)
autoplot(arima_pm2.5) 

# View the interpolated data
print(interpolated_data)
data_0 <- data_imputed
pm10 <- data_0$PM10..µg.m3.
pm2.5 <- data_0$PM2.5..µg.m3.
NO <- data_0$NO..µg.m3.
NO2 <- data_0$NO2..µg.m3.
NOX <- data_0$NOX..ppb.
CO <- data_0$CO..mg.m3.
SO2 <- data_0$SO2..µg.m3.
NH3 <- data_0$NH3..µg.m3.
Ozone <- data_0$Ozone..µg.m3.
Benzene <- data_0$Benzene..µg.m3.
############################################################################################
#############################################################################################
############################################################################################
#############################################################################################


# Handle missimg values with multiple imputation

imp_model <- mice(data[,c(-1,-2,-3)], method = "pmm")
data_imputed <- complete(imp_model)
data_0 = data_imputed
# Extract all the columns separately
pm10 <- data_0$PM10..µg.m3.
pm2.5 <- data_0$PM2.5..µg.m3.
NO <- data_0$NO..µg.m3.
NO2 <- data_0$NO2..µg.m3.
NOX <- data_0$NOX..ppb.
CO <- data_0$CO..mg.m3.
SO2 <- data_0$SO2..µg.m3.
NH3 <- data_0$NH3..µg.m3.
Ozone <- data_0$Ozone..µg.m3.
Benzene <- data_0$Benzene..µg.m3.


# Fit an ARIMA model for each column separately

arima_pm10 <- auto.arima(pm10)
arima_pm2.5 <- auto.arima(pm2.5)
arima_NO <- auto.arima(NO)
arima_NO2 <- auto.arima(NO2)
arima_NOX <- auto.arima(NOX)
arima_CO <- auto.arima(CO)
arima_SO2 <- auto.arima(SO2)
arima_NH3 <- auto.arima(NH3)
arima_Ozone <- auto.arima(Ozone)
arima_Benzene <- auto.arima(Benzene)

k = colnames(data)[4:13]
imdat <- matrix(0,nrow= 10, ncol =4)
imdat[1, ] <- c(k[1],paste("ARIMA",arimaorder(arima_pm10)[1],arimaorder(arima_pm10)[2],
                          arimaorder(arima_pm10)[3]), arima_pm10$aic, arima_pm10$bic)

imdat[2, ] <- c(k[2],paste("ARIMA",arimaorder(arima_NO)[1],arimaorder(arima_NO)[2],
                          arimaorder(arima_NO)[3]), arima_NO$aic, arima_NO$bic)

imdat[3, ] <- c(k[3],paste("ARIMA",arimaorder(arima_NO2)[1],arimaorder(arima_NO2)[2],
                          arimaorder(arima_NO2)[3]), arima_NO2$aic, arima_NO2$bic)

imdat[4, ] <- c(k[4],paste("ARIMA",arimaorder(arima_NOX)[1],arimaorder(arima_NOX)[2],
                          arimaorder(arima_NOX)[3]), arima_NOX$aic, arima_NOX$bic)

imdat[5, ] <- c(k[5],paste("ARIMA",arimaorder(arima_CO)[1],arimaorder(arima_CO)[2],
                          arimaorder(arima_CO)[3]), arima_CO$aic, arima_CO$bic)

imdat[6, ] <- c(k[6],paste("ARIMA",arimaorder(arima_SO2)[1],arimaorder(arima_SO2)[2],
                          arimaorder(arima_SO2)[3]), arima_SO2$aic, arima_SO2$bic)

imdat[7, ] <- c(k[7],paste("ARIMA",arimaorder(arima_NH3)[1],arimaorder(arima_NH3)[2],
                          arimaorder(arima_NH3)[3]), arima_NH3$aic, arima_NH3$bic)

imdat[8, ] <- c(k[8],paste("ARIMA",arimaorder(arima_Ozone)[1],arimaorder(arima_Ozone)[2],
                          arimaorder(arima_Ozone)[3]), arima_Ozone$aic, arima_Ozone$bic)

imdat[9, ] <- c(k[9],paste("ARIMA",arimaorder(arima_Benzene)[1],arimaorder(arima_Benzene)[2],
                          arimaorder(arima_Benzene)[3]), arima_Benzene$aic, arima_Benzene$bic)

imdat[10, ] <- c(k[10],paste("ARIMA",arimaorder(arima_pm2.5)[1],arimaorder(arima_pm2.5)[2],
                            arimaorder(arima_pm2.5)[3]), arima_pm2.5$aic, arima_pm2.5$bic)

colnames(imdat) <- c("Variable","Modeled Process","AIC","BIC")
traceplot(as.matrix(data_0),fast = F, legend = F, 
          main = "Time Series plots of Multivariate Time Series Data",col = "palegreen4")

# Obtain forecasts from the ARIMA model

forecast_pm10 <- forecast(arima_pm10, h = 10)
forecast_pm2.5 <- forecast(arima_pm2.5, h = 10)
forecast_NO <- forecast(arima_NO, h = 10)
forecast_NO2 <- forecast(arima_NO2, h = 10)
forecast_NOX <- forecast(arima_NOX, h = 10)
forecast_CO <- forecast(arima_CO, h = 10)
forecast_SO2 <- forecast(arima_SO2, h = 10)
forecast_NH3 <- forecast(arima_NH3, h = 10)
forecast_Ozone <- forecast(arima_Ozone, h = 10)
forecast_Benzene <- forecast(arima_Benzene, h = 10)

forecast <- cbind(forecast_pm10,forecast_pm2.5,forecast_NO,forecast_NO2,forecast_NOX,forecast_CO,forecast_SO2, forecast_NH3, forecast_Ozone, forecast_Benzene)


##Statistical Inference through Descriptive Statistics
date_t = matrix(0, nrow = 360, ncol = 13)
i = 56
j = 1
data_ex <- cbind(data[,1:3],data_0)
data_ex <- as.matrix(data_ex)
while(i <= 8640)
{
  date_t[j, ] <-   data_ex[i, ]
  date_t[j+1, ] <- data_ex[i+1, ]
  date_t[j+2, ] <- data_ex[i+2, ]
  date_t[j+3, ] <- data_ex[i+3, ]
  i = i + 96
  j = j + 4
}
data_t <- matrix(0, ncol = 10, nrow = 360)
for(i in 4:13)
{
  data_t[,i-3] = as.numeric(date_t[,i])
}
univdate_t <- cbind(date_t[,1:3],apply(as.matrix(normalized_weights*data_t),1,sum) )

traceplot(as.matrix(date_t[,4:13]))
plot.ts(date_t[,4:13], col = "blue")
hist(as.numeric(date_t[,4]))


library(randomForest)  # Example using the randomForest package

# Train a random forest model
model <- randomForest(data_0)

# Calculate feature importance
importance <- importance(model)
importance <- as.vector(importance)
# Extract the mean decrease in accuracy (MDA) values for each gas
gas_weights <- importance

# Normalize the weights to sum up to 1
normalized_weights <- gas_weights / sum(gas_weights)

# Print the derived weights
print(normalized_weights)
univariate_data <- cbind(data[,1:3],apply(as.matrix(normalized_weights*data_0),1,sum))
colnames(univariate_data)[4] <- "Pollution Index"
hist(univariate_data[,4])
unidate_t <- cbind(date_t[,1:3],apply(as.matrix(normalized_weights*as.numeric(date_t[,4:13])),1,sum))

