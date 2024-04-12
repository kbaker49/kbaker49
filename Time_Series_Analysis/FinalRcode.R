library(ggplot2)
library(dplyr)



###########################
## Data set, trend removal
###########################
# Create the time series plot
TSAFinalProject = read.csv("C:/Users/coler/Downloads/TSAFinalProject.csv",
                           header = TRUE)
data = TSAFinalProject %>% rename("CPI" = "CUUR0000SETA02")
data = data %>% rename("date" = "DATE")
data$date = as.Date(data$date)
data = data[data$date > as.Date("1969-12-01"),]
data$CPI = as.numeric(data$CPI)

# Check for normality, stationarity
plot(data, type = 'l')
hist(data$CPI)
qqnorm(data$CPI)
qqline(data$CPI)
shapiro.test(data$CPI) # Very much not normal

# Perform standardization and normalization
library(caret)
library(MASS)
library(forecast)
library(tseries)
bc<-boxcox(lm(data$CPI ~ data$date))
lambda <- BoxCox.lambda(data)
print(lambda)
# Fit new linear regression model using the Box-Cox transformation
bcData <- (BoxCox(data$CPI, lambda = lambda)) 

# Check for normality after Box-Cox
plot(bcData)
hist(bcData)
qqnorm(bcData)
qqline(bcData)
shapiro.test(bcData)
# Doesn't change much, don't include in the model

# Check stationarity of the raw data
library(tseries)
adf.test(data$CPI)
pp.test(data$CPI)
kpss.test(data$CPI) # Very much not stationary

# Transform the data into a time series
CPI = ts(data = data$CPI, start = c(1970, 1), frequency = 12)
# Take first difference to remove stochastic trend
diffCPI = ts(data = diff(CPI), start = c(1970, 2), frequency = 12)
plot(diffCPI) # Exhibits heteroscedasticity
abline(h = 0)

# Try out the first difference of the logged data
logCPI = ts(data = log(data$CPI), start = c(1970, 1), frequency = 12)
logDiff = ts(data = diff(logCPI), start = c(1970, 2), frequency = 12)
plot(logDiff) # Helps a bit with homoscedasticity, but not enough
abline(h = 0)

# Diagnostics for first difference
hist(diffCPI)         # Appears more normal
qqnorm(diffCPI)
qqline(diffCPI)       # Heavy-tailed, but more normal
shapiro.test(diffCPI) # Rejects null hypothesis of normality
library(TSA)
runs(diffCPI)         # Rejects null hypothesis of independence

# Diagnostics for the logged data
hist(logDiff)
qqnorm(logDiff)
qqline(logDiff)
shapiro.test(logDiff) # Not significantly better, so don't use



###########################
## Model specification
###########################
# d
library(tseries)
adf.test(diffCPI)
pp.test(diffCPI)
kpss.test(diffCPI) # Stationary now
# Check for log data, though we won't use it further
adf.test(logDiff)
pp.test(logDiff)
kpss.test(logDiff)

# p & q
# ACF and PACF
library(forecast)
par(mfrow = c(1, 1)) # Seasonality suggests SARIMA
acf(diffCPI, lag.max = 60) # MA(2)
pacf(diffCPI, lag.max = 60) # AR(4)
library(TSA)
eacf(diffCPI) # MA(2), ARMA(4, 1), ARMA(1, 3)

library(forecast)
auto.arima(diffCPI, d = 0) # ARIMA(4,0,0)(2,0,0), SAR(2)

library(TSA)
res = armasubsets(y = diffCPI, nar = 14, nma = 14,
                  y.name = 'test', ar.method = 'ols')

# Default is BIC
par(mfrow = c(1, 1))
plot(res) # ARMA(1, 3) simplest, ARMA(4, 3)
plot(res, scale = 'AIC')
plot(res, scale = 'AICc')

# Candidate models
sar42 = Arima(diffCPI,order = c(4,0,0), seasonal = list(order = c(2,0,0)))
sma22 = Arima(diffCPI,order = c(0,0,2), seasonal = list(order = c(2,0,0)))
sarima132 = Arima(diffCPI,order = c(1,0,3), seasonal = list(order = c(2,0,0)))
sarima412 = Arima(diffCPI,order = c(4,0,1), seasonal = list(order = c(2,0,0)))
sarima432 = Arima(diffCPI,order = c(4,0,3), seasonal = list(order = c(2,0,0)))

# Compare the models
sar42 # Best in terms of all metrics
sma22
sarima132
sarima412
sarima432

# Chosen SARIMA model
sarima = Arima(diffCPI, order = c(4,0,0), seasonal = list(order = c(2,0,0)))
tsdiag(sarima)



###########################
## Forecasting
###########################
library(forecast)
library(tseries)
library(TSA)
predx = predict(sarima, n.ahead = 36)
predx$pred[1] = predx$pred[1] + 183.241
for (x in 2:36) {
  predx$pred[x] = predx$pred[x] + predx$pred[x - 1]
}
pr = predx$pred
uci = pr + 2 * predx$se
lci = pr - 2 * predx$se

# Plot of stationary portion of the data + forecast
ymin = min(c(as.vector(lci), diffCPI)) - 1
ymax = max(c(as.vector(uci), diffCPI)) + 1

pr <- data.frame(date=zoo::as.Date(time(pr)),CPI=as.matrix(pr))
uci <- data.frame(date=zoo::as.Date(time(uci)), CPI=as.matrix(uci))
lci <- data.frame(date=zoo::as.Date(time(lci)), CPI=as.matrix(lci))

plot(data, type = 'l', ylim = c(ymin, ymax), xlim = c(as.Date("1970-12-01"),as.Date("2030-12-01")), main = "SARIMA(4,1,0)(2,0,0)")
lines(pr, col = 2)
lines(uci, col = 3)
lines(lci, col = 3)

plot(data, type = 'l', ylim = c(100, 240), xlim = c(as.Date("2019-12-01"),as.Date("2030-12-01")), main = "SARIMA(4,1,0)(2,0,0)")
lines(pr, col = 2)
lines(uci, col = 3)
lines(lci, col = 3)

