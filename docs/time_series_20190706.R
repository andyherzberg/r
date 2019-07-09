###################################################
# Time Series Analysis 
# Andy Herzberg
# Simple shampoo.csv data set
###################################################

# Load csv
df <- read.csv("C:/Users/herzb/Documents/knime/time_series/shampoo.csv")

plot(df)

# Convert to time series
my.ts =ts(df$Sales, start = 0, freq = 12)

# Plot time series
plot(my.ts)

# Plot summary statistics
summary(my.ts)

# Plot histogram of my.ts
hist(my.ts)

# PLot aggregated my.ts
plot(aggregate(my.ts,FUN=mean))

# PLot variance over cycles
boxplot(my.ts~cycle(my.ts))

# Plots wth normal my.ts, diff() and log() of my.ts
par(mfrow=c(1,4))
plot(my.ts)
# Remove trends
plot(diff(my.ts))
# Stabilize the variance
plot(log(my.ts))
# Do both
plot(diff(log(my.ts)))

# install.packages("forecast")  
# Load package tseries
library(forecast)
library(tseries)
# Augmented Dickey Fuller Test for stationarity
adf.test(my.ts, alternative="stationary", k=0)
adf.test(diff(my.ts), alternative="stationary", k=0)
adf.test(diff(log(my.ts)), alternative="stationary", k=0)
# => Stationary (means "trend stationary") result, because trend is automatically added in this test

# KPSS Test
kpss.test(my.ts, null = "Trend")
# How to interpret KPSS results?
# https://stats.stackexchange.com/questions/13213/how-to-interpret-kpss-results

# Inspecting for trend
plot(my.ts)
abline(reg=lm(my.ts~time(my.ts)))
plot(diff(log(my.ts)))
abline(reg=lm(diff(log(my.ts))~time(diff(my.ts))))

# Decompose my.ts
components.ts = decompose(my.ts)
plot(components.ts)

       # ACF                    # PCF
# AR   # Geometric              # Significant til p lags  
# MA   # Significant til p lags # Geometric
# ARMA # Geometric              # Geometric 

par(mfrow=c(2,1))
acf(diff(log(my.ts)))
# AR: For an AR series this correlation will gradually go down without any cut off value
# MA: For a MA series the total correlation chart cuts off at n-th lag.

# => AR(1) => p
# => I(1) => d
# => MA(2) =>q

pacf(diff(log(my.ts)))
# Clearly, the graph above has a cut off on PACF curve after 1st lag.
# This means this is mostly an AR(1) process.

# Fit model to my.ts data
fit <- arima(my.ts, order = c(1, 1, 2), seasonal = list(order = c(0, 1, 0), pe1iod = 12)) # wie genau ist der seasonal parameter zu verstehen?

fit$aic # soll minimal sein

plot(forecast(fit, h=12))

forecast.ts <- forecast(object = my.ts, h = 24)

forecast.ts[10]$residuals

# install.packages("HH")
library(HH)
library(tseries)

plot(forecast.ts$'mean')

seqplot.ts(forecast.ts$x, forecast.ts$'mean')

pred <- predict(fit, n.ahead = 2*12)

ts.plot(my.ts, 2.718^pred$pred, log = "y", lty = c(1,3))

auto.arima(my.ts)
fit <- auto.arima(my.ts)
fit$aic # soll minimal sein
plot(forecast(fit,h=12))


# Let's take u want to predict the value of a series at the next time instant. 
# That means you're intersted in finding one step ahead prediction value of a given series. 
# When you compare this predicted value with the observed one whatever difference you get is called Residual. 
plot(residuals(fit))