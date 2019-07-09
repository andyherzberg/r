##########################################################
# AirPassenger TimeSeries Analysis
# Andy Herzberg
##########################################################

library(tseries)
library(forecast)

data("AirPassengers")

# Plot complete time series
plot(AirPassengers)

# Decompose series
decomp = stl(AirPassengers, s.window = "periodic")
deseasonal_cnt = seasadj(decomp)
plot(decomp)

auto.arima(AirPassengers, max.order=5, stepwise=TRUE)

par(mfrow=c(1,2))
acf(AirPassengers)
pacf(AirPassengers)

       # ACF                    # PCF
# AR   # Geometric              # Significant til p lags  
# MA   # Significant til p lags # Geometric
# ARMA # Geometric              # Geometric 

# Differncing to remove trend and make series stationary
AirPassengers.d1 <- diff(AirPassengers)

# Inspect ACF + PACF
acf(AirPassengers.d1)
pacf(AirPassengers.d1)

# MA: q = 2
# AR: p = 1
# I: d = 1

# Gefunden in der R-Hilfe: ?Arima
air.model_111 = Arima(AirPassengers, 
      order = c(1, 1, 1), 
      seasonal=list(order=c(1,1,1), period=12),
      lambda = 0)

# Inspect ACF, PACF and residuals
tsdisplay(residuals(air.model_111), lag.max = 45, main='(1,1,1) Model ')

air.model_011 = Arima(AirPassengers, 
                  order = c(0, 1, 1), 
                  seasonal=list(order=c(0,1,1), period=12),
                  lambda = 0)

# Inspect ACF, PACF and residuals
tsdisplay(residuals(air.model_011), lag.max = 45, main='(0,1,1) Model ')

################################################################

# Compare forecast with actual data

# Fit model to first few years of AirPassengers data
air.model_111 <- Arima(window(AirPassengers,end=1956+11/12),
                   order=c(1,1,1),
                   seasonal=list(order=c(1,1,1),period=12),
                   lambda=0)


# Fit model to first few years of AirPassengers data
air.model_011 <- Arima(window(AirPassengers,end=1956+11/12),
                       order=c(0,1,1),
                       seasonal=list(order=c(0,1,1),period=12),
                       lambda=0)


# Plot prediction
par(mfrow=c(1,2))
plot(forecast(air.model_111,h=48))
lines(AirPassengers, col="red") 
plot(forecast(air.model_011,h=48))
lines(AirPassengers, col="red")  

