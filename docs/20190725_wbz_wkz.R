###################################################
# Time Series Analysis
# Andy Herzberg
# C:/Users/herzb/Documents/WBZ_WKZ.csv data set
# https://www.datascienceblog.net/post/machine-learning/forecasting-an-introduction/
###################################################

library (tseries)
library(forecast)

# Load csv
df <- read.csv("/Users/herzb/Downloads/WBZ_WKZ.csv")

nrow(df)

df <- df[3:nrow(df)-1,] # erste und letzte Zeile weglassen, da (möglicherweise) unvollständig

nrow(df)

par(mfrow=c(3,1))
# Plot
plot(df$'Sum.EINWEISUNGJAHR.' ,type = "o", col = "red", xlab = "time", ylab = "Sum.EINWEISUNGJAHR.", main = "Sum.EINWEISUNGJAHR.")

# Create time series
wbz_wkz.ts <- ts(df$Sum.EINWEISUNGJAHR., start = c(2010, 1), end = c(2019, 2), frequency = 4) # log() damit keine negativen Werte vorhergesagt werden

# Plot ts wbz_wkz.ts
plot(wbz_wkz.ts)

# Create time series
wbz_wkz.ts <- ts(log(df$Sum.EINWEISUNGJAHR.), start = c(2010, 1), end = c(2019, 2), frequency = 4) # log() damit keine negativen Werte vorhergesagt werden

# Plot log of ts wbz_wkz.ts
plot(wbz_wkz.ts)

# Plot decomposition
plot(decompose(wbz_wkz.ts, type = "multiplicative"))

# Augmented Dickey-Fuller Test
adf.test(wbz_wkz.ts) # nicht stationär, negativer trend
# p > 0, and the process is not stationary.
# p = 0, the null hypothesis is rejected, and the process is considered to be stationary.

# Auto correlation function
acf((wbz_wkz.ts)) # no diff()
pacf((wbz_wkz.ts))
par(mfrow = c(1,2))
acf(diff(wbz_wkz.ts)) # MA=0 aka p
# Partial auto correlation function
pacf(diff(wbz_wkz.ts)) # AR=3 aka q
# I=1 aka d

# Custom Arima
fit.custom.arima <- arima(wbz_wkz.ts, order = c(0, 1, 0)) # , seasonal = list(order = c(0, 0, 0), period = 4 # wie genau ist der seasonal parameter zu verstehen?
fit.custom.arima$aic # soll minimal sein
exp(forecast(fit.custom.arima,h=8)$mean)
plot(exp(forecast(fit.custom.arima,h=8)$mean))

# Auto Arima
auto.arima(wbz_wkz.ts)
fit.auto.arima <- auto.arima(wbz_wkz.ts)
fit.auto.arima$aic # soll minimal sein
plot(exp(forecast(fit.auto.arima,h=8)$mean))

# Check modelm statistics
summary(fit.auto.arima)

# Aufgrund von diff() ist die ts lediglich um 1 verschoben
plot(exp(wbz_wkz.ts))
lines(exp(fitted(fit.auto.arima)), col='red')

# predict() und forecast() liefern die gleichen Vorhersagen
predict(fit.auto.arima, n.ahead=8)$pred
forecast(fit.auto.arima, h=8)$mean

# Create view for real and forecasted values
#lag(pred, k=1) # lag pred by 1
combined_values <- cbind(exp(wbz_wkz.ts), fitted = lag(exp(fitted(fit.auto.arima)), k=1), exp(forecast(fit.auto.arima, h=8)$mean)) #, resid = exp(resid(fit.custom.arima))
combined_values

######### Fehler berechnen!?!
xreg = cbind(df$First..HEFT_BETRAG., df$First..JAHRESABO_WKZ_BETRAG.) #, df$First..SPRUNGHAFTUNG.

# Auto Arima
auto.arima(wbz_wkz.ts, xreg = xreg)
fit.auto.arima <- auto.arima(wbz_wkz.ts)
fit.auto.arima$aic # soll minimal sein
plot(exp(forecast(fit.auto.arima,h=8)$mean))

# Check residuals
checkresiduals(fit.auto.arima)

# Nochmal monatlich versuchen!
# Vielleicht kommt da was raus!

# How to interpret ARIMA(0,1,0)?
# ARIMA(0,0,0) is just white noise
# ARIMA(0,1,0) is random walk
