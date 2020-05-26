#-------------------------------------------------------------------------------------#
# Written by Geoff Shuetrim
# Lecture 05: Stationary time series
#
# Covers:
#
# - Stationarity examples
#
# - Holt Winters models
#
# - ARMA models
#
# - Structural time series models
#
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Stationary series - White noise.
#-------------------------------------------------------------------------------------

plot.ts(rnorm(50, mean=10, sd=1), main="White noise, E(X)=10, Var(X)=1")

#-------------------------------------------------------------------------------------
# Stationary series - Covariance of Xt and Xt-1 = 0.8
#-------------------------------------------------------------------------------------

plot.ts(arima.sim(list(order = c(1,0,0), ar = 0.75), n = 100, mean=10, sd = 1), main="Mean reverting series")

#-------------------------------------------------------------------------------------
# Stationary series - MA(2) model 
#-------------------------------------------------------------------------------------

x = arima.sim(list(order = c(0,0,2),  ma = c(0.5, 0.25)), n = 1000, mean=0, sd = 1)
plot.ts(x, main="MA(2)")
acf (x, lag = 10)

#-------------------------------------------------------------------------------------
# Stationary series - AR(2) model 
#-------------------------------------------------------------------------------------

x = arima.sim(list(order = c(2,0,0),  ar = c(0.7, -0.5)), n = 1000, mean=0, sd = 1)
plot.ts(x, main="AR(2)")
acf (x, lag = 10)

#-------------------------------------------------------------------------------------
# Stationary series - ARMA(1,2) model 
#-------------------------------------------------------------------------------------

x = arima.sim(list(order = c(1,0,2),  ar = c(0.8), ma= c(0,-0.8)), n = 100, mean=0, sd = 1)
plot.ts(x, main="ARMA(1,2)")
acf (x, lag = 10)


#-------------------------------------------------------------------------------------
# Impulse response function: ARMA(1,2) model 
#-------------------------------------------------------------------------------------

# install.packages("TSSS")
library("TSSS")
armaimp(arcoef = c(0.8), macoef = c(0,-0.8), v=1, n=1000, lag=NULL, nf=200)


#-------------------------------------------------------------------------------------
# Estimation
#-------------------------------------------------------------------------------------
# See https://smac-group.github.io/ts/introtimeseries.html
#install.packages(c("devtools", "astsa", "mgcv"))
#devtools::install_github("SMAC-Group/simts")
library(astsa)
library(mgcv)
library(simts)

x = arima.sim(list(order = c(2,0,0),  ar = c(0.7, -0.5)), n = 100, mean=0, sd = 1)
plot.ts(x, main="AR(2)")
par(mfrow=c(2,1))
acf (x, lag = 10)
pacf( x, lag = 10)
par(mfrow=c(1,1))

model = estimate(AR(2), x, demean = FALSE, method="rgmwm") # mle or yule-walker
summary(model)
check(model = model)


#-------------------------------------------------------------------------------------
# Holt Winters with additive seasonality 
#-------------------------------------------------------------------------------------

m = 12
years = 10
n = years * m
trend.slope = 1
seasonals = rnorm(m, mean=1, sd=10)

simulated.data <- as.data.frame(matrix(0, ncol = 4, nrow = n))
colnames(simulated.data) = c("observed", "level", "slope", "seasonal")

observation.deviations = arima.sim(list(order = c(1,0,0), ar = 0.2),n = n, sd = 0.1)
slope.deviations = arima.sim(list(order = c(1,0,0), ar = 0.2),n = n, sd = 1)
seasonal.deviations = arima.sim(list(order = c(1,0,0), ar = 0.2),n = n, sd = 0.1)

for (t in 1:n) {
  if (t <=m ) {
    simulated.data[t,] = 0.0
    simulated.data[t,"level"] = 100.0
  } else {
    simulated.data[t,"slope"] = trend.slope + slope.deviations[t]
    simulated.data[t,"level"] = simulated.data[t-1,"level"] + simulated.data[t,"slope"]
    simulated.data[t,"seasonal"] = seasonals[t %% m+1] + seasonal.deviations[t]
    simulated.data[t,"observed"] = simulated.data[t,"level"] + simulated.data[t,"seasonal"] + observation.deviations[t]
  }
}

trimmed.data <- na.omit(simulated.data[m+1:n,])

par(mfrow=c(2,2))
plot.ts(trimmed.data$observed, main="Observed")
plot.ts(trimmed.data$level, main="Level")
plot.ts(trimmed.data$slope, main="Slope")
plot.ts(trimmed.data$seasonal, main="Seasonal")

#install.packages("TTR")
library("TTR")
time.series = ts(trimmed.data$observed, start = c(1990, 1), frequency = m)
forecasts <- HoltWinters(time.series, seasonal = "additive")
par(mfrow=c(1,1))
plot(forecasts, main = "Additive seasonal")


#-------------------------------------------------------------------------------------
# Holt Winters with multiplicative seasonality 
#-------------------------------------------------------------------------------------

m = 12
years = 40
n = years * m
trend.slope = 1
seasonals = rnorm(m, mean=1, sd=0.05)

simulated.data <- as.data.frame(matrix(0, ncol = 4, nrow = n))
colnames(simulated.data) = c("observed", "level", "slope", "seasonal")

observation.deviations = arima.sim(list(order = c(1,0,0), ar = 0.2),n = n, sd = 0.1)
slope.deviations = arima.sim(list(order = c(1,0,0), ar = 0.2),n = n, sd = 2)
seasonal.deviations = arima.sim(list(order = c(1,0,0), ar = 0.2),n = n, sd = 0.1)

for (t in 1:n) {
  if (t <=m ) {
    simulated.data[t,] = 0.0
    simulated.data[t,"level"] = 100.0
  } else {
    simulated.data[t,"slope"] = trend.slope + slope.deviations[t]
    simulated.data[t,"level"] = simulated.data[t-1,"level"] + simulated.data[t,"slope"]
    simulated.data[t,"seasonal"] = seasonals[t %% m+1] + seasonal.deviations[t]
    simulated.data[t,"observed"] = simulated.data[t,"level"] * simulated.data[t,"seasonal"] + observation.deviations[t]
  }
}

trimmed.data <- na.omit(simulated.data[m+1:n,])

par(mfrow=c(2,2))
plot.ts(trimmed.data$observed, main="Observed")
plot.ts(trimmed.data$level, main="Level")
plot.ts(trimmed.data$slope, main="Slope")
plot.ts(trimmed.data$seasonal, main="Seasonal")

#install.packages("TTR")
library("TTR")
time.series = ts(trimmed.data$observed, start = c(1990, 1), frequency = m)
forecasts <- HoltWinters(time.series, seasonal = "multiplicative")
par(mfrow=c(1,1))
plot(forecasts, main = "Multiplicative seasonal")
