#-------------------------------------------------------------------------------------#
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 08: ARMA models
#
# Covers:
#
# - ARMA models:
#   simulation
#   identification
#   estimation
#   forecasting
#   impulse response functions
#
#-------------------------------------------------------------------------------------

# Set number of observations in each simulation
n = 1000

#-------------------------------------------------------------------------------------
# Packages
#-------------------------------------------------------------------------------------

# install.packages("forecast")
library("forecast")

#-------------------------------------------------------------------------------------
# White noise.
#-------------------------------------------------------------------------------------

help(arima.sim)
x <- arima.sim(list(order = c(0,0,0)), n = n, mean=0, sd = 1)
# Alternative to:
# x <- rnorm(n, mean=0, sd=1)
sd(x)
plot.ts(x, main="White noise")
acf (x, lag = 10)
pacf (x, lag = 10)

#-------------------------------------------------------------------------------------
# Random walk
#-------------------------------------------------------------------------------------
x <- arima.sim(list(order = c(0,1,0)), n = n, mean=0, sd = 1)
sd(x) # How does var(x) change as n increases?
plot.ts(x, main="Random walk")
acf (x, lag = 10)
pacf (x, lag = 10)

#-------------------------------------------------------------------------------------
# Causal AR(1)
#-------------------------------------------------------------------------------------
x <- arima.sim(list(order = c(1,0,0), ar = 0.75), n = n, mean=10, sd = 1)
sd(x)
plot.ts(x, main="Mean reverting series")
plot.ts(x, main="Causal AR(1)")
acf (x, lag = 10)
pacf (x, lag = 10)

#-------------------------------------------------------------------------------------
# Invertible MA(2) model 
#-------------------------------------------------------------------------------------
x = arima.sim(list(order = c(0,0,2),  ma = c(0.5, 0.5)), n = n, mean=0, sd = 1)
plot.ts(x, main="MA(2)")
sd(x)
acf (x, lag = 10)
pacf (x, lag = 10)

#Fit the model (extra () around the assignment print out "fit" also)
help("Arima") # Part of forecast package
(fit <- Arima(x, order=c(0,0,2), include.mean = FALSE ))

# Get model diagnostics
help("checkresiduals")
checkresiduals(fit)

# Perform forecasting
(forecasts <- forecast(fit, h = 10, level = c(25, 50, 75, 90)))
autoplot(forecasts)

#-------------------------------------------------------------------------------------
# Causal AR(2) model 
#-------------------------------------------------------------------------------------
x = arima.sim(list(order = c(2,0,0),  ar = c(0.7, -0.5)), n = n, mean=0, sd = 1)
plot.ts(x, main="AR(2)")
acf (x, lag = 10)
pacf (x, lag = 10)

#-------------------------------------------------------------------------------------
# Causal and invertible ARMA(1,2) 
#-------------------------------------------------------------------------------------

x = arima.sim(list(order = c(1,0,2),  ar = c(0.8), ma= c(0,-0.8)), n = n, mean=0, sd = 1)
var(x)
plot.ts(x, main="ARMA(1,2)")
acf (x, lag = 10)
pacf (x, lag = 10)

#-------------------------------------------------------------------------------------
# Impulse response function: ARMA(1,2) model 
#-------------------------------------------------------------------------------------
help(armaimp)
armaimp(arcoef = c(0.8), macoef = c(0,-0.8), v=1, n=n, lag=NULL, nf=100)

