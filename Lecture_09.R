#-------------------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 09: Unit root testing and ARIMA models
#
# Topics:
#
# - Spurious regression
#
# - Simulating and fitting data from ARIMA models
#
# - Unit root tests
#
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Spurious regression results
#-------------------------------------------------------------------------------------

# Run a regression of two random walks with drift on eachother, N times and plot R-squared.
T = 100 # data points in each simulation
N = 1000 # simulations
mu = 0.0 # drift
sigma = 1

rsq = vector(mode="double", length=N)
pvalue = vector(mode="double", length=N)
for (i in 1:N) {
  y = arima.sim(list(order = c(0,1,0)), n=T, mean=mu, sd = sigma)
  x = arima.sim(list(order = c(0,1,0)), n=T, mean=mu, sd = sigma)
  data = data.frame(y,x)
  model = lm(y ~ x, data = data)
  output = summary(model)
  rsq[i] = output$r.squared
  pvalue[i] = output$coefficients[2, 4]
}

hist(rsq)

hist(pvalue)

# Explore how the histograms change as the drift becomes larger (set mu to 0.1, 0.5, 1)

#-------------------------------------------------------------------------------------
# Generate I(1) processes and become familiar with differencing them.
#-------------------------------------------------------------------------------------
T = 2000
mu = 0
sigma = 1
x1 = arima.sim(list(order = c(0,1,0)), n=T, mean=mu, sd = sigma)
x2 = arima.sim(list(order = c(1,1,0), ar=c(0.9)), n=T, mean=mu, sd = sigma)

par(mfrow=c(2,1))
plot.ts(x1, main="ARIMA(0,1,0)")
plot.ts(x2, main="ARIMA(1,1,0)")

x1d = diff(x1, differences = 1)
x2d = diff(x2, differences = 1)

par(mfrow=c(2,1))
plot.ts(x1d, main="ARIMA(0,0,0)")
plot.ts(x2d, main="ARIMA(1,0,0)")

#-------------------------------------------------------------------------------------
# I(1) vs I(2) processes and differencing.
#-------------------------------------------------------------------------------------

x3 = arima.sim(list(order = c(0,2,0)), n=T, mean=mu, sd = sigma)

par(mfrow=c(2,1))
plot.ts(x1, main="ARIMA(0,1,0)")
plot.ts(x3, main="ARIMA(0,2,0)")

x1d = diff(x1, differences = 1)
x3d = diff(x2, differences = 1)
par(mfrow=c(2,1))
plot.ts(x1d, main="ARIMA(0,0,0)")
plot.ts(x3d, main="ARIMA(0,1,0)")

x3d2 = diff(x2, differences = 2)
par(mfrow=c(2,1))
plot.ts(x1d, main="ARIMA(0,0,0)")
plot.ts(x3d2, main="ARIMA(0,0,0)")


#-------------------------------------------------------------------------------------
# Unit root testing.
# tseries package: https://cran.r-project.org/web/packages/tseries/
#-------------------------------------------------------------------------------------
# Make sure that the tseries package of time-series-related functionality is available.
# Run the following command if tseries is not installed already.
# install.packages("tseries")
library(tseries)

# Generic augmented D-F test command
#adf.test(x, alternative = c("stationary", "explosive"), k = trunc((length(x)-1)^(1/3)))

adf.test(x1, alternative = c("stationary"), k = 1)

#-------------------------------------------------------------------------------------
# Alternative unit root testing.
# urca package: https://cran.r-project.org/web/packages/urca/
#-------------------------------------------------------------------------------------
# Make sure that the urca package of time-series-related functionality is available.
# Run the following command if urca is not installed already.
# install.packages("urca")
library(urca)

# Generic augmented D-F test command
#ur.df(y, type = c("none", "drift", "trend"), lags = 1, selectlags = c("Fixed", "AIC", "BIC")) 

ur.df(x1, type = c("none"), lags = 1, selectlags = c("Fixed"))

#-------------------------------------------------------------------------------------
# Phillips Perron test for unit root:
#-------------------------------------------------------------------------------------

help(PP.test)

# Test a random walk
PP.test(x1)

# Test white noise.
PP.test(arima.sim(list(order = c(0,0,0)), n=T, mean=mu, sd = sigma))

# Test a causal AR(1) process I(0)
PP.test(arima.sim(list(order = c(1,0,0), ar=c(0.9)), n=100, mean=mu, sd = sigma))
# Try this with different values of n and the AR coefficient. Is it always reliable?

#-------------------------------------------------------------------------------------
# KPSS stationarity test in tseries package.
#-------------------------------------------------------------------------------------
help(kpss.test)
kpss.test(x1, null = c("Level"))

kpss.test(arima.sim(list(order = c(0,0,0)), n=T, mean=mu, sd = sigma), null = c("Level"))

# Test a causal AR(1) process I(0)
kpss.test(arima.sim(list(order = c(1,0,0), ar=c(0.99)), n=100, mean=mu, sd = sigma), null = c("Level"))
# Try this with different values of n and the AR coefficient.

#-------------------------------------------------------------------------------------
# Test for unit root in a stock index
#-------------------------------------------------------------------------------------

# Make sure that the readxl package of functionality is available.
# Run the following command if readxl is not installed already.
# install.packages("readxl")
library(readxl)

# Read data from spreadsheet in same folder as this R script.
equityIndexData <- read_excel("lecture_09_monthly_stock_index.xlsx")

#-------------------------------------------------------------------------------------
# Test stationarity of Australian CPI I(0)? I(1)? I(2)?
#-------------------------------------------------------------------------------------

# Get Latest CPI from ABS website.
# https://www.abs.gov.au/ausstats/abs@.nsf/mf/6401.0
consumerPriceIndexData <- read_excel("Lecture_09_Australia_CPI.xlsx")
