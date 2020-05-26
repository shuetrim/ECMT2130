#-------------------------------------------------------------------------------------
# USAGE OF R IS NOT ASSESSABLE FOR ECMT2130.
#
# Written by Geoff Shuetrim
# Tutorial 06: Unit root testing and ARIMA models
#
# Covers:
#
# - Unit root tests
#
# - ARIMA model specification, fit, and diagnostics.
#
#-------------------------------------------------------------------------------------

# Make sure that the readxl package of functionality is available.
# Run the following command if readxl is not installed already.
# install.packages("readxl")
library(readxl)

# ARMA and ARIMA related packages. Run the next two commands once.
# See https://smac-group.github.io/ts/introtimeseries.html
#install.packages(c("devtools", "astsa", "mgcv"))
#devtools::install_github("SMAC-Group/simts")
library(astsa)
library(mgcv)
library(simts)
 

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
# Generate I(1) processes and become familiar with differencing them.
#-------------------------------------------------------------------------------------

# Read data from spreadsheet in same folder as this R script.
myData <- read_excel("lecture_05_tutorial_stock_index.xlsx")

