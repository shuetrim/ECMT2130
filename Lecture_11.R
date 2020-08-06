#-------------------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 11: GARCH model estimation and testing
#
# Topics:
#
# - Basic GARCH model estimation (tseries package): https://www.rdocumentation.org/packages/tseries/versions/0.10-47
#
# - Practical article on GARCH usage: https://www.r-bloggers.com/a-practical-introduction-to-garch-modeling/
#
# - Great tutorial on using the fGarch package: https://kevinkotze.github.io/ts-12-tut/
#
#-------------------------------------------------------------------------------------

# install.packages("tseries")
library(tseries)

#install.packages("fGarch")
library(fGarch)

# install.packages("readxl")
library(readxl)

# install.packages("xts")
library(xts)

#-------------------------------------------------------------------------------------
# GARCH model of daily returns
# Uses tseries and readxl and xts packages
#-------------------------------------------------------------------------------------

# Read data from spreadsheet in same folder as this R script.
equityIndexData <- read_excel("Fama_French_market_daily_returns.xlsx")
dates = as.Date(equityIndexData$Date)

# Represent data as an extended time-series, omitting first 6 columns
# Note it is important to only have numeric data in the extended timeseries object columns!
rm.xts <- xts(equityIndexData$Rm, order.by=dates)

model <- garch(rm.xts, order = c(1, 1), grad="numerical")

summary(model)

plot(model)

#-------------------------------------------------------------------------------------
# ARMA + GARCH estimation
# Use fGarch package
#-------------------------------------------------------------------------------------

m1=garchFit(formula=~arma(2,0)+garch(1,1),data=rm.xts,trace=TRUE)
summary(m1)

# Plot the original data (rm.xts)
plot(m1@data)
#Confirm this by comparing the first 5 values
head(rm.xts,5)
head(m1@data)

# Plot the fitted error standard deviations
plot(m1@sigma.t)

# Plot the fitted error variances
head(m1@h.t ^ 0.5,5)
# Again confirm the information by comparing the first 5 values
head(m1@h.t ^ 0.5,5)
head(m1@sigma.t,5)

# Plot the model residuals
plot(m1@residuals)

# Confirm that the series$z values are also the residuals:
head(m1@residuals,5)

# Plot the fitted values
plot(m1@fitted)

# Compare fitted values + residuals to the original data - they should match.
head(m1@fitted + m1@residuals,5)
head(m1@data,5)

# Model coefficient estimates
m1@fit$coef

# Model coefficient estimate standard errors
m1@fit$se.coef

# Hessian matrix (second derivatives of the likelihood function)
m1@fit$hessian

# Variance covariance matrix of coefficients
m1@fit$cvar

# Compare to the inverse hessian matrix...
solve(m1@fit$hessian)

#standard errors
sqrt(diag(m1@fit$cvar))

# Confirm these match the given standard errors.
m1@fit$se.coef

#-------------------------------------------------------------------------------------
# GARCH prediction
# Uses fGarch package
#-------------------------------------------------------------------------------------

# Make forecasts of the original series.
predict(m1,3)

# Can you construct the forecast error variances over the next 3 days?
# Use the model coefficients and the last few residuals and the last error variance fitted value.

#-------------------------------------------------------------------------------------
# GARCH testing
# LR/LM/Wald tests
#-------------------------------------------------------------------------------------
# Add example of testing restriction on GARCH structure and ARMA structure jointly using
# the 3 different types of tests.
