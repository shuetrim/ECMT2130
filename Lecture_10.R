#-------------------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 10: GARCH models
#
# Topics:
#
# - Basic GARCH model estimation (tseries package): https://www.rdocumentation.org/packages/tseries/versions/0.10-47
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
# ARCH(2) model simulation the hard way.
# Use tseries package for estimation
#-------------------------------------------------------------------------------------
n <- 1100
a <- c(0.1, 0.5, 0.2)  # ARCH(2) coefficients
e <- rnorm(n)
x <- double(n)
x[1:2] <- rnorm(2, sd = sqrt(a[1]/(1.0-a[2]-a[3]))) 
for(i in 3:n)  # Generate ARCH(2) process
{
  x[i] <- e[i]*sqrt(a[1]+a[2]*x[i-1]^2+a[3]*x[i-2]^2)
}
x <- ts(x[101:1100])

# Using tseries package GARCH estimation
model <- garch(x, order = c(0, 2), grad="numerical")

summary(model)

plot(model)


#-------------------------------------------------------------------------------------
# GARCH data simulation the easy way
# Uses fGarch package
#-------------------------------------------------------------------------------------
help(garchSpec)
help(garchSim)

# Simulate a GARCH(1,1) model (read help documentation to see the GARCH parameters)
data <- garchSim(spec = garchSpec(), n = 1000, extended = FALSE)

# Check the coefficient estimates - do they match those used to simulate the data? 
summary(garchFit(formula=~garch(1,1),data=data,trace=TRUE))

# Simulate an AR(1)-ARCH(2) model
spec = garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
garchSim(spec, n = 10, extended = TRUE)

# Simulate an AR(1)-ARCH(2) model
spec = garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
garchSim(spec, n = 10, extended = TRUE)

# ARMA(1,2)-GARCH(1,1) - use default garch coefficient values
spec = garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3)))
garchSim(spec, n = 10, extended = TRUE)

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

