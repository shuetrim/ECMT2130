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

# Estimation of ARCH(2) or GARCH(2,0) model
help(garchFit)
summary(model <- garchFit(formula=~garch(2,0),data=x,trace=TRUE))

# Lots of different plot options
plot(model)
plot(model, which = 2)

#-------------------------------------------------------------------------------------
# GARCH data simulation the easy way
# Uses fGarch package
#-------------------------------------------------------------------------------------
help(garchSpec)
help(garchSim)
# garchFit parameter interpretation guidance:
# https://stats.stackexchange.com/questions/247140/interpretation-garchfit

# Simulate a GARCH(1,1) model (read help documentation to see the GARCH parameters)
data <- garchSim(spec = garchSpec(), n = 1000, extended = FALSE)

# Check the coefficient estimates - do they match those used to simulate the data? 
summary(model <- garchFit(formula=~garch(1,1),data=data,trace=TRUE))

# Log likelihood function value:
model@fit$value

# Simulate an AR(1)-ARCH(2) model
spec = garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
garchSim(spec, n = 10, extended = TRUE)

# Simulate an AR(1)-ARCH(2) model
spec = garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
garchSim(spec, n = 10, extended = TRUE)

# ARMA(1,2)-GARCH(1,1) - use default garch coefficient values
spec = garchSpec(model = list(ar = 0.01, ma = c(0.3, -0.3), alpha = 0.3, beta = 0.4))
simulatedData <- garchSim(spec, n = 50000, extended = TRUE)
plot(simulatedData$garch, main="Observed data in simulation")
plot(simulatedData$sigma, main="Time varying error standard deviation in simulation")
plot(simulatedData$eps, main="N(0,1) v_t in the main equation for ARCH/GARCH")

#-------------------------------------------------------------------------------------
# Estimate the Garch model using the simulated data.
#-------------------------------------------------------------------------------------
summary(model <- garchFit(formula=~arma(1,2)+garch(1,1),data=simulatedData$garch,trace=TRUE))
plot(model)
model@fit$message # This does not seem to be a reliable indicator of convergence issues.
model@fit$convergence

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

summary(model <- garchFit(formula=~garch(1,1),data=rm.xts,trace=TRUE))


#-------------------------------------------------------------------------------------
# Impact of clustered volatility on standard errors from OLS regression
#-------------------------------------------------------------------------------------

n <- 1000
spec <- garchSpec(model = list(alpha = 0.7, beta = 0.2, omega = 1))
set.seed(1)
# Simulate a GARCH(1,1) errors to get errors with volatility clustering
data <- as.matrix(garchSim(spec, n = n, extended = FALSE))
heteroskedasticErrors <- scale(data, center = TRUE, scale = TRUE)
plot(heteroskedasticErrors)

set.seed(1)
homoskedasticErrors <- rnorm(n = n)
homoskedasticErrors <- scale(homoskedasticErrors, center = TRUE, scale = TRUE)
plot(homoskedasticErrors)

x <- rnorm(n = n)

heteroskedasticY <- x + heteroskedasticErrors
homoskedasticY <- x + homoskedasticErrors

data <- data.frame(cbind(x, heteroskedasticY, homoskedasticY))
colnames(data) <- c("x", "heteroskedasticY", "homoskedasticY")

summary(lm(homoskedasticY ~ x, data=data))
summary(lm(heteroskedasticY ~ x, data=data))

