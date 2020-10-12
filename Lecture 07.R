#-------------------------------------------------------------------------------------#
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 07: Efficient markets hypothesis and stationary time series
#
# Covers:
#
# - Stationary and non-stationary data examples.
#
# - Financial prices as geometric random walks.
#
# - Holt Winters EWMA models.
#
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Load relevant packages
#-------------------------------------------------------------------------------------

# Red Excel data
library(readxl)

# Extended time series package
library(xts)

#-------------------------------------------------------------------------------------
# EWMA: Use Holt Winters to forecast C02 concentration in atmosphere (monthly)
#-------------------------------------------------------------------------------------


# Fit the model
(model <- HoltWinters(co2, seasonal = "additive"))
summary(model)

# View the components of the model
components <- as.data.frame(fitted(model))
components <- as.xts(components, order.by = as.Date(index(components)))
par(mfrow=c(2,2))
plot(components$xhat)
plot(components$level)
plot(components$trend)
plot(components$season)
par(mfrow=c(1,1))

# Make forecasts
predictions <- predict(model, 50, prediction.interval = TRUE)
plot(model, predictions)

#-------------------------------------------------------------------------------------
# EWMA: Use Holt Winters to fit monthly airline passengers with multiplicative season
#-------------------------------------------------------------------------------------
(model <- HoltWinters(AirPassengers, seasonal = "mult"))
plot(model)

components <- as.data.frame(fitted(model))
components <- as.xts(components, order.by = as.Date(index(components)))
par(mfrow=c(2,2))
plot(components$xhat)
plot(components$level)
plot(components$trend)
plot(components$season)
par(mfrow=c(1,1))

# 99% confidence interval
predictions <- predict(model, 24, prediction.interval = TRUE, level = 0.99)
plot(model, predictions)

# 90% confidence interval
predictions <- predict(model, 24, prediction.interval = TRUE, level = 0.9)
plot(model, predictions)

#-------------------------------------------------------------------------------------
# Stationary series
#-------------------------------------------------------------------------------------

# Set the number of observations
T = 1000

# Gaussian white noise (stationary)
data <- as.timeSeries(rnorm(T,mean=0, sd = 1))
colnames(data) <- c("White noise N(0,1)")
plot(data)

# Mean reverting AR(1) process (x_t = 0.75 x_t-1 + e_t)
data <- as.timeSeries(arima.sim(list(order = c(1,0,0), ar = 0.75), n = T, mean = 0, sd = 1))
colnames(data) <- c("AR(1) with 0.75 coefficient")
plot(data)


#------------------------------------------------------------------------------------
# Non-stationary series
#------------------------------------------------------------------------------------

# Deterministic trend
data <- as.timeSeries(1:T + rnorm(T,mean=0, sd = 2))
colnames(data) <- c("Deterministic trend")
plot(data)

# Random walk
data <- as.timeSeries(arima.sim(list(order = c(0,1,0)), n = T, mean = 0, sd = 1))
colnames(data) <- c("Random walk")
plot(data)

# Random walk with drift
data <- as.timeSeries(arima.sim(list(order = c(0,1,0)), n = T, mean = 0.25, sd = 1))
colnames(data) <- c("Random walk")
plot(data)

#------------------------------------------------------------------------------------------
# US daily market index and returns
#------------------------------------------------------------------------------------------

# Read data from spreadsheet in same folder as this R script.
# Double click on "myData" in the environment tab to see that data in a tabular view. 
data <- read_excel("Fama_French_market_daily_returns.xlsx")
dates <- as.Date(data$Date)
dataXts <- xts(data[,7:ncol(data)], order.by=dates)

timespan <- '20100101/20201231'

# Reconstruct adjusted prices (adjusting for dividends etc) from returns.
dataXts$Pm <- exp(cumsum(dataXts$Rm / 100))

# Plot the time series of daily returns for various industries.
# Look at alternative date ranges yourself.
par(mfrow = c(2, 1))
plot.xts(dataXts$Rm[timespan], col=c('blue'), main="Market returns (%)")
plot.xts(dataXts$Pm[timespan], col=c('red'), main="Market index")
par(mfrow = c(1, 1))

# Compare volatility over different observation frequencies

#Daily data standard deviation
sd(dataXts$Rm, na.rm=TRUE)

# Weekly data
aggregatedData <- to.weekly(dataXts$Pm)
weeklyRm <- diff.xts(log(aggregatedData$`dataXts$Pm.Close`), lag = 1, differences = 1) * 100
plot.xts(weeklyRm, col=c('blue'), main="Weekly market return")
sd(weeklyRm, na.rm=TRUE)
sd(dataXts$Rm, na.rm=TRUE) * sqrt(5)

# Monthly data
aggregatedData <- to.monthly(dataXts$Pm)
monthlyRm <- diff.xts(log(aggregatedData$`dataXts$Pm.Close`), lag = 1, differences = 1) * 100
plot.xts(monthlyRm, col=c('blue'), main="Monthly market return")
sd(monthlyRm, na.rm=TRUE)
sd(dataXts$Rm, na.rm=TRUE) * sqrt(20.8)

# Yearly data
aggregatedData <- to.yearly(dataXts$Pm)
yearlyRm <- diff.xts(log(aggregatedData$`dataXts$Pm.Close`), lag = 1, differences = 1) * 100
plot.xts(yearlyRm, col=c('blue'), main="Monthly market return")
sd(yearlyRm, na.rm=TRUE)
sd(dataXts$Rm, na.rm=TRUE) * sqrt(261)

# Collate volatilities of returns over 1 through N days. 260 = average working days per year in USA
N <- 260
actualSD <- vector("numeric", N) # prepare a container for actual standard deviations
predictedSD <- vector("numeric", N) # prepare a container for predicted standard deviations.
for (i in 1:N) {
  actualSD[i] <- sd(diff.xts(log(dataXts$Pm), lag = i, differences = 1) * 100, na.rm = TRUE)
  predictedSD[i] <- sd(dataXts$Rm, na.rm=TRUE) * sqrt(i)
}

plot(actualSD, predictedSD, xlab = "Actual", ylab="Predicted", main = "Market return standard deviations over different data frequencies")
grid(NULL, NULL, col = "blue", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
# Try running the comparison above but with N = 5000? 
# Can you interpret the kinks in the scatter plot?


