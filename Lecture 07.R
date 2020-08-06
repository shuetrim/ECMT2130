#-------------------------------------------------------------------------------------#
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 07: Efficient markets hypothesis and stationary time series
#
# Covers:
#
# - Financial prices as geometric random walks
#
# - Holt Winters EWMA models
#
#-------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# US daily market index and returns
#------------------------------------------------------------------------------------------

# Load the data
# Make sure that the readxl package of functionality is available.
# Run the following command if readxl is not installed already.
# install.packages("readxl")
library(readxl)

# Read data from spreadsheet in same folder as this R script.
# Double click on "myData" in the environment tab to see that data in a tabular view. 
data <- read_excel("Fama_French_market_daily_returns.xlsx")

# Get data from 2000 onwards ?
#dailyData <- dailyData[dailyData$Year >= 2000,]

data$Pm <- exp(cumsum(data$Rm / 100))

# Do the same but demean the returns to remove the drift.
data$R0 <- data$Rm - mean(data$Rm)
data$P0 <- exp(cumsum(data$R0 / 100))

# Extended time series package
#install.packages("xts")
library(xts)

# Get the dates to use in constructing the time-series representation
dates = as.Date(data$Date)

# Represent data as an extended time-series, omitting first 6 columns
# Note it is important to only have numeric data in the extended timeseries object columns!
dataXts <- xts(data[,7:ncol(data)], order.by=dates)

# Plot the time series of daily returns for various industries. Examine other industries yourself.
# Note the date selection criteria with StartDate / EndDate and dates in format YYYYMMDD
# Look at alternative date ranges yourself.
par(mfrow = c(1, 1))
plot.xts(dataXts$Rm['20100101/20201231'], col=c('blue'), main="Market returns (%)")

plot.xts(dataXts$Pm['19700101/20001231'], col=c('red'), main="Market index")

# Examine the market price movements if we remove the drift component.
plot.xts(dataXts$R0['20100101/20201231'], col=c('blue'), main="Demeaned Market returns (%)")

plot.xts(dataXts$P0['19700101/20001231'], col=c('red'), main="Demeaned Market index")

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
# What do the kinks in the scatter plot suggest?


#-------------------------------------------------------------------------------------
# Timeseries data generation and model selection, fitting and forecasting
#-------------------------------------------------------------------------------------

#install.packages(c("astsa", "mgcv", "forecast"))
library(forecast)
library(astsa)
library(mgcv)

# Better if these are not necessary!!! Save on compiling packages for students.
# See https://smac-group.github.io/ts/introtimeseries.html
#install.packages("devtools")
#devtools::install_github("SMAC-Group/simts")
#library(simts)

#-------------------------------------------------------------------------------------
# EWMA: Holt Winters with additive seasonality 
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

time.series = ts(trimmed.data$observed, start = c(1990, 1), frequency = m)
fittedData <- HoltWinters(time.series, seasonal = "additive")
par(mfrow=c(1,1))
plot(fittedData, main = "Additive seasonal")


#-------------------------------------------------------------------------------------
# EWMA: Holt Winters with multiplicative seasonality 
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

time.series = ts(trimmed.data$observed, start = c(1990, 1), frequency = m)
fittedData <- HoltWinters(time.series, seasonal = "multiplicative")
par(mfrow=c(1,1))
plot(fittedData, main = "Multiplicative seasonal")




