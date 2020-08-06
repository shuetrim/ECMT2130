#------------------------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 03 - Equity returns and Value at Risk.
#
# Topics:
# - Kurtosis
# - Daily industry equity return data in the US.
# - Daily return distribution - histogram and Q-Q plot
# - Industry correlations
# - Time variation in variances and correlations
# - Value at Risk
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# Kurtosis
#------------------------------------------------------------------------------------------
help("kurtosis")
data <- rnorm(10)
kurtosis(data,method = "sample_excess")

#------------------------------------------------------------------------------------------
# US industry data
#------------------------------------------------------------------------------------------

# Load the data
# Make sure that the readxl package of functionality is available.
# Run the following command if readxl is not installed already.
# install.packages("readxl")
library(readxl)

# Read data from spreadsheet in same folder as this R script.
# Double click on "myData" in the environment tab to see that data in a tabular view. 
dailyData <- read_excel("Fama_French_industry_daily_returns.xlsx")

# Get data from 2000 onwards
dailyData <- dailyData[dailyData$Year >= 2000,]
n <- nrow(dailyData)


# Extended time series package
library(xts)
industries <- colnames(dailyData)[9:ncol(dailyData)]

dates = as.Date(dailyData$Date)


returnsData <- xts(dailyData[,industries], order.by=dates)

# Plot the time series of daily returns for various industries. Examine other industries yourself.
par(mfrow = c(5, 1))
plot(returnsData$Fin, col=c('blue'), main="Finance daily returns (%)")
plot(returnsData$Smoke, col=c('blue'), main="Health daily returns (%)")
plot(returnsData$Guns, col=c('blue'), main="Telecoms daily returns (%)")
plot(returnsData$Cnstr, col=c('blue'), main="Construction daily returns (%)")
plot(returnsData$Gold, col=c('blue'), main="Gold daily returns (%)")
par(mfrow = c(1, 1))

# Get the data for the chosen industry
industryReturns <- returnsData$Hlth # Pick health industry in this case.
# TIP: Use CTRL-space for completion of industry names

#------------------------------------------------------------------------------------------
# Histogram with superimposed normal density function
#------------------------------------------------------------------------------------------
help("hist")
#help("dnorm")
m <- mean(industryReturns)
s <- StdDev(industryReturns)
hist(industryReturns, density=20, breaks=200, prob=TRUE, 
     xlab="Finance returns", 
     main="Histogram with theoretical normal density",
     xlim=c(-6,6))
curve(dnorm(x, mean=m, sd=s), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#------------------------------------------------------------------------------------------
# Q-Q plots to examine distribution tails
#------------------------------------------------------------------------------------------
help("qqplot")

# Note the deviations from the normal line at the tails - strongly indicating "fat tails".
qqnorm(industryReturns, ylab = "Daily industry returns")
qqline(industryReturns) # Add theoretical Q-Q plot for a normal distribution with same moments.

# Note small numbers of observations there are in the fat tails of the distribution.

#------------------------------------------------------------------------------------------
# Industry correlations
#------------------------------------------------------------------------------------------

help("cor")

# Explore with a correlation plot
# Run the install packages command once if corrplot is not installed already.
# install.packages("corrplot")
library(corrplot)
help("corrplot")
corrplot(cor(returnsData), method = "ellipse")
title("Correlations", line = -2) # adjust placement of the plot title.

#------------------------------------------------------------------------------------------
# Rolling correlations
#------------------------------------------------------------------------------------------
#install.packages("roll")
library(roll)
window <- 90

# Alter this list of 2 industries to explore other industry pairs
pair <- c("Gold", "Fin")
pair <- c("Cnstr", "Steel")
pair <- c("Softw", "Hardw")

# Create an extended time series for the correlations of the two chosen industries
correlations <- as.data.frame(matrix(unlist(roll_cor(returnsData[,pair[1]], returnsData[,pair[2]], width = window)),nrow=n, ncol=1))
correlations <- xts(correlations, order.by=dates)
colnames(correlations) = "correlations"

# Graph the variance of a single industry over time
variances1 = roll_var(returnsData[,pair[1]], width=window) # already an XTS object.
variances2 = roll_var(returnsData[,pair[2]], width=window) # already an XTS object.
data <- merge(correlations, sqrt(variances1), sqrt(variances2))

# Graph the rolling estimates as two panels
par(mfrow = c(2, 1))
plot(data$correlations, main="Correlation")
plot(data[,pair], col=c('red', 'blue'), main="Standard Deviations")
legend('topright',pair , lty=1, col=c('red', 'blue'), bty='n')
par(mfrow = c(1, 1))

#------------------------------------------------------------------------------------------
# Get 0.99% Value at Risk estimation for equally weighted portfolio of 2 industries
# Use last year of data.
#
# For more on XTS data manipulation (especially getting subsets based on time criteria): 
# https://rstudio-pubs-static.s3.amazonaws.com/288218_117e183e74964557a5da4fc5902fc671.html
#
# For more on matrix operations in R:
# https://www.statmethods.net/advstats/matrix.html
#
#------------------------------------------------------------------------------------------
weights = c(0.5, 0.5)

# Get most recent year of returns, noting 20200529 is the last observations
recentReturns <- as.data.frame(returnsData['20190530/'])

# Get means and variance-covariance matrix for the 2 industries
expectedReturns <- colMeans(recentReturns[,pair])
VCOV <- var(recentReturns[,pair])

# Compute the portfolio return and variance using matrix operations (%*% is the matrix multiplication operator)
portfolioReturn <- t(weights) %*% expectedReturns
portfolioVariance <- t(weights) %*% VCOV %*% weights
portfolioStandardDeviation <- sqrt(portfolioVariance)

# Assuming that returns are normally distributed, 
# compute the 1st percentile of the portfolio 
# 1 day expected return distribution
parametricNormalVaR = qnorm(0.01,mean = portfolioReturn, sd = portfolioStandardDeviation)
parametricNormalVaR

# Now determine the historical simulation VaR

# Compute the daily returns for the portfolio over the last year
recentReturns$portfolio = (as.matrix(recentReturns[,pair]) %*% weights)

portfolioReturns <- recentReturns[order(recentReturns$portfolio), c("portfolio")]
numberOfObservations = nrow(portfolioReturns)
varIndex = floor(0.01 * numberOfObservations) # Be conservative - round index down.
historicalSimulationVaR = portfolioReturns[varIndex]
historicalSimulationVaR

# Now work on the monte carlo simulation

# Based on a normal distribution...
simulationCount = 500000 # Distribution of the Monte Carlo VaR collapses on the parametric VaR as simulationCount -> infinity.
varIndex = floor(0.01 * simulationCount)

# Generate simulated portfolio returns directly using mean and variance of portfolio returns and sort into ascending order.
simulatedPortfolioReturns = sort(rnorm(simulationCount, mean=portfolioReturn, sd = portfolioStandardDeviation))

# Double check that simulated data implies variance of volatility returns that matches sample
var(simulatedPortfolioReturns)
portfolioVariance


monteCarloSimulationVaR = simulatedPortfolioReturns[varIndex]
monteCarloSimulationVaR

#-------------------------------------------------------------------------------------
# More typical Monte Carlo approach where all prices are simulated.
# Here we need to generate correlated normal random variates
#
# MASS Package: https://cran.r-project.org/web/packages/MASS/
#
#-------------------------------------------------------------------------------------
# Make sure we have the MASS package available.
# install.packages("MASS")
library(MASS)

# Randomly generate normal data with the given means and the
# given variance-covariance matrix, Sigma.
altSimulatedReturns <- MASS::mvrnorm(n=simulationCount, mu = expectedReturns, Sigma = VCOV)

# Double check that simulated data has mean returns close to population mean returns
colMeans(altSimulatedReturns)
expectedReturns

# Double check that simulated data has variance close to population variance
var(altSimulatedReturns)
VCOV


# Compute portfolio returns and sort them into ascending order.
altSimulatedPortfolioReturns = sort(as.matrix(altSimulatedReturns) %*% weights)

# Double check that simulated data implies variance of volatility returns that matches sample
var(altSimulatedPortfolioReturns)
portfolioVariance

altMonteCarloSimulationVaR = altSimulatedPortfolioReturns[varIndex]
altMonteCarloSimulationVaR

#------------------------------------------------------------------------------------------
# Expected shortfall using Monte Carlo data
#------------------------------------------------------------------------------------------
monteCarloExpectedShortfall = mean(simulatedPortfolioReturns[1:varIndex-1])
monteCarloExpectedShortfall

altMonteCarloExpectedShortfall = mean(altSimulatedPortfolioReturns[1:varIndex-1])
altMonteCarloExpectedShortfall


#------------------------------------------------------------------------------------------
# Backtesting the Monte Carlo VaR
#------------------------------------------------------------------------------------------

# Lets examine exceptions over the year of data used in formulating the model
exceptionRateUsingLastYearOfData = sum(dailyData$portfolio < monteCarloSimulationVaR) / nrow(dailyData)
exceptionRateUsingLastYearOfData

# Lets examine exceptions over all of history.
dailyData$portfolio = (as.matrix(dailyData[,pair]) %*% weights)
exceptionRateOverEntireHistory = sum(dailyData$portfolio < monteCarloSimulationVaR) / nrow(dailyData)
exceptionRateOverEntireHistory

# Just for kicks, formulate the monteCarlo VaR over the data for 1999 and then test it over 2020?
# What would you expect from the out-of-sample testing given COVID-19?
returnsfor1999 <- as.data.frame(returnsData['20190101/20191231'])
returnsfor1999$portfolio = (as.matrix(returnsfor1999[,pair]) %*% weights)
expectedReturns1999 <- colMeans(returnsfor1999[,pair])
VCOV1999 <- var(returnsfor1999[,pair])
portfolioReturn1999 <- t(weights) %*% expectedReturns1999
portfolioVariance1999 <- t(weights) %*% VCOV1999 %*% weights
portfolioStandardDeviation1999 <- sqrt(portfolioVariance1999)
simulatedReturns1999 = sort(rnorm(simulationCount, mean=portfolioReturn1999, sd = portfolioStandardDeviation1999))
varIndex = floor(0.01 * simulationCount)
monteCarloSimulationVaR1999 = simulatedReturns1999[varIndex]
monteCarloSimulationVaR1999

returnsfor2020 <- as.data.frame(returnsData['20200101/20200530'])
returnsfor2020$portfolio = (as.matrix(returnsfor2020[,pair]) %*% weights)
exceptionRateUsingLastYearOfDataForVaR1999 = sum(returnsfor2020$portfolio < monteCarloSimulationVaR1999) / nrow(returnsfor2020)
exceptionRateUsingLastYearOfDataForVaR1999

# How would you respond to this situation if you had risk management responsibility for this portfolio?

#------------------------------------------------------------------------------------------
# NOT REQUIRED FOR ECMT2130 but an interesting read
# Extension involving more sophisticated usage of R to obtain rolling statistics.
#
# Rolling analysis with Tidyquant.
# Vignette: https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ03-scaling-and-modeling-with-tidyquant.html
#
# Apply rolling functions to timeseries data with various frequencies.
#
# Brings together timeseries packages and the R Tidyverse
# Getting to know the Tidyverse  https://www.tidyverse.org/
#
# 4 part tutorial
# https://www.business-science.io/timeseries-analysis/2017/07/02/tidy-timeseries-analysis.html
# https://www.business-science.io/timeseries-analysis/2017/07/23/tidy-timeseries-analysis-pt-2.html
# https://www.business-science.io/timeseries-analysis/2017/07/30/tidy-timeseries-analysis-pt-3.html
# https://www.business-science.io/timeseries-analysis/2017/08/30/tidy-timeseries-analysis-pt-4.html
#------------------------------------------------------------------------------------------
