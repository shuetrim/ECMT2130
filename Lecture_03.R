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
# Note the error in the sample_excess formula where var() is used instead of sd().
# Explore this difference between the implementation and the documentation in the source.
# https://rdrr.io/cran/PerformanceAnalytics/src/R/kurtosis.R

data <- rnorm(10)
kurtosis(data,method = "sample_excess") # check this matches the Excel KURT() function

#------------------------------------------------------------------------------------------
# US industry data
#------------------------------------------------------------------------------------------


# Load the data
# Make sure that the readxl package of functionality is available.
# Run the following command if readxl is not installed already.
# install.packages("readxl")
library(readxl)

# install.packages("xts")
library(xts)

# install.packages(c("PortfolioAnalytics", "ROI", "ROI.plugin.quadprog"))
library(ROI)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)


# Read data from spreadsheet in same folder as this R script.
# Double click on "myData" in the environment tab to see that data in a tabular view. 
dailyData <- read_excel("Fama_French_industry_daily_returns.xlsx")

# Preserve dates to create extended time-series object later on.
dates <- as.Date(dailyData$Date)
industries <- colnames(dailyData)[9:ncol(dailyData)]
returnsData <- xts(dailyData[,industries], order.by=dates)

# Just use data from 2000 onwards
returnsData <- returnsData["20000101/"]

n <- nrow(dailyData)

# Plot the time series of daily returns for various industries. Examine other industries yourself.
par(mfrow = c(1, 1))
plot(returnsData$Fin, col=c('blue'), main="Finance daily returns (%)")
plot(returnsData$Smoke, col=c('blue'), main="Health daily returns (%)")
plot(returnsData$Guns, col=c('blue'), main="Telecoms daily returns (%)")
plot(returnsData$Cnstr, col=c('blue'), main="Construction daily returns (%)")
plot(returnsData$Gold, col=c('blue'), main="Gold daily returns (%)")
par(mfrow = c(1, 1))

# Get the data for the chosen industry
industryReturns <- returnsData$Hlth # Pick health industry in this case.
# TIP: Use CTRL-space for auto-completion of industry names as you type after the $ in the command above.

#------------------------------------------------------------------------------------------
# Histogram with superimposed normal density function
#
# Do you think the normal distribution is a good approximation? - bit hard to see...
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
# Pairwise rolling correlations
#------------------------------------------------------------------------------------------
#install.packages("roll")
library(roll)
window <- 90

# Alter this list of 2 industries to explore other industry pairs
pair <- c("Gold", "Fin") # or
pair <- c("Cnstr", "Steel") # or
pair <- c("Softw", "Hardw")

# Create an extended time series for the correlations of the two chosen industries
rollingCorrelations <- matrix(unlist(roll_cor(returnsData[,pair[1]], returnsData[,pair[2]], width = window)),nrow=nrow(returnsData), ncol=1)
correlations <- xts(as.data.frame(rollingCorrelations), order.by=index(returnsData))
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
positions = c(100, 100) # $100 invested in each industry

# Get most recent year of returns for the two industries and convert from % to decimal
recentReturns <- as.data.frame(returnsData['20190530/'][,pair] / 100)

# Parametric VaR - first estimate the distribution parameters

# Get means and variance-covariance matrix for the 2 industries
(expectedReturns <- colMeans(recentReturns))
(VCOV <- var(recentReturns))

# Compute the portfolio return and variance using matrix operations (%*% is the matrix multiplication operator)
(portfolioMeanProfit <- t(positions) %*% expectedReturns) # In dollars
(portfolioProfitVariance <- t(positions) %*% VCOV %*% positions) 
(portfolioProfitStandardDeviation <- sqrt(portfolioProfitVariance)) # In dollars

# Assuming that returns are normally distributed, 
# compute the 1st percentile of the portfolio 
# 1 day expected return distribution
(parametricVaR <- qnorm(0.01,mean = portfolioMeanProfit, sd = portfolioProfitStandardDeviation))


# Now determine the historical simulation VaR

# Compute the daily returns for the portfolio over the last year
portfolioProfits = ((as.matrix(recentReturns)) %*% positions)

orderedPortfolioProfits <- portfolioProfits[order(portfolioProfits)]
head(orderedPortfolioProfits)
numberOfHistoricalObservations <- length(orderedPortfolioProfits)
(varIndex <- floor(0.01 * numberOfHistoricalObservations)) # Be conservative - round index down.
(historicalSimulationVaR <- orderedPortfolioProfits[varIndex])

# Can you say why this is so much bigger than the parametric VaR?

# Now do a simplified monte carlo simulation of the portfolio profits directly.

# Based on a normal distribution...
simulationCount = 500000 # Distribution of the Monte Carlo VaR collapses on the parametric VaR as simulationCount -> infinity.
varIndex = floor(0.01 * simulationCount)

# Generate simulated portfolio returns directly using mean and variance of portfolio returns and sort into ascending order.
simulatedPortfolioProfits = sort(rnorm(simulationCount, mean=portfolioMeanProfit, sd = portfolioProfitStandardDeviation))

# Double check that simulated data implies variance of volatility returns that matches sample
var(simulatedPortfolioProfits)
portfolioProfitVariance

(monteCarloSimulationVaR = simulatedPortfolioProfits[varIndex])

# In this trivial example the monte carlo simulation is just using a simulation approach to get
# the same result as is available from the parametric approach.

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
simulationCount = 500000 # Distribution of the Monte Carlo VaR collapses on the parametric VaR as simulationCount -> infinity.
altSimulatedReturns <- MASS::mvrnorm(n=simulationCount, mu = expectedReturns, Sigma = VCOV)
head(altSimulatedReturns)

# Compute portfolio profits (this is the 'pricing engine') add across assets and sort them into order.
altSimulatedPortfolioProfits = sort(as.matrix(altSimulatedReturns) %*% positions)
head(altSimulatedPortfolioProfits)

# Pick the percentile for the VaR from the ordered draws.
varIndex = floor(0.01 * simulationCount)
(altMonteCarloSimulationVaR <- altSimulatedPortfolioProfits[varIndex])

# Double check that simulated data has mean returns close to population mean returns
colMeans(altSimulatedReturns)
expectedReturns

# Double check that simulated data has variance close to population variance
var(altSimulatedReturns)
VCOV

# Double check that simulated data implies variance of volatility returns that matches sample
var(altSimulatedPortfolioProfits)
portfolioProfitVariance

#------------------------------------------------------------------------------------------
# Expected shortfall using Monte Carlo data
#------------------------------------------------------------------------------------------
(monteCarloExpectedShortfall = mean(simulatedPortfolioProfits[1:varIndex-1]))

(altMonteCarloExpectedShortfall = mean(altSimulatedPortfolioProfits[1:varIndex-1]))

#------------------------------------------------------------------------------------------
# Backtesting the Monte Carlo VaR
#------------------------------------------------------------------------------------------

# Lets examine exceptions over data used in formulating the model
recentReturns <- as.data.frame(returnsData['20190530/'][,pair] / 100)
recentPortfolioProfits = (as.matrix(recentReturns) %*% positions)
(exceptionRateUsingLastYearOfData = sum(recentPortfolioProfits < monteCarloSimulationVaR) / nrow(recentPortfolioProfits))

# Often you would look at exceptions over other longer time periods.
# Lets examine exceptions over all of history.
allReturns <- as.data.frame(returnsData['20000101/'][,pair] / 100)
allPortfolioProfits = (as.matrix(allReturns) %*% positions)
(exceptionRateUsingAllData = sum(allPortfolioProfits < monteCarloSimulationVaR) / nrow(allPortfolioProfits))

# For extra points, formulate the Monte Carlo VaR over the data for 1999 and then test it over 2020?
# What would you expect from the out-of-sample testing given COVID-19?
returnsfor2019 <- NULL
returnsfor2019 <- as.data.frame(returnsData['20190101/20191231'][,pair] / 100)
expectedReturns2019 <- colMeans(returnsfor2019)
VCOV2019 <- var(returnsfor2019)
portfolioExpectedProfit2019 <- t(positions) %*% expectedReturns2019
portfolioProfitVariance2019 <- t(positions) %*% VCOV2019 %*% positions
portfolioProfitStandardDeviation2019 <- sqrt(portfolioProfitVariance2019)
simulatedPortfolioProfits2019 <- sort(rnorm(simulationCount, mean=portfolioExpectedProfit2019, sd = portfolioProfitStandardDeviation2019))
varIndex <- floor(0.01 * simulationCount)
(monteCarloSimulationVaR2019 <- simulatedPortfolioProfits2019[varIndex])

returnsfor2020 <- as.data.frame(returnsData['20200101/20200530'][,pair] / 100)
portfolioProfits2020 <- (as.matrix(returnsfor2020) %*% positions)
(exceptionRateIn2020ForVaR2019 <- sum(portfolioProfits2020 < monteCarloSimulationVaR2019) / nrow(returnsfor2020))

# How would you respond to this situation if you had risk management responsibility for this portfolio?



