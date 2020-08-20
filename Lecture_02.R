#------------------------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 02: Portfolio optimisation
#
# Topic 1: Portfolio optimisation in Excel
#
# Topic 2: Portfolio optimisation in R
#
# See also http://www.finance-r.com/
#
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# Get summary statistics to use in efficient frontier construction for a subset of industries.
# Copy the results into Excel and use the text to columns function with fixed width 
# separation to do the splitting into columns.
#------------------------------------------------------------------------------------------

# Load the data
# Make sure that the readxl package of functionality is available.
# Run the following command if readxl is not installed already.
# install.packages("readxl")
library(readxl)

# install.packages("xts")
library(xts)

# Read data from spreadsheet in same folder as this R script.
# Double click on "myData" in the environment tab to see that data in a tabular view. 
myData <- read_excel("Fama_French_industry_monthly_returns.xlsx")

# Preserve dates to create extended time-series object later on.
dates <- as.Date(myData$Date)

# Get list of industries used in the Excel analysis
industries <- c("Smoke", "Aero", "Fin", "Softw", "Hardw", "Drugs", "Hlth")

# Alternatively, for use in R, work with all industries (too hard in Excel, in my view)
# Note that the first 8 columns of myData are not industry returns.
#industries <- colnames(myData)[9:ncol(myData)]

# We need to understand that the data has a time dimension to do the portfolio analysis
# so convert the industry returns data frame to an extended timeseries object.
# This is required to use the returns in the PortfolioAnalytics package.
industryReturnsXTS <- xts(myData[,industries], order.by=dates)
industryReturnsXTS <- industryReturnsXTS['20010101/20200331']

# Store the risk-free returns as an extended timeseries object. 
riskFreeReturnsXTS <- xts(myData[,c("RF")], order.by=dates)
riskFreeReturnsXTS <- riskFreeReturnsXTS['20010101/20200331']

# Summary statistics - know your data!
summary(industryReturnsXTS[,industries])

# Variance covariance matrix
var(industryReturnsXTS)

# Expected return means for industries
(industryMeanReturns <- colMeans(industryReturnsXTS[,industries]))

# Compute the average risk free rate of return. Check its value in the environment tab.
(rf <- colMeans(riskFreeReturnsXTS))

#------------------------------------------------------------------------------------------
# Industry correlations
# install.packages("corrplot")
library(corrplot)
#help("cor")
#help("corrplot")
#------------------------------------------------------------------------------------------

# Correlations useful in understanding weights along efficient frontier.
(correlations <- cor(industryReturnsXTS))

# Explore with a correlation plot
# Run the install packages command once if corrplot is not installed already.
corrplot(correlations, method = "ellipse")
title("Correlations", line = -2) # adjust placement of the plot title.

#------------------------------------------------------------------------------------------
# Portfolio optimisation in R using PortfolioAnalytics package instead of doing it in Excel.
#
# Documentation:
#
# https://cran.r-project.org/web/packages/PortfolioAnalytics/PortfolioAnalytics.pdf
# https://cran.r-project.org/web/packages/PortfolioAnalytics/vignettes/portfolio_vignette.pdf
#
# A nice easy introductory usage of Portfolio Analytics
# https://finbloggers.wordpress.com/2016/03/08/the-efficient-frontier-with-portfolioanalytics-part-i/
# https://finbloggers.wordpress.com/2016/04/13/the-efficient-frontier-with-portfolioanalytics-part-ii/
# https://finbloggers.wordpress.com/2016/05/23/the-efficient-frontier-with-portfolioanalytics-part-iii/
#
# # https://bookdown.org/sstoeckl/Tidy_Portfoliomanagement_in_R/s-4portfolios.html
#
# Configure the environment to have the right functionality.
# Make sure we have the PortfolioAnalytics and ROI and related packages installed
# Only run the install.packages function once.
# install.packages("ROI", "ROI.plugin.glpk", "ROI.plugin.quadprog")
# install.packages("PortfolioAnalytics")
#
# Make sure the PortfolioAnalytics package and the ROI optimiser packages are loaded.
library(ROI)
library(PortfolioAnalytics)
#------------------------------------------------------------------------------------------

# Initialise the portfolio specification using names of assets
portfolio <- portfolio.spec(assets=industries)

portfolio <- add.constraint(portfolio, type="full_investment")
#portfolio <- add.constraint(portfolio, type="long_only")

# Force weights on assets to sum to 1
#portfolio <- add.constraint(portfolio=portfolio, type="weight_sum",  min_sum=1, max_sum=1, enabled = TRUE)

# Make sure these bounds do not constrain the minimum variance or tangency portfolio weights!
# Upper and lower bound on weight for each industry
portfolio = add.constraint(portfolio=portfolio, type="box", min=-1, max=1)

# Add objectives
portfolio <- add.objective(portfolio=portfolio, type="risk", name="StdDev")
#portfolio <- add.objective(portfolio=portfolio, type="return", name="mean")

# Solve for the weights in the minimum variance portfolio of risky assets
optimisedMinimumVariancePortfolio <- optimize.portfolio(R=industryReturnsXTS, portfolio=portfolio, optimize_method="ROI", trace=TRUE)
print(optimisedMinimumVariancePortfolio)
plot(optimisedMinimumVariancePortfolio, risk.col="StdDev", return.col="mean", main="Minimum Variance Portfolio", chart.assets=TRUE)
(optimisedMinimumVariancerPortfolioReturn <- t(optimisedMinimumVariancePortfolio$weights) %*% industryMeanReturns)
(optimisedMinimumVariancerPortfolioStdDev <- optimisedMinimumVariancePortfolio$objective_measures$StdDev)
(sharpeRatio <- (optimisedMinimumVariancerPortfolioReturn - rf) /optimisedMinimumVariancePortfolio$objective_measures$StdDev)

# Generate the efficient frontier of risky-asset portfolios and show the security market line
efficientFrontier <- create.EfficientFrontier(R=industryReturnsXTS, portfolio = portfolio, type = "mean-sd", n.portfolios = 25)
summary(efficientFrontier)
chart.EfficientFrontier(efficientFrontier, match.col = "StdDev", 
                        element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "Sharpe ratio", rf = rf, tangent.line = TRUE,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21)
means = efficientFrontier$frontier[,c("mean")]
stddevs = efficientFrontier$frontier[,c("StdDev")]
sharpeRatios = (means - rf) / stddevs
max(sharpeRatios)
# Note this gets a dodgy tangency portfolio because we have not optimised the sharpe ratio.
(dodgyTangencyPortfolio = efficientFrontier$frontier[sharpeRatios == max(sharpeRatios),])
(dodgyTangencyPortfolioWeights = dodgyTangencyPortfolio[4:length(dodgyTangencyPortfolio)])

# Define the sharpe ratio calculation function - that nests a portfolio optimisation problem.
frontierPortfolioSharpeRatio <- function(expectedReturn) {
  testPortfolio <- add.constraint(portfolio=portfolio, type="return", return_target=expectedReturn)
  optimisedTestPortfolio <- optimize.portfolio(R=industryReturnsXTS, portfolio=testPortfolio, optimize_method="ROI", trace=TRUE)
  return <- t(optimisedTestPortfolio$weights) %*% industryMeanReturns
  sharpeRatio <- (return - rf) /optimisedTestPortfolio$objective_measures$StdDev
  -sharpeRatio
}

# Get the sharpe ratio of portfolio that minimises variance given a required rate of return
# Initial guess of tangency portfolio required return is minimum variance portfolio expected return. Try others.
results <- optim(mean(industryMeanReturns), frontierPortfolioSharpeRatio, method="BFGS")

# Get details of the tangency portfolio
# Start by imposing target portfolio return equal to known tangency portfolio return based on optimisation results.
tangencyPortfolioRequiredReturn <- results$par
tangencyPortfolioDefinition <- add.constraint(portfolio=portfolio, type="return", return_target=tangencyPortfolioRequiredReturn)
tangencyPortfolio <- optimize.portfolio(R=industryReturnsXTS, portfolio=tangencyPortfolioDefinition, optimize_method="ROI", trace=TRUE)
(tangencyPortfolioReturn <- t(tangencyPortfolio$weights) %*% industryMeanReturns)
tangencyPortfolio$objective_measures
(sharpeRatio <- (tangencyPortfolioReturn - rf) /tangencyPortfolio$objective_measures$StdDev)
tangencyPortfolio$weights
plot(tangencyPortfolio, risk.col="StdDev", return.col="mean", main="Tangency Portfolio", chart.assets=TRUE)

# Plot the weights along the efficient frontier
par(mfrow=c(1,1))
chart.EF.Weights(efficientFrontier, main= "Weights on the Efficient Frontier", match.col="StdDev", colorset=rainbow(n = length(industries)))
