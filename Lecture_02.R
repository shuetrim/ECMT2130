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

# Read data from spreadsheet in same folder as this R script.
# Double click on "myData" in the environment tab to see that data in a tabular view. 
myData <- read_excel("Fama_French_industry_monthly_returns.xlsx")

# Remove all data before 2000.
myData <- myData[myData$Year > 2000,]

# Get list of industries used in the Excel analysis
industries <- c("Smoke", "Aero", "Fin", "Softw", "Hardw", "Drugs", "Hlth")

# Alternatively, for use in R, work with all industries (too hard in Excel, in my view)
# Note that the first 8 columns of myData are not industry returns.
industries <- colnames(myData)[9:ncol(myData)]

# Summary statistics - know your data!
summary(myData[,industries])

# Variance covariance matrix
var(myData[,industries])

# Correlations useful in understanding weights along efficient frontier.
cor(myData[,industries])

# Compute the average risk free rate of return. Check its value in the environment tab.
rf = colMeans(myData[,c("RF")])

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
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)
#------------------------------------------------------------------------------------------

# We need to understand that the data has a time dimension to do the portfolio analysis
# so convert the industry returns data frame to an extended timeseries object.
# This is required to use the returns in the PortfolioAnalytics package.
myDataXTS <- xts(myData[,industries], order.by=as.Date(myData$Date))

# Initialise the portfolio specification using names of assets
portfolio <- portfolio.spec(assets=industries)

portfolio <- add.constraint(portfolio, type="full_investment")
#portfolio <- add.constraint(portfolio, type="long_only")

# Force weights on assets to sum to 1
#portfolio <- add.constraint(portfolio=portfolio, type="weight_sum",  min_sum=1, max_sum=1, enabled = TRUE)

# Upper and lower bound on weight for each industry
portfolio = add.constraint(portfolio=portfolio, type="box", min=-20, max=20)

# Impose target portfolio return
#portfolio <- add.constraint(portfolio=portfolio, type="return", return_target=1.0)

# Add risk minimisation objective
portfolio <- add.objective(portfolio=portfolio, type="risk", name="var")

# Add return maximisation objective
#portfolio <- add.objective(portfolio=portfolio, type="return", name="return")

# Show the efficient frontier with and without the risk-free asset.
efficientFrontier <- create.EfficientFrontier(R=myDataXTS, portfolio = portfolio, type = "mean-sd", n.portfolios = 25)
summary(efficientFrontier)

par(mfrow=c(1,1))
chart.EfficientFrontier(efficientFrontier, match.col = "StdDev", 
                        element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "Sharpe ratio", rf = rf, tangent.line = TRUE,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21)

# Solve for the weights in the minimum variance portfolio of risky assets
optimalWeightsInMinimumVariancePortfolio <- optimize.portfolio(R=myDataXTS, portfolio=portfolio, optimize_method="ROI", trace=TRUE)
print(optimalWeightsInMinimumVariancePortfolio)
plot(optimalWeightsInMinimumVariancePortfolio, risk.col="StdDev", return.col="mean", main="Minimum Variance Optimization", chart.assets=TRUE)

# Solve for the weights in the minimum variance portfolio of risky assets
optimalWeightsInTangencyPortfolio <- optimize.portfolio(R=myDataXTS, portfolio=portfolio, optimize_method="ROI", trace=TRUE, maxSR = TRUE)
print(optimalWeightsInTangencyPortfolio)
plot(optimalWeightsInTangencyPortfolio, risk.col="StdDev", return.col="mean", main="Minimum Variance Optimization", chart.assets=TRUE)


# Plot the weights along the efficient frontier
par(mfrow=c(1,1))
chart.EF.Weights(efficientFrontier, main= "Weights on the Efficient Frontier", match.col="StdDev", colorset=rainbow(n = length(industries)))
