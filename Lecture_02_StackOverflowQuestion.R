library(ROI)
library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:5]
# change the column names for better legends in plotting
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQM")
funds <- colnames(R)

# Mean returns for each asset
(assetMeanReturns <- colMeans(R))

(rf <- min(assetMeanReturns) / 2.0)

# Initialise the portfolio specification using names of assets
portfolio <- portfolio.spec(assets=funds)
portfolio <- add.constraint(portfolio, type="full_investment")
portfolio <- add.constraint(portfolio=portfolio, type="box", min=-1, max=1)
portfolio <- add.objective(portfolio=portfolio, type="risk", name="StdDev")

iterations <-20
results <- matrix(nrow = iterations, ncol = 8)

for (i in 1:iterations) {
  efficientFrontier <- create.EfficientFrontier(R=R, portfolio=portfolio, type="mean-sd", n.portfolios=4+i)
  (means <- efficientFrontier$frontier[,c("mean")] * 100)
  (stddevs <- efficientFrontier$frontier[,c("StdDev")] * 100)
  (sharpeRatios <- (means - rf*100) / stddevs)
  (tangencyPortfolio <- efficientFrontier$frontier[sharpeRatios == max(sharpeRatios),])
  tangencyPortfolioDetails <- efficientFrontier$frontier[sharpeRatios == max(sharpeRatios),1:ncol(efficientFrontier$frontier)]
  results[i,] <- efficientFrontier$frontier[sharpeRatios == max(sharpeRatios),1:ncol(efficientFrontier$frontier)]  
}

results

# Define the sharpe ratio calculation function - that nests a portfolio optimisation problem.
frontierPortfolioSharpeRatio <- function(expectedReturn) {
  testPortfolio <- add.constraint(portfolio=portfolio, type="return", return_target=expectedReturn)
  optimisedTestPortfolio <- optimize.portfolio(R=R, portfolio=testPortfolio, optimize_method="ROI", trace=TRUE)
  return <- t(optimisedTestPortfolio$weights) %*% assetMeanReturns
  sharpeRatio <- (return - rf) /optimisedTestPortfolio$objective_measures$StdDev
  -sharpeRatio
}

# Get the sharpe ratio of portfolio that minimises variance given a required rate of return
# Initial guess of tangency portfolio required return is minimum variance portfolio expected return. Try others.
results <- optim(mean(assetMeanReturns), frontierPortfolioSharpeRatio, method="BFGS")

tangencyPortfolioRequiredReturn <- results$par
tangencyPortfolioDefinition <- add.constraint(portfolio=portfolio, type="return", return_target=tangencyPortfolioRequiredReturn)
tangencyPortfolio <- optimize.portfolio(R=R, portfolio=tangencyPortfolioDefinition, optimize_method="ROI", trace=TRUE)
(tangencyPortfolioReturn <- t(tangencyPortfolio$weights) %*% assetMeanReturns)
tangencyPortfolio$objective_measures
(sharpeRatio <- (tangencyPortfolioReturn - rf) /tangencyPortfolio$objective_measures$StdDev)
tangencyPortfolio$weights
plot(tangencyPortfolio, risk.col="StdDev", return.col="mean", main="Tangency Portfolio", chart.assets=TRUE)
