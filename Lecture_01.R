#------------------------------------------------------------------------------------------
#
# ECMT2130 Financial Econometrics
# Lecture 1
#
#
# Topic 1: Portfolio optimization
#
#
# See also http://www.finance-r.com/
#
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# Configure the environment to have the right functionality.
#
# Make sure we have the PortfolioAnalytics package installed
# Only run this install.packages function once.
install.packages("PortfolioAnalytics")
#
# Make sure the PortfolioAnalytics package is loaded.
library(PortfolioAnalytics)
#
# Documentation:
# https://bookdown.org/sstoeckl/Tidy_Portfoliomanagement_in_R/s-4portfolios.html
# https://cran.r-project.org/web/packages/PortfolioAnalytics/vignettes/portfolio_vignette.pdf
# https://cran.csiro.au/web/packages/PortfolioAnalytics/PortfolioAnalytics.pdf
#------------------------------------------------------------------------------------------

# Load the data to analyse
data(edhec)
R <- edhec[, 1:6]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQMN", "ED")
funds <-colnames(R)

# Initialise the portfolio specification using names of assets
init <- portfolio.spec(assets=funds)

# Force weights on assets to sum to 1
init <- add.constraint(portfolio=init, type="weight_sum",  min_sum=1, max_sum=1)

# Force weights to be non-negative
#init <- add.constraint(portfolio=init, type="box", min=0.0, max=1.0)

# Impose target portfolio return
#init <- add.constraint(portfolio=init, type="return", return_target=0.014)

# Add risk minimisation objective
minvar <- add.objective(portfolio=init, type="risk", name="var")

# Show the efficient frontier
efficientFrontier <- create.EfficientFrontier(R=R, portfolio = minvar, type = "mean-sd", n.portfolios = 25)
chart.EfficientFrontier(efficientFrontier, match.col = "StdDev", n.portfolios = 25, xlim=NULL, ylim=NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0.001, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

# Solve for the optimal weights in the portfolio
opt_minvar <- optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI", trace=TRUE)
print(opt_minvar)
plot(opt_minvar, risk.col="StdDev", return.col="mean", main="Minimum Variance Optimization", chart.assets=TRUE, xlim=c(0, 0.05), ylim=c(0,0.015))

