#-------------------------------------------------------------------------------------#
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 05: Tests of the CAPM
#
# Data source (Ken French)
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Benchmarks
#
# Industry definitions
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_49_ind_port.html
#
# Topics:
#
# - CAPM time series regression by industry over different time periods.
# 
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Load the relevant libraries
#-------------------------------------------------------------------------------------

# Log progress to help with debugging.
# install.packages("futile.logger")
library(futile.logger)

# Ensure that the logger reports debug messages.
flog.threshold(DEBUG)

library(readxl)

library(xts)

#-------------------------------------------------------------------------------------
# Load and organise the data
#-------------------------------------------------------------------------------------

# Read data from spreadsheet in same folder as this R script.
rawData <- read_excel("Fama_French_industry_monthly_returns.xlsx")

# Get the dates to create the XTS object
dates <- as.Date(rawData$Date)

# Remove the columns we are not interested in.
rawData$YearMonth <- NULL
rawData$Year <- NULL
rawData$Month <- NULL
rawData$SMB <- NULL
rawData$HML <- NULL

# Get the industry names
columnNames = names(rawData)
industryNames <- columnNames[4:length(columnNames)]

# Create the industry returns XTS object
industryReturns <- xts(rawData[,industryNames], order.by=dates)

# Create the risk free returns XTS object
riskFreeReturns <- xts(rawData$RF, order.by=dates)

# Create the market returns XTS object
marketReturns <- xts(rawData$RM, order.by=dates)

# Create the excess market return over the risk free return
excessMarketReturns <- marketReturns - riskFreeReturns

# Create the matrix or grid that will hold the regression output
results <- matrix(ncol=11, nrow=length(industryNames))

# Initialise counter over industries
i<-0

# Choose timespan for the regression
timespan <- "20000101/20091231"

# For examining out of sample performance
heldBacktimespan <- "20100101/20201231"

# Iterate over industries, printing out industry results.
for (industryName in industryNames) {
  
  flog.debug("Doing CAPM regression for the %s industry", industryName)
  i <- i + 1
  excessIndustryReturns <- industryReturns[,c(industryName)] - riskFreeReturns
  colnames(excessIndustryReturns) <- c("excessIndustryReturns")
  estimationData <- merge(excessIndustryReturns, excessMarketReturns)
  
  industryModel <- lm(excessIndustryReturns ~ excessMarketReturns, data=estimationData[timespan])
  regressionSummary <- summary(industryModel)
  coefficientDetails <- regressionSummary$coefficients
  # Extract the information to use in assessing industries
  alpha <- coefficientDetails["(Intercept)", "Estimate"]
  alphaT <- coefficientDetails["(Intercept)", "t value"]
  alphaP <- coefficientDetails["(Intercept)", "Pr(>|t|)"]
  beta <- coefficientDetails["excessMarketReturns", "Estimate"]
  betaT <- coefficientDetails["excessMarketReturns", "t value"]
  betaP <- coefficientDetails["excessMarketReturns", "Pr(>|t|)"]
  R <- regressionSummary$r.squared
  
  # Geometric weighted average returns for the industry
  inSampleIndustryReturn <- 100*( prod(1 + industryReturns[timespan][,industryName]/100 ) ^ (1/nrow(industryReturns[timespan])) - 1)
  inSampleMarketReturn <- 100*( prod(1 + marketReturns[timespan]/100 ) ^ (1/nrow(marketReturns[timespan])) - 1)
  inSampleRiskFreeReturn <- 100*( prod(1 + riskFreeReturns[timespan]/100 ) ^ (1/nrow(riskFreeReturns[timespan])) - 1)
  fittedIndustryReturn = 100*( (prod(1 + (riskFreeReturns[timespan] + alpha + beta * excessMarketReturns[timespan])/100 )) ^ (1/nrow(riskFreeReturns[timespan])) - 1 )
  fittedIndustryPerformance = fittedIndustryReturn - inSampleMarketReturn
  inSampleActualPerformance = (inSampleIndustryReturn - inSampleMarketReturn)
  
  outOfSampleIndustryReturn <- 100*( prod(1 + industryReturns[heldBacktimespan][,industryName]/100 ) ^ (1/nrow(industryReturns[heldBacktimespan])) - 1)
  outOfSampleMarketReturn <- 100*( prod(1 + marketReturns[heldBacktimespan]/100 ) ^ (1/nrow(marketReturns[heldBacktimespan])) - 1)
  outOfSampleActualPerformance = (outOfSampleIndustryReturn - outOfSampleMarketReturn)
  
  # Store the information in row i of the results grid
  results[i,] <- c(industryName, alpha, alphaT, alphaP, beta,  betaT, betaP, R, fittedIndustryPerformance, inSampleActualPerformance, outOfSampleActualPerformance)
  
}

# Turn the results grid into a data frame for easy inspection
results <- data.frame(as.data.frame(matrix(unlist(results), nrow=length(industryNames))))
colnames(results)<- c("industry","alpha","alpha t ratio","alpha p value", "beta", "beta t ratio", "beta p value", "R", "fitted-performance", "in-sample performance", "out-of-sample performance")
rownames(results)<-industryNames

# Double click on results in the global environment to inspect and analyse within R

# Make sure that numbers are recognised as numbers instead of "factors" in the results data frame.
indx <- sapply(results, is.factor)
indx["industry"] = FALSE
results[indx] <- lapply(results[indx], function(x) as.numeric(as.character(x)))

# Write results to CSV file if you want to analyse the output in Excel.
write.table(results, "Lecture_05_results.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)


