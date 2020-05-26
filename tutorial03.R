#-------------------------------------------------------------------------------------
# USAGE OF R IS NOT ASSESSABLE FOR ECMT2130.
#
# Written by Geoff Shuetrim
# Tutorial 03: Linear regression to analyse abnormal stock returns (Jensen 1967)
#
# Data source (Ken French)
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Benchmarks
#
# Industry definitions
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_49_ind_port.html
# 
#-------------------------------------------------------------------------------------

# Load the data
# Make sure that the readxl package of functionality is available.
# Run the following command if readxl is not installed already.
# install.packages("readxl")
library(readxl)

# Read data from spreadsheet in same folder as this R script.
myData <- read_excel("Tutorial03_industry_monthly_returns.xlsx")

# Create the excess market return over the risk free return
myData$excessMarketReturn = myData$RM - myData$RF

# Move excess market return to be before the industry returns
names = names(myData)
names = names[c(1:8, length(names), 9:(length(names)-1))]
myData <- myData[, names]

# Get a list of industry names to iterate over
industryNames <- names(myData)[10:ncol(myData)]

# Create the matrix or grid that will hold the regression output
results <- matrix(ncol=11, nrow=length(industryNames))

# Initialise counter over industries
i<-0

# Iterate over industries, printing out industry results.
for (industryName in industryNames) {
  print(paste("Industry:", industryName), row.names = FALSE)
  i <- i + 1
  info <- myData[,c(industryName)] - myData[,c("RF")]
  myData$excessIndustryReturn <- info[,1]
  
  # Estimate using monthly data from 1970 or later to 2009 inclusive (Data is missing for some industries before 1970)
  estimationData <- subset(myData,myData$Year>=1970 & myData$Year<2010)
  myModel <- lm(excessIndustryReturn ~ excessMarketReturn, data=estimationData)
  mySummary <- summary(myModel)

  print(mySummary)
  # Extract the information to use in assessing industries
  alpha <- mySummary[["coefficients"]]["(Intercept)", "Estimate"]
  alphaT <- mySummary[["coefficients"]]["(Intercept)", "t value"]
  alphaP <- mySummary[["coefficients"]]["(Intercept)", "Pr(>|t|)"]
  beta <- mySummary[["coefficients"]]["excessMarketReturn", "Estimate"]
  betaT <- mySummary[["coefficients"]]["excessMarketReturn", "t value"]
  betaP <- mySummary[["coefficients"]]["excessMarketReturn", "Pr(>|t|)"]
  R <- mySummary[["r.squared"]]

  # Measure out of sample alpha  
  heldBackData <- subset(myData,myData$Year>=2010)

  inSampleIndustryReturn <- 100*( prod(1 + estimationData[,industryName]/100 ) ^ (1/nrow(estimationData)) - 1)
  inSampleMarketReturn <- 100*( prod(1 + estimationData[,"RM"]/100 ) ^ (1/nrow(estimationData)) - 1)
  inSampleRiskFreeReturn <- 100*( prod(1 + estimationData[,"RF"]/100 ) ^ (1/nrow(estimationData)) - 1)
  expectedPerformance = alpha + (beta-1) * (inSampleMarketReturn - inSampleRiskFreeReturn)
  inSampleActualPerformance = (inSampleIndustryReturn - inSampleMarketReturn)

  outSampleIndustryReturn <- 100*( prod(1 + heldBackData[,industryName]/100 ) ^ (1/nrow(heldBackData)) - 1)
  outSampleMarketReturn <- 100*( prod(1 + heldBackData[,"RM"]/100 ) ^ (1/nrow(heldBackData)) - 1)
  outSampleRiskFreeReturn <- 100*( prod(1 + heldBackData[,"RF"]/100 ) ^ (1/nrow(heldBackData)) - 1)
  outSampleActualPerformance = (outSampleIndustryReturn - outSampleMarketReturn)
  
  # Store the information in row i of the results grid
  results[i,] <- c(industryName, alpha, alphaT, alphaP, beta,  betaT, betaP, R, expectedPerformance, inSampleActualPerformance, outSampleActualPerformance)
  
}

# Turn the results grid into a data frame for easy inspection
results <- data.frame(as.data.frame(matrix(unlist(results), nrow=length(industryNames))))
colnames(results)<- c("industry","alpha","alpha t ratio","alpha p value", "beta", "beta t ratio", "beta p value", "R", "expected performance", "in sample performance", "out sample performance")
# Double click on results in the global environment to inspect and analyse within R

# Make sure that numbers are recognised as numbers instead of "factors" in the results data frame.
indx <- sapply(results, is.factor)
indx["industry"] = FALSE
results[indx] <- lapply(results[indx], function(x) as.numeric(as.character(x)))

# Write results to CSV file if you want to analyse the output in Excel.
write.table(results, "Tutorial03_results.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

