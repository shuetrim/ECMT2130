#-------------------------------------------------------------------------------------#
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 06: Multifactor models and Principle Component Analysis
#
# Topics:
#
# - Industry Portfolios
# - Multiple regression complex restrictions on coefficients.
# - Principle component analysis and interpretation.
#
# Data source (Ken French)
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Benchmarks
#
# Industry definitions
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_49_ind_port.html
# 
# Demos: Good practice and great graphics.
# https://rpubs.com/esobolewska/pcr-step-by-step
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Multiple regression model.
# Population model: y = 1 + 1 * X_1 + 1 * X_2 + u
#-------------------------------------------------------------------------------------

# Simulate small number of observations

n = 30
beta_0 = 1.0
beta_1 = 1.0
beta_2 = 1.25

# Generate the regressors and error term as i.i.d. standard normal variates
X_1 = rnorm(n)
X_2 = rnorm(n)
u = rnorm(n)

# Create the dependent variable
y = beta_0 + beta_1 * X_1 + beta_2 * X_2 + u

# Create a data frame to contain our data.
ourData <- data.frame(y, X_1, X_2, u)

unrestrictedModel <- lm(y ~ X_1 + X_2, ourData)
unrestrictedModelSummary <- summary(unrestrictedModel)
unrestrictedModelSummary

#-------------------------------------------------------------------------------------
# Starting simple: Test whether beta_1 = 0
#-------------------------------------------------------------------------------------

# Get coefficient and standard error.
beta1Hat = coef(unrestrictedModelSummary)["X_1","Estimate"]
seBeta1Hat = coef(unrestrictedModelSummary)["X_1","Std. Error"]

# Create t ratio (exclusion test statistic for beta_1)
tStar = beta1Hat / seBeta1Hat


# Get the p-value for the t ratio (2-tailed test)
help("pt")
pValue = ifelse( tStar < 0, 2*pt(tStar, n-3), 2*(1-pt(tStar, n-3))) 

# Compare scaled p-values constructed manually and in the regression summary.
pValue
coef(unrestrictedModelSummary)["X_1","Pr(>|t|)"]

#-------------------------------------------------------------------------------------
# Test whether beta_1 = beta_2
#-------------------------------------------------------------------------------------

# Method 1 - construct t-statistic.
beta1Hat = coef(unrestrictedModelSummary)["X_1","Estimate"]
beta2Hat = coef(unrestrictedModelSummary)["X_2","Estimate"]

# Get the estimated variance covariance matrix for the coefficients.
vCovHat = vcov(unrestrictedModel)
varBeta1Hat = vCovHat["X_1","X_1"]
varBeta2Hat = vCovHat["X_2","X_2"]
covHat = vCovHat["X_1","X_2"]
varHat = varBeta1Hat + varBeta2Hat - 2*covHat
seHat = sqrt(varHat)
tStar = (beta1Hat - beta2Hat) / seHat
pValue = ifelse( tStar < 0, 2*pt(tStar, n-3), 2*(1-pt(tStar, n-3)))
pValue

# Method 2 - construct new regressor and use the reported p-value on X_2 t-ratio
ourData$XSum <- ourData$X_1 + ourData$X_2
transformedUnrestrictedModel <- lm(y ~ XSum + X_2, ourData)
transformedUnrestrictedModelSummary <- summary(transformedUnrestrictedModel)
transformedUnrestrictedModelSummary

pValue = coef(transformedUnrestrictedModelSummary)["X_2","Pr(>|t|)"]
pValue

# Method 3 - F test of restriction
restrictedModel <- lm(y ~ XSum, ourData)
restrictedModelSummary <- summary(restrictedModel)
restrictedModelSummary
# Use built in F-testing function in R:
pValue = anova(unrestrictedModel, restrictedModel)[2,"Pr(>F)"]
pValue

# Or to be cool, construct the F statistic manually from R_squared values
restrictedRsquared = restrictedModelSummary[["r.squared"]]
unrestrictedRsquared = unrestrictedModelSummary[["r.squared"]]
fStar = ( (unrestrictedRsquared - restrictedRsquared) / 1 ) / ( (1 - unrestrictedRsquared) / (n-3) )
pValue = 1-pf(fStar,1,n-3)
pValue

# Or, construct the F statistic manually from restricted and unrestricted sums of squared residuals
restrictedSSR = sum( restrictedModel$resid^2 )
unrestrictedSSR = sum( unrestrictedModel$resid^2 )
fStar = ( (restrictedSSR - unrestrictedSSR) / 1 ) / ( (unrestrictedSSR) / (n-3) )
pValue = 1-pf(fStar,1,n-3)
pValue

#-------------------------------------------------------------------------------------
# Analysis of relationship of industry return principle components and factors.
#-------------------------------------------------------------------------------------

# Run the install packages command once if factoextra is not installed already.
# install.packages("factoextra")
library(factoextra)

# Run the install packages command once if corrplot is not installed already.
# install.packages("corrplot")
library(corrplot)

# Load the data
# Make sure that the readxl package of functionality is available.
# Run the following command once if readxl is not installed already.
# install.packages("readxl")
library(readxl)

# Read data from spreadsheet in same folder as this R script.
myData <- read_excel("Fama_French_industry_monthly_returns.xlsx")

myData <- myData[myData$Year > 2000,]

# Get excess returns for market and industries, over the risk free return.
myData[,9:ncol(myData)] <- myData[,9:ncol(myData)] - myData$RF
myData$RM <- myData$RM - myData$RF

# Plot correlation structure.
res <- cor(myData[,c(5,6,7,9:ncol(myData))], method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust", tl.pos = 'n')

# Get excess industry returns as the data to apply PCA to.
xData <- myData[,9:ncol(myData)]

# Normalise the data to zero mean and unit variance
xData.norm <- data.frame(scale(xData))

# Do PCA
xData.pca <- prcomp(xData.norm, center=TRUE, scale.=TRUE)

# Visualise importance of each principle component
fviz_eig(xData.pca)

# Visualise industry returns, explained by the first 2 principle components.
fviz_pca_var(xData.pca,axes = c(1, 2))

principleComponents <- as.data.frame(xData.pca$x)

# Get the variable to relate to to the principle components of the industry excess returns
# Change SMB to HML or RM as required...
yData.norm <- data.frame(scale(myData$RM))
colnames(yData.norm)<- c("y")

ols.data <- cbind(yData.norm, principleComponents)

# Regress the factor on various sets of principle components of the industry returns
summary(lm(y ~ ., data = ols.data))
summary(lm(y ~ PC1 + PC2 + PC3 + PC4 + PC5, data = ols.data))
summary(lm(y ~ PC1 + PC2 + PC3 + PC4, data = ols.data))
summary(lm(y ~ PC1 + PC2 + PC3, data = ols.data))
summary(lm(y ~ PC1 + PC2, data = ols.data))
summary(lm(y ~ PC1, data = ols.data))

summary(xData.pca)



