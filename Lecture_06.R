#-------------------------------------------------------------------------------------#
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 06: Multifactor models and Principle Component Analysis
#
# Topics:
#
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
# Principle components analysis
# - Packages: corrplot for correlation matrix graphs and factoextra for PCA graphical analysis
#-------------------------------------------------------------------------------------

# Run the install packages command once if factoextra is not installed already.
# install.packages("factoextra")
library(factoextra)

# Run the install packages command once if corrplot is not installed already.
# install.packages("corrplot")
library(corrplot)

#-------------------------------------------------------------------------------------
# Analysis of relationship of industry return principle components and factors.
#-------------------------------------------------------------------------------------

# Generate the principle components as i.i.d. standard normal variates (these are typically not observed)
observationCount = 1000
componentCount <- 2
principleComponents <- matrix( rnorm(observationCount*componentCount,mean=0,sd=1), observationCount, componentCount) 

# Confirm rows and columns of the matrix containing the principle components
dim(principleComponents)

# Create square matrix of weights that maps the principle components to the observed data 
# Initial mapping is just 1:1 - each observed variable is a principle component
weights = diag(replicate(1,componentCount))

# Try increasing the multiple of the first principle component that gives us the first observed variable
# What effect does this have on the ability to identify the "true" principle component with scaling TRUE and FALSE.
weights[1,1] = 1

# Alternatively try to make the second observed variable a larger multiple of the first principle component
# What effect does this have on the ability to identify the "true" principle component with scaling TRUE and FALSE.
weights[1,1] = 4

data <- principleComponents %*% weights

# Confirm rows and columns of the matrix containing the observed data
dim(data)

# Do the principle components analysis: Try with all observed variables scaled to have the same standard deviation (and without).
pcaResults <- prcomp(data, center = TRUE, scale. = FALSE)
(eigenvalues = get_eig(pcaResults))
fviz_eig(pcaResults)

#fviz_pca_var(pcaResults, axes = c(1, 2))
#fviz_pca_var(pcaResults, axes = c(3, 4))

estimatedPrincipleComponents <- pcaResults$x
dim(estimatedPrincipleComponents)

# Concatenate (columnwise) actual principle components and estimated principle components
pcs <- data.frame(cbind(principleComponents,estimatedPrincipleComponents))
dim(pcs)

# Plot the correlations and focus attention on the top right quadrant.
corrplot(cor(pcs), method = "ellipse")

pc1Model <- lm(V1 ~ 0 + PC1 + PC2, data = pcs)

# Scatter plot of predicted and actual values of variable 1
plot(predict(pc1Model), pcs$V1)

# What do you notice about the coefficient values on the two principle components.
# Run it several times with different random samples - does anything change between runs?
summary(pc1Model)

pc1Modelb <- lm(V1 ~ 0 + PC1, data = pcs)

# Scatter plot of predicted and actual values of variable 1
plot(predict(pc1Modelb), pcs$V1)

# Compare the coefficient value to the one obtained from the regression on two regressors.
summary(pc1Modelb)

#-------------------------------------------------------------------------------------
# Analysis of relationship of industry return principle components and factors.
#-------------------------------------------------------------------------------------

# Load the data
library(readxl)

library(xts)

# Read data from spreadsheet in same folder as this R script.
# Note these are the value-weighted returns for each industry
data <- read_excel("Fama_French_industry_monthly_returns.xlsx")

# Get the dates for each observation
dates <- as.Date(data$Date)

# Get names of industries
industries <- colnames(data[9:ncol(data)])

# Remove the non-numeric data columns
data <- data[,5:ncol(data)]

# Convert market and industry returns into excess returns while still working with a data frame.
data$RM = (data$RM - data$RF)
data[,industries] = (data[,industries] - data$RF)

# Create the extended timeseries object needed for portfolio analytics
dataXTS <- xts(data, order.by=dates)

# Normalise data (zero mean and unit standard deviation)
help(scale) # Find out more about the scaling function
normDataXTS <- dataXTS
normDataXTS$RM = scale(dataXTS$RM)
normDataXTS$SMB = scale(dataXTS$SMB)
normDataXTS$HML = scale(dataXTS$HML)
normDataXTS[,industries] = scale(data[,industries])

# Choose the time span of interest
timespan = "19800101/20001231"

# Plot correlation structure among the three factors in the fama-french 3 factor model
corrplot(cor(dataXTS[timespan][,c("RM", "SMB", "HML", industries)]), method = "ellipse")

# Do PCA
pca <- prcomp(normDataXTS[timespan][,c("PerSv","BusSv","Hardw", "Softw", "Chips", "LabEq")], center=TRUE, scale.=TRUE)

# Get the eigenvalues and associated information
(eigenvalues = get_eig(pca))

# Note that eigenvalues obtain from the computed standard deviations of the principle components
pca$sdev^2

#Visualse the importance of each principle component
fviz_eig(pca)

# Visualise industry returns, explained by the various principle components.
fviz_pca_var(pca,axes = c(1, 2))
fviz_pca_var(pca,axes = c(3, 4))
fviz_pca_var(pca,axes = c(5, 6))

# Get the principle components and augment with the market excess return
principleComponents <- as.data.frame(pca$x)
principleComponents$RM <- dataXTS[timespan]$RM

# Regress the factor on various sets of principle components of the industry returns
summary(lm(RM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = principleComponents))




