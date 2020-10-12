#-------------------------------------------------------------------------------------#
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 06: Principal Component Analysis
#
# Topics:
#
# - Principal component analysis and interpretation.
#
# Data source (Ken French)
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Benchmarks
#
# Demos: Good practice and great graphics for PCA using R.
# https://rpubs.com/esobolewska/pcr-step-by-step
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Principal components analysis
# - Packages: corrplot for correlation matrix graphs and factoextra for PCA graphical analysis
#-------------------------------------------------------------------------------------

# Run the install packages command once if factoextra is not installed already.
# Useful for visualisation of PCA results.
# install.packages("factoextra")
library(factoextra)

# Run the install packages command once if corrplot is not installed already.
# install.packages("corrplot")
library(corrplot)

# install.packages("MASS")
library(MASS)

# install.packages("GMCM")
# For column standard deviation function
library(GMCM)

#-------------------------------------------------------------------------------------
# Round trip the data: 
# original data 
# to standardised data 
# to principal components 
# to standardised data 
# to original data
#-------------------------------------------------------------------------------------

# Generate the data
n = 100
k <- 2

set.seed(1) 

# list of means - both equal to 0
means <- as.matrix(replicate(k,0))

# Create the variance covariance with a 0.9 correlation between x1 and x2
vcov = diag(replicate(k,1))
vcov[1,2] <- 0.9
vcov[2,1] <- vcov[1,2]

originalData <- MASS::mvrnorm(n=n, mu = means, Sigma = vcov)

# Check original data - sampling variation causes deviation from population parameters
(originalMeans <- colMeans(originalData))
(originalStdDevs <- GMCM:::colSds(originalData))

# Plot the correlations of the original data
corrplot(cor(originalData), method = "ellipse")

# Standardise the data (mean zero and unit standard deviation/variance)
help(scale)
standardisedData <- scale(originalData, center = TRUE, scale = TRUE)

# Check standardised data means and standard deviations
colMeans(standardisedData)
GMCM:::colSds(standardisedData)

# Plot the correlations of the standardised data
corrplot(cor(standardisedData), method = "ellipse")

# Do the eigenvalue decomposition of the variance covariance matrix of the scaled data.
pca1Results <- prcomp(standardisedData, center = FALSE, scale. = FALSE)
summary(pca1Results)
# alternatively do the same thing with:
pcaResults <- prcomp(originalData, center = TRUE, scale. = TRUE)
summary(pcaResults)

# Get the eigenvalues
(eigenvalues = get_eig(pcaResults))

# Compute the percentage of variation explained by each principal component.
(eigenvalues$eigenvalue / sum(eigenvalues$eigenvalue))

# View the percentage of variation explained by each eigenvector.
fviz_eig(pcaResults)

# Get the eigenvectors
(eigenvectors <- pcaResults$rotation)

# Have a look at how the axes have been rotated to find the principal components.
# Change the sign of the correlation when generating the original data and recreate these graphs.
help(fviz_pca_var)
fviz_pca_biplot(pcaResults, axes = c(1, 2)) +
  xlim(-4, 4) + ylim (-4, 4)
fviz_pca_var(pcaResults, axes = c(1, 2)) 

# get the principal components
principalComponents <- pcaResults$x
dim(principalComponents)

# Plot the correlations of the principal components - they are orthogonal
corrplot(cor(principalComponents), method = "ellipse")

# Get the standardised data back from the principal components
newStandardisedData <- principalComponents %*% solve(eigenvectors)

# Compare the first few observations to make sure we got back what we wanted.
head(newStandardisedData)
head(standardisedData)

# Get the original data from the principal components by unscaling (reversing the scale operation in two steps to get operation ordering right)
newOriginalData <- scale((principalComponents %*% solve(eigenvectors)), center = FALSE, scale = (1 / originalStdDevs))
GMCM:::colSds(newOriginalData)
newOriginalData <- scale(newOriginalData, center = -originalMeans, scale = FALSE)
colMeans(newOriginalData)

# Compare the first few observations to make sure we got back what we wanted.
head(newOriginalData)
head(originalData)

#-------------------------------------------------------------------------------------
# Explore the impact of not scaling on the principal component construction
# Try running this experiment with different correlation in the original data.
#-------------------------------------------------------------------------------------

# Generate the data
n = 100
k <- 2

set.seed(1) 

# list of means - both equal to 0
means <- as.matrix(replicate(k,0))

# Create the variance covariance with a 0.9 correlation between x1 and x2
vcov = diag(replicate(k,1))
vcov[1,2] <- 0
vcov[2,1] <- vcov[1,2]

originalData <- MASS::mvrnorm(n=n, mu = means, Sigma = vcov)

# Increase scale of the first variable
scaleFactor <- 10
originalData[,1] <- originalData[,1] * scaleFactor

# Do the principal components analysis: Try with all observed variables 
# scaled to have the same standard deviation (and without).
pcaResultsWithoutScaling <- prcomp(originalData, center = TRUE, scale. = FALSE)
fviz_eig(pcaResultsWithoutScaling)
fviz_pca_biplot(pcaResultsWithoutScaling, axes = c(1, 2)) +xlim(-40, 40) + ylim (-40, 40)

# Do the principal components analysis: Try with all observed variables 
# scaled to have the same standard deviation (and without).
pcaResultsWithScaling <- prcomp(originalData, center = TRUE, scale. = TRUE)
fviz_eig(pcaResultsWithScaling)
fviz_pca_biplot(pcaResultsWithScaling, axes = c(1, 2)) +xlim(-10, 10) + ylim (-10, 10)

#-------------------------------------------------------------------------------------
# Analysis of principal components for industry returns data
# Compare first principal component to excess return on market.
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

# Get excess returns
data[,industries] = (data[,industries] - data$RF)
rmData = (data$RM - data$RF)

# Remove the non-industry data columns while creating XTS
dataXTS <- xts(data[,9:ncol(data)], order.by=dates)

# Generate excess returns on market as XTS for later comparison
rmXTS <- xts(rmData, order.by=dates)

# Choose the time span of interest
timespan = "19800101/20001231"

# Plot correlation structure among the three factors in the fama-french 3 factor model
corrplot(cor(dataXTS[timespan]), method = "ellipse")

# Do PCA
pca <- prcomp(dataXTS[timespan], center=TRUE, scale.=TRUE)

# Get the eigenvalues and associated information
(eigenvalues = get_eig(pca))

# Note that eigenvalues are related to the standard deviations of the principal components
pca$sdev^2

# Visualise the importance of each principal component
fviz_eig(pca)

# Get the principal components and augment with the market excess return
principalComponents <- as.data.frame(pca$x)

# Plot the first principal component against the excess return on the market.
scatter.smooth(principalComponents[,1], rmXTS[timespan])

#-------------------------------------------------------------------------------------
# Removing the wrong principal component for multicollinearity.
#-------------------------------------------------------------------------------------

# Generate the data
n = 30
k <- 2

set.seed(1) 

# list of means - both equal to 0
means <- as.matrix(replicate(k,0))

# Create the variance covariance with a 0.9 correlation between x1 and x2
vcov = diag(replicate(k,1))
vcov[1,2] <- 0.95
vcov[2,1] <- vcov[1,2]

originalData <- MASS::mvrnorm(n=n, mu = means, Sigma = vcov)

# Do PCA
pca <- prcomp(originalData, center=TRUE, scale.=TRUE)
fviz_eig(pca)
principalComponents <- as.data.frame(pca$x)

data <- cbind(originalData, principalComponents)
colnames(data) <- c("x1", "x2", "p1", "p2")

data$y = data$p2 + rnorm(n,mean = 0, sd = 1.95)

corrplot(cor(data), method = "ellipse")

summary(lm(y~x1+x2, data=data))

summary(lm(y~p1, data=data))

summary(lm(y~p2, data=data))
