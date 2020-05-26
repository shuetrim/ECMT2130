#-------------------------------------------------------------------------------------#
# Written by Geoff Shuetrim
# Tutorial 04: Principle Component Analysis of Industry Portfolios
#
# Covers:
#
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
myData <- read_excel("Tutorial03_industry_monthly_returns.xlsx")

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



