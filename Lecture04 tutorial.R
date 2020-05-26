#-------------------------------------------------------------------------------------
# USAGE OF R IS NOT ASSESSABLE FOR ECMT2130.
#
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
# Introduction to PCA in R
# https://www.datacamp.com/community/tutorials/pca-analysis-r


#-------------------------------------------------------------------------------------

data(mtcars)

# Do the decomposition for all numeric variables
mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)
summary(mtcars.pca)
str(mtcars.pca)

#install.packages("glue") 
#install.packages("devtools") 
library(glue)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(mtcars.pca)

# Load the data
# Make sure that the readxl package of functionality is available.
# Run the following command if readxl is not installed already.
# install.packages("readxl")
library(readxl)
 
# Read data from spreadsheet in same folder as this R script.
myData <- read_excel("Tutorial03_industry_monthly_returns.xlsx")
