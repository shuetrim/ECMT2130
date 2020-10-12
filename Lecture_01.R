#------------------------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 01: Introduction to R
#
# Topics:
#
# - Getting to know R and R-Studio
#
# - R Studio installation instructions from Sydney University 
#   (https://sydney.edu.au/students/student-it/apps.html)
#   Look under the list of Apps to download onto your own device.
#   You can do this yourself, if you wish to execute this script.
#
# - Introduction to using R Studio including installation help
#   (10 minutes: https://datascienceplus.com/introduction-to-rstudio/)
#   You can do this yourself, if you wish to execute this program.
#
# - Basic calculations 
#   (https://www.cyclismo.org/tutorial/R/basicOps.html)
#   Lots here to extend on this script.
#
# - Storing data in R including lists of values (data samples)
#   (http://michaelminn.net/tutorials/r-representing-data/)
#
#
# - Matrix operations
# Some sources of assistance:
# https://www.statmethods.net/advstats/matrix.html
# https://www.datamentor.io/r-programming/vector/
# https://www.datamentor.io/r-programming/matrix/
#
# - Descriptive statistics
#   (https://www.statmethods.net/stats/descriptives.html)
#
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------------------

y <- function(x1, x2, x3) {
  
  x1^2+x2^4+x3^6
  
}

y(3,5,6)

#------------------------------------------------------------------------------------------
# Basic calculations
#------------------------------------------------------------------------------------------

# Simple numbers
10
12.5
exp(1)
log(exp(1)) # log() is the natural log function.

#------------------------------------------------------------------------------------------
# Basic calculations
#------------------------------------------------------------------------------------------

#Multiplication and division
10 * 4 / 2 # gives 20

#Addition and subtraction
10 - 5 + -0.5 # gives 4.5

# powers
10^2 # gives 100
16.0^0.5 # gives 4
sqrt(9) # gives 3

# Natural logs (base e)

# gives 1.3 
log(exp(1.3)) 
# because exp(3) is e to the power of 3) 
# and log(.) is the log to base e.

#------------------------------------------------------------------------------------------
# Storing results in variables in the global environment 
#------------------------------------------------------------------------------------------

a <- 10^3
# See the newly created value for 'a' (1000) in the environment 
# tab in top right panel.
# alternative = syntax
a = 10^2

# Show the value of a
a

# Get the datatype of a
typeof(a)

# Storing text in "character strings"
textValue <- "Hello, world!"
textValue
typeof(textValue)

# Boolean (true/false) values
y <- 7 < 5
y
typeof(y)

# The square root of a is 10 because a=100
sqrt(a) 

# Comparisons
sqrt(a) == 10

#------------------------------------------------------------------------------------------
# binomial theorem
#------------------------------------------------------------------------------------------

help(choose)
choose(4,2)

#------------------------------------------------------------------------------------------
# Lists of values
#------------------------------------------------------------------------------------------

# Create a list of values called X. This is a data sample.
X <- c(1,2,3,5,3,7)

# See the data type of the values stored in X.
# Have a look at X in the environment tab also!
typeof(X)

# View X
X

# get 1st observation in the sample
X[1]

# get 4th observation in the sample
X[4]

# Get the 2nd and 5th observations in the sample
X[c(2,5)]

# Get a sub-sample of values 2 to 5 from within the sample.
X[2:5]

#------------------------------------------------------------------------------------------
# Matrix operations
#------------------------------------------------------------------------------------------
# Create a 2 by 2 matrix:
# Create a 1 by 4 vector.
A <- c(5, 2, 2, 4)

# Change the dimensions of the list of numbers to be 2 by 2.
dim(A) <- c(2, 2)

# Matrix rows and columns
nrow(A)
ncol(A)

# Matrix inverse
solve(A)

# Matrix transpose
t(A)

# Matrix determinant
det(A)

# Matrix multiplication
A %*% A

c(1, 1) %*% A

# Get diagonal of matrix
diag(A)

# Matrix eigenvalues
eigen(A)$val

# Matrix eigenvectors
eigen(A)$vec

# Cholesky decomposition and generating correlated data
chol(A)

#------------------------------------------------------------------------------------------
# Descriptive statistics
#------------------------------------------------------------------------------------------

# Summarise the list of values in the sample.
summary(X)

# Use the inbuilt function to compute the mean (average)
(X_mean <- mean(X))

# Define out own function to 
# compute the geometric mean of a sample
my_geometricMean <- function(x) {
  prod(x)^(1/length(x))
}

# Use the newly defined function to get the
# geometric mean of our sample, X
(X_geometric_mean = my_geometricMean(X))

# Create an alternative, logarithm-based function for the
# geometric mean. 
my_geometricMean2 <- function(x) {
  exp( (1/length(x)) * sum(log(x)) )
}
(X_geometric_mean2 <- my_geometricMean2(X))

# Make sure the two formulae give the same result for
# the geometric mean
X_geometric_mean == X_geometric_mean2

# Use the inbuilt function to compute the Median of X
(X_median <- median(X))

# Quartiles - computed using the inbuilt qantiles function.

# Get information about this quantiles function.
help("quantile") # Note 9 different types.

# Use the type of quantile that is defined in lecture 1.
help(quantile)
X_quantiles <- quantile(X, type=6)
X_first_quartile <- X_quantiles[2]
X_third_quartile <- X_quantiles[4]

# Check median function
X_quantiles[3] == X_median

# Compute the range of the sample X as the 
# difference betweeen the maximum and minimum value in X
(X_range = max(X) - min(X))

# variance
(X_variance <- var(X))


# standard deviation
(X_stdev <- sd(X))(X_stdev <- sd(X))

