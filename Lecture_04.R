#------------------------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Lecture 04 - Classical Linear Regression Model
#
# Topics:
# - Consistent random number generation 
# - Simple linear regression (http://r-statistics.co/Linear-Regression.html) assumptions
# - Getting linear regression predicted values and residuals
#   (http://www.r-tutor.com/elementary-statistics/simple-linear-regression/residual-plot)
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# Exploring the regression assumption E(u) = 0
#------------------------------------------------------------------------------------------

# Make sure we generate the SAME random data each time we run by seeding (starting) the 
# pseudo-random number generator with the same seed each time.
set.seed(1) 

# Simulate data
n = 1000
beta_0 = 5.0
beta_1 = 1.0
u = rnorm(n)
X = rnorm(n)
y = beta_0 + beta_1 * X + u

# Create a data frame to contain our known data and unknown data, u.
ourData <- data.frame(y, X, u)

# Plot X vs y: note y-intercept is not through (0,0)
scatter.smooth(ourData$X, ourData$y, main="scatter plot", xlab="X", ylab="y")
grid()

# Create the regression "model" using the lm function (lm for linear model)
# Defaults to include a constant (intercept) on the right hand side.
ourModel <- lm(y ~ X, data=ourData) 

# Show regression results
summary(ourModel)

# Store the residuals from this correct model
ourData$uhat = resid(ourModel)

# Manually compute the residual standard error
sd(ourData$uhat)

# Verify that the mean of the residuals is zero (aside from floating point errors)
mean(ourData$uhat)

# Store the fitted values of y from this model 
ourData$yhat = fitted(ourModel)

# Plot predicted values from model against X
scatter.smooth(ourData$X, ourData$yhat, main="scatter plot", xlab="X", ylab="yhat")
grid()

#Verify the orthogonality conditions sum(residual_i * predicted value_i) = 0 (aside from floating point errors)
sum(ourData$uhat * ourData$yhat)
sum(ourData$uhat * ourData$yhat)

#------------------------------------------------------------------------------------------
# For the enthusiastic:
#
# Violating the simple linear regression assumption E(u) = 0 by leaving out the
# intercept beta_0 (error term now includes the intercept beta_0).
# Be careful not to omit the intercept without having a strong reason for believing that the
# relationship has to go through the origin (0,0).
#------------------------------------------------------------------------------------------

# Create the regression "model" using the lm function (lm for linear model)
# Defaults to include a constant (intercept) on the right hand side.
# NOTE the change to include 0 intercept term in the model specification.
# That change forces the intercept coefficient to be zero - effectively leaving the intercept
# out of the regression model.
ourModel <- lm(y ~ 0 + X, data=ourData) 

# Show regression results
summary(ourModel)

# Store the residuals from this correct model
ourData$uhat = resid(ourModel)

# Check  the mean of the residuals - not zero
mean(ourData$uhat)

# Store the fitted values of y from this model 
ourData$yhat = fitted(ourModel)

# Plot regression residuals against u
scatter.smooth(ourData$u, ourData$uhat, main="scatter plot", xlab="u", ylab="uhat")
grid()

# Plot fitted values against X - note that the regression line goes through the origin.
scatter.smooth(ourData$yhat, ourData$X, main="scatter plot", xlab="X", ylab="yhat")
grid()

#------------------------------------------------------------------------------------------
# Violating Linearity assumption for the population model
#------------------------------------------------------------------------------------------

# Make sure the regressor is positive so the relationship is monotonic (though not linear).
ourData$X = ourData$X + abs(min(ourData$X)) + 1

ourData$y = beta_0 + beta_1 * (ourData$X)^2 + u

# Plot X vs y to show y-intercept is not through (0,0)
scatter.smooth(ourData$X, ourData$y, main="scatter plot", xlab="X", ylab="y")
grid()

# Create the regression "model" using the lm function (lm for linear model)
# Defaults to include a constant (intercept) on the right hand side.
ourModel <- lm(y ~ X, data=ourData) 

# Show regression results
summary(ourModel)

# Store the residuals from this model with an incorrect functional form
ourData$uhat = resid(ourModel)

# Plot residuals from model against X - what should residuals plots look like?
scatter.smooth(ourData$X, ourData$uhat, main="scatter plot", xlab="X", ylab="uhat")
grid()

#------------------------------------------------------------------------------------------
# Violating Conditional Mean Independence
#------------------------------------------------------------------------------------------

library(MASS) # so we can generate correlated data

set.seed(1) 

# Simulate data
n = 1000
beta_0 = 5.0
beta_1 = 1.0

# Generate data for X ~ N(0,1) and u~N(0,1) 
# Require a positive correlation = 0.9 
# This means Cov(X,u) = 0.9 * sqrt(1) * sqrt(1) = 0.9

# list of means - both equal to 0
means = c(0,0)

# Variances and covariances stored in a 2 by 2 grid or 'matrix'.
# Create the 2 by 2 matrix as a diagonal matrix with ones on
# the diagonal
variances = diag(2)

# Replace the ones with the variances for X and u 
variances[1,1] = 1 # variance of X
variances[2,2] = 1 # variance of u
variances[1,2] = 0.9 # Covariance (and also correlation) between X and u
variances[2,1] = variances[1,2]

bivariateData <- MASS::mvrnorm(n=n, mu = means, Sigma = variances)

X = bivariateData[,1]
u = bivariateData[,2]

# Verify we have the required correlation between X and u
cor(X,u)

# Create the dependent variable
y = beta_0 + beta_1 * X + u

# Create a data frame to contain our known data and unknown data, u.
ourData <- data.frame(y, X, u)

# Plot X vs y to show y-intercept is not through (0,0)
scatter.smooth(ourData$X, ourData$y, main="scatter plot", xlab="X", ylab="y")
grid()

# Create the regression "model"
ourModel <- lm(y ~ X, data=ourData) 

# Store the fitted values of y from this model 
ourData$yhat = fitted(ourModel)

# Store the residuals from this model
ourData$uhat = resid(ourModel)

# Verify that the mean of the residuals is zero (by construction)
mean(ourData$uhat)

# Note that the model has been fitted to ensure zero correlation between X and u
cor(ourData$uhat,X)

# Plot X against actual disturbances, u.
scatter.smooth(ourData$X, ourData$u, main="scatter plot", xlab="X", ylab="u")
grid()

# Plot X against estimated residuals, uhat
scatter.smooth(ourData$X, ourData$uhat, main="scatter plot", xlab="X", ylab="uhat")
grid()

# Show regression results - note inaccurate Beta_1 slope coefficient on X
# This is a bias in Beta_1 caused by the positive correlation between
# X and u. To achieve the zero correlation between X and uhat, the slope
# coefficient estimate has to be larger than its true value.
summary(ourModel)

#------------------------------------------------------------------------------------------
# Relationship between correlation and simple regression slope coefficient 
#------------------------------------------------------------------------------------------

# Make sure we generate the SAME random data each time we run by seeding the 
# random number generator with the same seed each time we start.
set.seed(1) 

# Simulate data
n = 1000
beta_0 = 5.0
beta_1 = 1.0
u = rnorm(n)
X = rnorm(n)
y = beta_0 + beta_1 * X + u

# Create a data frame to contain our known data and unknown data, u.
ourData <- data.frame(y, X, u)

# Create the regression "model" using the lm function (lm for linear model)
# Defaults to include a constant (intercept) on the right hand side.
ourModel <- lm(y ~ X, data=ourData) 

# Print the slope coefficient
coef(summary(ourModel))["X","Estimate"]

# Relationship between simple slope coefficient and correlation
scaleFactor = sd(ourData$y) / sd(ourData$X) # to get y and X to have equal variation
cor(ourData$y,ourData$X) * scaleFactor

# Alternatively standardising the data first before doing regression
ourData$y = ourData$y / sd(ourData$y)
ourData$X = ourData$X / sd(ourData$X)
ourModel <- lm(y ~ X, data=ourData) 

# Compare R_squared in this model to R_squared in the model before scaling X and y.
summary(ourModel)

# Print the slope coefficient
coef(summary(ourModel))["X","Estimate"]

# Relationship between simple slope coefficient and correlation
cor(ourData$y,ourData$X)


#------------------------------------------------------------------------------------------
# Jinshi's experiment - added in response to Ed question
#------------------------------------------------------------------------------------------

library(MASS) # so we can generate correlated data

set.seed(1) 

# Simulate data
n = 50
beta_0 = 1.0
beta_1 = 1.0
beta_2 = 1.0
beta_3 = 1.0

# list of means - both equal to 0
means = c(0,0,0,0)

# Create the variance covariance with a 0.9 correlation between x1 and x2
variances = diag(c(1,1,1,1))
variances[1,2] = 0.99
variances[2,1] = 0.99

bivariateData <- MASS::mvrnorm(n=n, mu = means, Sigma = variances)

simulatedData <- data.frame(bivariateData)
colnames(simulatedData) <- c("x1", "x2", "x3", "u")

simulatedData$y = beta_0 + beta_1*simulatedData$x1 + beta_2*simulatedData$x2 + beta_3*simulatedData$x3 + simulatedData$u

# Define the linear regression model
model_unrestricted <- lm(y ~ x1 + x2 + x3, data=simulatedData)

# Report the regression results
(results_unrestricted <- summary(model_unrestricted))

rsq_unrestricted = results_unrestricted$r.squared

# Estimate the restricted model
model_restricted <- lm(y ~ x3, data=simulatedData)

# Report the regression results
(results_restricted <- summary(model_restricted))

rsq_restricted = results_restricted$r.squared

# Compute the test statistic for the joint hypothesis
(fStar <- ( (rsq_unrestricted - rsq_restricted) / 2 ) / ( (1 - rsq_unrestricted) / (n-4) ))

# Compute the critical value defining the lower bound on the critical (rejection) region (one-sided upper tail test.)
(f_critical_value = qf(0.99,2,n-4))

# Compute the p-value
(pValue <- 1-pf(fStar,2,n-4))

#------------------------------------------------------------------------------------------
# Jinshi's second experiment - what if q > n-k-1? Requires very low number of observations
#------------------------------------------------------------------------------------------

library(MASS) # so we can generate correlated data

set.seed(1) 

# Simulate data
n = 6
beta_0 = 1.0
beta_1 = 1.0
beta_2 = 1.0
beta_3 = 1.0

# list of means - both equal to 0
means = c(0,0,0,0)

# Create the variance covariance with a 0.9 correlation between x1 and x2
variances = diag(c(1,1,1,1))
variances[1,2] = 0.99
variances[2,1] = 0.99

bivariateData <- MASS::mvrnorm(n=n, mu = means, Sigma = variances)

simulatedData <- data.frame(bivariateData)
colnames(simulatedData) <- c("x1", "x2", "x3", "u")

simulatedData$y = beta_0 + beta_1*simulatedData$x1 + beta_2*simulatedData$x2 + beta_3*simulatedData$x3 + simulatedData$u

simulatedData$restrictedY = simulatedData$y - beta_0 - beta_1*simulatedData$x1 - beta_2*simulatedData$x2

# Define the linear regression model
model_unrestricted <- lm(y ~ x1 + x2 + x3, data=simulatedData)

# Report the regression results
(results_unrestricted <- summary(model_unrestricted))

rsq_unrestricted = results_unrestricted$r.squared

# Estimate the restricted model
model_restricted <- lm(restrictedY ~ 0 + x3, data=simulatedData)

# Report the regression results
(results_restricted <- summary(model_restricted))

rsq_restricted = results_restricted$r.squared

# Compute the test statistic for the joint hypothesis
(fStar <- ( (rsq_unrestricted - rsq_restricted) / 3 ) / ( (1 - rsq_unrestricted) / (n-4) ))

# Compute the critical value defining the lower bound on the critical (rejection) region (one-sided upper tail test.)
(f_critical_value = qf(0.99,3,n-4))

# Compute the p-value
(pValue <- 1-pf(fStar,3,n-4))

