
########################################
############## Question 1 ##############

# libraries
library(dgof) # for performing K-S test
library(tidyverse)
library(stargazer)

# MY TEST FOR ECDF
set.seed(2020)
x <- rnorm(5)
ecdf(x)
plot(ecdf(x))
#################

# Empirical Cumulative Distribution Function EDCF 
# This computes the ECDF of a numeric input vector

set.seed(123) # set seed for reproducibility 
# set.seed(123) from problem set 1

# rcauchy() is used to compute random cauchy density among a range of inputs
# not very well explained
data <- rcauchy(1000, location = 0, scale = 1) # as given in problem set 1
data <- sort(data)
# applying the ecdf function to calculate the ecdf values of the R data
vector_edcf <- ecdf(data)
vector_edcf

plot(ecdf(data)) # create the ecdf plot 
# the plot is S shaped

# the Kolmogorov-Smirnov test is used in situations where a comparison has to be made 
# between an observed sample distribution and theoretical distribution
ks_data <- max(abs(vector_edcf(data) - pnorm(data)))
print(ks_data) 
# gives 0.1347281 and this is the test statistic 

# or using R ks.test() function
ks.test(data, "pnorm")
# THIS GIVES
## data:  data
## D = 0.13573, p-value = 2.22e-16
## alternative hypothesis: two-sided

# we can see that the test statistic is 0.13573 and the corresponding 
# p-value is 2.2e-16. Since the p-value is less than .05, 
# we reject the null hypothesis. 
# We have sufficient evidence to say that the sample data does not come 
# from a normal distribution.

# I found it difficult to turn the K-S CDF function into R code 
# considering that x values are to do with the largest absolute differences
# between the two distribution functions. 
# CDF = The cumulative distribution function (CDF) of a random variable evaluated 
# at x, is the probability that x will take a value less than or equal to x. 


# x = seq(0, 1, by = 0.01)
# plot(x, vector_edcf(x))

# 2 pi

########################################
############## Question 2 ##############

set.seed(123) # as given in the problem set 1 - question 2

# ECDF <- ecdf(data)
data <- data.frame(x = runif(200, 1, 10)) # as given in the problem set 1 - question 2

data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

head(data) # prints out the 6 data values using the head() function 
# `     x         y
# 1 2.397377  5.099089
# 2 8.612659 22.124880
# 3 2.929424  8.028945
# 4 7.028859 19.131100
# 5 6.559808 14.215458
# 6 1.449998  5.548355

## Estimate the OLS Regression

ols_reg <- lm(data$x ~ data$y, data) # linear model lm()

# or 
ols_reg1 <- lm(y ~ x, data = data)
#summary(data)
summary(ols_reg)
summary(ols_reg1)

# stuck here