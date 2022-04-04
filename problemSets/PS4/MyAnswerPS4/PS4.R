#########################################################
################### Problem Set 4 #######################
#########################################################

## QUESTION 1
## We're interested in modelling the historical causes of infant mortality. We have data from
## 5641 first-born in seven Swedish parishes 1820-1895. Using the "infants" datset in the eha
## library, fit a Cox Proportional Haxard Model using mother's age and infant's gender as 
## covariates. Present and interpret the output. 

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)


# set wd for current folder
setwd(dirname("/Users/mark/Documents/ASDS-applied-stats-2-2022/problemset4/PS4.R"))

# Load infants dataset
data(infants)

# using Tutorial 10 as reference, where 'child' was used
infant_survivor <- with(infants, Surv(enter, exit, event))

# using survit to create the survival model

## and need to run a Cox Proportional Hazard regression on the data

km <- survfit(infant_survivor ~ 1, data = infants)


# summary(km, times = seq(0, 15, 1))
# event = 0, exit = 365, exit = 5 

summary(km, times = seq(0, 365, 5))
autoplot(km, main="Overall Infant Survival Rate",
         xlab = "Time",
         ylab = "Survival Rate")



# Run a Cox Proportional Hazard Regression
# referencing tutorial 10
cox <- coxph(Surv(enter, exit, event) ~ sex + age, data = infants)
summary(cox)
drop1(cox, test = "Chisq")
# stargazer(cox, type = "text")
stargazer(cox)

## For the Survival Rate in the first 100 days there is an increase, which then decrease after 
## 100 days but the overall infant survival rate is shown to be quite low. 

