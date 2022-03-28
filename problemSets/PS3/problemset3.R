#######################
# Problem Set 3 #
#######################


# libraries

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "stargazer",
         "nnet",
         "ggplot2",
         "MASS"), pkgTest)

setwd("/Users/mark/Documents/ASDS-applied-stats-2-2022/problem_set3")
changeData <- read_csv("./gdpChange.csv")

summary(changeData)
names(changeData)
head(changeData, n=10)
tail(changeData, n=5)

##################
### Question 1 ###
##################

# Question 1 - Part 1. Construct and interpret an unordered multinominal logit with GDPWdiff 
# as the output and 'no change' as the reference category, including the estimated cutoff 
# points and coefficients

# keep only the following:
# GDPWdiff which is the Response variable
# REG, and OIL which are the explanatory variables

newchange_data <- changeData[,c("GDPWdiff", "REG", "OIL")]
newchange_data

# categories "positive", "negative", "no change"
# no change == 0
# negative < 0
# positive > 0

newchange_data <- within(newchange_data, {
  GDPWdiff1 <- NA
  GDPWdiff1[GDPWdiff == 0] <- "no change"
  GDPWdiff1[GDPWdiff < 0] <- "negative"
  GDPWdiff1[GDPWdiff > 0] <- "positive"
})

newchange_data

newchange_data$GDPWdiff1 <- factor(newchange_data$GDPWdiff1, 
                                   levels = c("no change", "positive", "negative"))

summary(newchange_data$GDPWdiff1)
# no change  positive  negative 
#   16          2600      1105 

# run the base multinominal logit
multi_logit <- multinom(GDPWdiff1 ~ REG + OIL, data = newchange_data)
summary(multi_logit)

# the exponentiate coefficients
coef_data <- exp(coef(multi_logit))
coef_data

#             (Intercept)   REG         OIL
# positive    93.10789    5.865024    97.15632
# negative    44.94186    3.972047    119.57794


# the estimated cutoff points 
confint_data <- exp(confint(multi_logit))
confint_data 

# this gives 

# positive

#                 2.5 %       97.5 %
#  (Intercept)  5.493416e+01 1.578085e+02
# REG           1.304269e+00 2.637379e+01
# OIL           1.339263e-04 7.048166e+07

# negative

#                 2.5 %       97.5 %
#   (Intercept) 2.643900e+01 7.639360e+01
# REG           8.804391e-01 1.791965e+01
# OIL           1.647467e-04 8.679315e+07

# interpreting the coefficients and cutoff points

# 5.865024 increase suggests a positive growth in the GDP for Democracy (REG: 1=Democracy)?




# Question 1 - Part 2

newchange_data2 < - newchange_data
newchange_data2 <- changeData[,c("GDPWdiff", "REG", "OIL")]
newchange_data2

# categories "positive", "negative", "no change"
# no change == 0
# negative < 0
# positive > 0

summary(newchange_data2)

newchange_data2$GDPWdiff2 <- newchange_data$GDPWdiff


newchange_data2 <- within(newchange_data2, {
  GDPWdiff2 <- NA
  GDPWdiff2[GDPWdiff == 0] <- "no change"
  GDPWdiff2[GDPWdiff < 0] <- "negative"
  GDPWdiff2[GDPWdiff > 0] <- "positive"
})

newchange_data2


# check ordering 

is.ordered(newchange_data2$GDPWdiff2)
# FALSE

# ordering

as.ordered(newchange_data2$GDPWdiff2)

# gives Levels: no change < positive < negative

# create an ordered multinominal logit


multi_logit2 <- multinom(GDPWdiff2 ~ REG + OIL, data = newchange_data2)
summary(multi_logit2)

multi_logit2

# the exponentiate coefficients
coef_data2 <- exp(coef(multi_logit2))
coef_data2

# this gives 

#             (Intercept)       REG          OIL
# no change  0.02234416     0.2587991     0.0003619269
# positive   2.07177984     1.4768404     0.8124904479


# the estimated cutoff points 
confint_data2 <- exp(confint(multi_logit2))
confint_data2

# no change

#               2.5 %         97.5 %
# (Intercept) 1.315877e-02  3.794135e-02
# REG         5.855130e-02  1.143903e+00
# OIL         3.078737e-32  4.254701e+24

# positive

#               2.5 %       97.5 %
# (Intercept) 1.8861400   2.275691
# REG         1.2736401   1.712460
# OIL         0.6475021   1.019519



# interpreting the coefficients and cutoff points

# the ordered multinominal logit suggest an 1.4768404 increase in the GDP for Democracy 



##################
### Question 2 ###
##################

# the data 

mex_data <- read.csv("/Users/mark/Documents/ASDS-applied-stats-2-2022/problem_set3/MexicoMuniData.csv")
str(mex_data)
view(mex_data)
names(mex_data)

as.factor(mex_data$PAN.governor.06)
as.factor(mex_data$competitive.district)
# as.factor(mex_data$cPAN.visits.06)

# (a) run a POISSON REGRESSION 

mex_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
              data = mex_data, family = poisson(link = log))

mex_poisson

# this gives 

# Coefficients:
# (Intercept)  competitive.district        marginality.06       PAN.governor.06  
#    -3.81023         -0.08135              -2.08014              -0.31158 

mex_coeffs <- coefficients(mex_poisson)
mex_coeffs 

# this gives 

# (Intercept) competitive.district       marginality.06      PAN.governor.06 
# -3.81023498          -0.08135181          -2.08014361          -0.31157887 


