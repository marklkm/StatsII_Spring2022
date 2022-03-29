
#########################################################
################### Problem Set 2 #######################
#########################################################

library(stargazer)

load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))

str(climateSupport)
names(climateSupport)


# Question 1
# 
# fit an additive model. provide the summary output,

levels(climateSupport$sanctions)

climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, ordered = F), ref = "5%")

climateSupport$sanctions

# fitting the additive model

c_logit <- glm(choice ~ countries + sanctions, family = "binomial", data = climateSupport)
summary(c_logit)


# Coefficients:
#                   Estimate Std. Error  z value  Pr(>|z|)    
# (Intercept)       0.24743   0.04406   5.616    1.95e-08 ***
#  countries.L      0.45845   0.03810   12.033   < 2e-16 ***
#  countries.Q     -0.00995   0.03806  -0.261    0.79374    
# sanctionsNone    -0.19185   0.06216  -3.086    0.00203 ** 
#  sanctions15%    -0.32510   0.06224  -5.224    1.76e-07 ***
#  sanctions20%    -0.49542   0.06228  -7.955    1.79e-15 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 11783  on 8499  degrees of freedom
# Residual deviance: 11568  on 8494  degrees of freedom

# 11783 = null deviance and 11568 = residual deviance
# find the p-value for the chi-square test statistic
pchisq(11783-11568, 5, lower.tail = F) 

# this gives
# 1.749304e-44 



# logit model
# period functions as omnibus selector (the kitchen sink additive model)
# ~ . will select countries (ord) and sanctions (fct)

climate_logit <- glm(choice ~ ., family = binomial(link="logit"), data = climateSupport)
summary(climate_logit)

reg_exp <- exp(coef(climate_logit))
stargazer(reg_exp, type = "text")


# not supported or supported
#  the dependent variable is binary(0/1, True/False, Yes/No) in nature
t_glm <- glm(choice ~ 1, data = climateSupport, family=binomial(link = "logit"))
summary(t_glm)

