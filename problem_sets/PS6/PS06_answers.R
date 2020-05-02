#####################
# load libraries
# set wd
# clear global .envir
#####################

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
library(nnet)
library(foreign)
library(MASS)
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS6")

#####################
# Problem 1
#####################


# 1) We are interested in predicting the cholesterol category based on sex
#   and fat intake.
# a) Fit an additive model. Provide the summary output, the global null
#   hypothesis, and p-value. Please describe the results and provide a  
#   conclusion.

cholesterol <- read.csv("cholesterol.csv", stringsAsFactors = F, header=T)
lm.chol <- lm(cholCat ~ sex + fat, data=cholesterol)
summary(lm.chol)

# Yi = -0.13 + 0.189(sexXi) + 0.0082(fatXi)

# The global null hypothesis would be that the slope of the regression line
# is equal to zero (meaning that there is no significant linear relationship
# between sex and fat intake and high cholesterol).

# The p-value is <2.2e-16, which is much less than 0.05. This indicates that
# we can reject the null hypothesis and conclude that the slope is not equal
# to zero. There is support for a significant linear relationship between high
# cholesterol and sex and fat intake.

# 2) If explanatory variables are significant in this model, then
#   a) For women, how does increasing their fat intake by 1 gram per day
#     change their odds on being in the high cholesterol group?

#     A 1-gram increase in fat intake would result in a 0.0082 percent increase
#     in the likelihood of a woman having high cholesterol.

#   b) For men, how does increasing their fat intake by 1 gram per day change
#     their odds on being in the high cholesterol group?

#     A 1-gram increase in fat intake would result in a 0.1972 percent increase
#     in the likelihood of a man having high cholesterol.

#   c) What is the estimated probability of a woman with a fat intake of 100
#     grams per day being in the high cholesterol group?

-.13 + 0.189*0 + 0.0082*100
#     A woman with a fat intake of 100 grams per day is estimated to have a 69%
#     likelihood of high cholesterol.

#   d) Would the answers to 2a and 2b potentially change if we included the
#     interaction term in this model? Why?

# Make a new model with an interaction term.
lm.chol.2 <- lm(cholCat ~ sex*fat, data=cholesterol)
summary(lm.chol.2)

# The p-value for the interaction term is 0.2715. This is less than 0.05, indicating
# that it is significant and that there is an interaction relationship between sex
# and fat intake. This suggests that the answers to 2a and 2b could change with a new
# model that includes the interaction term.


#####################
# Problem 2
#####################
gdpChange <- read.csv("gdpChange.csv")

# 1) Construct and interpret an unordered multinomial logit with GDPWdiff as the
# output and "no change" as the reference category, including the estimated cutoff
# points and coefficients.
class(gdpChange$GDPWdiff)

gdpChange$GDPWdiff2 <- relevel(gdpChange$GDPWdiff, ref = "no change")

multi.gdp <- multinom(GDPWdiff2 ~ REG + OIL, data=gdpChange)
summary(multi.gdp)

# The coefficient for REG is higher for "positive", indicating that if a country is
# a democracy, they are more slightly more likely to have a positive change in GDP.
# Meanwhile the coefficients for OIL indicate that if  oil export ratios exceeded 50%, countries
# are slightly more likely to have a negative difference in GDP.

ord.gdp <- polr(GDPWdiff ~ REG + OIL, data=gdpChange, Hess=TRUE)
summary(ord.gdp)

# For democratic countries, the odds of having a negative difference in GDP is 0.41 points lower
# than countries that are non-democratic.
# For countries with an oil export ratio above 50%, the odds of having a negative difference in
# GDP is 0.179 points higher than countries with a ratio below 50%.

