#Install car packages.
install.packages("car")
library("car")
data(Prestige)
help(Prestige)

#Question 1: We would like to study whether individuals with higher levels of income have
# more pretigious jobs.

#a) Create a new variable "professional" by recoding the variable type so that professionals
#   are coded as 1, and blue and white collar workers are coded as 0.

Prestige$professional <- ifelse(Prestige$type=="prof", 1, 0)
#check recoding:
Prestige$type
Prestige$professional


#b) Run a linear model with prestige as an outcome and income, professional, and the
#   interaction of the two as predictors. (Continuous x dummy interaction)
lm(prestige ~ income + professional + income:professional, data = Prestige)

#c) Write the predition equation based on the result.

#Yi = 30.618 + 0.0014(incomeXi) + 22.757(professionalXi) + -0.0023

#d) Interpret the coefficient for income.

# Controlling for type of profession, a 1 unit increase in income is associated
# with a 0.0014 increase in the prestige of an occupation.

#e) Interpret the coefficient for professional.

# Controlling for income, being a "professional" as a opposed to a white collar or
# blue collar worker is associated with a 22.757 increase in the Pineo-Porter
# prestige score of an occupation.

#f) What is the effect of a $1,000 increase in income on prestige score for
#   professional occupations? In other words, we are interested in the marginal
#   effect of income when the variable professional takes the value of 1.
#   Calculate the change in yhat associated with an $1,000 increase in income
#   based on your answer for c.

30.618 + 0.0014*(1000) + 22.757*(1) + -0.0023
# The prestige score of a professional occupation with a $1,000 income is about 54.7727.
# Calculate the prestige score of the same with an extra $1,000 in income:
30.618 + 0.0014*(2000) + 22.757*(1) + -0.0023
# The prestige score is 56.173.
# Subtract the prestige score of the base income from the boosted income:
56.173 - 54.773
# An increase of $1,000 in income produces a marginal effect of a 1.4 increase in the prestige
# score of a professional occupation.

#g) What is the effect of changing one's occupations from non-professional to professional
#   when her income is $6,000? We are interested in the marginal effect of professional jobs
#   when the variable income takes the value of 6,000. Calculate the change in yhat
#   based on your answer for c.

30.618 + 0.0014*(6000) + 22.757*(1) + -0.0023
# The prestige score of a professional occupation with an income of $6000 is 61.773.
# Now calculate with same income and non-professional occupation:
30.618 + 0.0014*(6000) + 22.757*(0)
# The prestige score of a non-professional occupation with an income of $6000 is 39.018.
# Subtract the non-professional prestige score from the professional prestige score to find
# the marginal effect:
61.773 - 39.018
# The marginal effect of a professional occupation on a $6,000-income job
# is a 22.755 increase in the prestige score.


#Question 2: Researchers are interested in learning the effect of yard signs on voting
# preferences.
#a) Use the results of the regression table to determine whether having these yard signs in
#   a precinct affects vote share. Conduct a hypothesis test with alpha = 0.05.

#H0: B2 = 0, Ha: B2 =/= 0
n <- 30
#Find the test statistic:
(0.042 - 0)/0.016
#The test statistic is 2.625.
#Find the p-value:
2*pt(-abs(2.625), df=n-1)
#The p-value is 0.0137. This is lower than our alpha at 0.05, meaning we can reject our null
# hypothesis that having yard signs in a precinct has no effect on vote share.


#b) Use the results to determine whether being next to precincts with these yard signs
#   affects vote share. Conduct a hypothesis test with alpha = 0.05.

#H0: B3 = 0; Ha: B3 =/= 0
n2 <- 76
#Find the test statistic:
(0.042 - 0)/0.013
#The test statistic is 3.23.
#Find the p-value:
2*pt(-abs(3.23), df=n2-1)
#The p-value is 0.00183. This is lower than our alpha at 0.05, meaning we can reject our
# null hypothesis that being in a precinct adjacent to the yard signs does not have an effect
# on vote share.

#c) Interpret the coefficient for the constant term substantively.

#On average, a precinct that was neither adjacent to a precinct with the yard signs nor assigned
# the yard signs itself was expected to have 0.302, or 30.2%, of its voteshare go to McAuliff's
# opponent.

#d) Evaluate the model fit for this regression. What does this tell us about the importance
#   of yard signs versus other factors that are not modeled?

#The r-squared value is 0.094, meaning about 9.4% of the variation in voteshare can be explained by
# our model. This is not a very large percentage, indicating that there are probably other variables
# that have a much larger effect on vote share, such as income of the precinct, or polarization, for
# example.



