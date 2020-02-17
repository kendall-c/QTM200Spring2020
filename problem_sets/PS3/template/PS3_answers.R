#Import incumbents dataset:
library(readr)
incumbents_subset <- read_csv("~/GitHub/QTM200Spring2020/problem_sets/PS3/incumbents_subset.csv")
View(incumbents_subset)

#Question 1: Difflog and Voteshare
#a) Run a regression where the outcome variable is voteshare and the explanatory variable is difflog.
summary(lm(incumbents_subset$voteshare ~ incumbents_subset$difflog))

#b) Make a scatterplot of the two variables and add the regression line.
voteshare_reg <- lm(incumbents_subset$voteshare ~ incumbents_subset$difflog)
pdf("Q1plot.pdf")
plot(incumbents_subset$difflog, incumbents_subset$voteshare, xlab = "Difference in Campaign Spending", ylab = "Incumbent Vote Share")
abline(voteshare_reg)
dev.off()

#c) Save the residuals of the model in a separate object.
q1.resid <- resid(voteshare_reg)

#d) Write the prediction equation.
# y = 0.579 + 0.416x

#Question 2: Difflog and Presvote
#a) Run a regression where the outcome variable is presvote and the explanatory variable is difflog.
summary(lm(incumbents_subset$presvote ~ incumbents_subset$difflog))

#b) Make a scatterplot of the two variables and add the regression line.
presvote_reg <- lm(incumbents_subset$presvote ~ incumbents_subset$difflog)
pdf("Q2plot.pdf")
plot(incumbents_subset$difflog, incumbents_subset$presvote, xlab = "Difference in Campaign Spending", ylab = "Incumbent Vote Share")
abline(presvote_reg)
dev.off()

#c) Save the residuals of the model in a separate object.
q2.resid <- resid(presvote_reg)

#d) Write the prediction equation.
# y = 0.508 + 0.238x

#Question 3: Preshare and Voteshare
#a) Run a regression where the outcome variable is voteshare and the explanatory variable is presvote.
summary(lm(incumbents_subset$voteshare ~ incumbents_subset$presvote))

#b) Make a scatterplot of the two variables and add the regression line.
presshare_reg <- lm(incumbents_subset$voteshare ~ incumbents_subset$presvote)
pdf("Q3plot.pdf")
plot(incumbents_subset$presvote, incumbents_subset$voteshare, xlab = "Party's Presidential Vote Share", ylab = "Incumbent Vote Share")
abline(presshare_reg)
dev.off()

#c) Write the prediction equation.
# y = 0.441 + 0.388x

#Question 4: Residuals
#a) Run a regression where the outcome variable is the residuals from Question 1 and the explanatory variable is the residuals from Question 2.
resid_reg <- lm(q1.resid ~ q2.resid)
summary(resid_reg)

#b) Make a scatterplot of the two residuals and add the regression line.
pdf("Q4plot.pdf")
plot(q2.resid, q1.resid, xlab = "Party Presidential Vote Share Residuals", ylab = "Incumbent Vote Share Residuals")
abline(resid_reg)
dev.off()

#c) Write the prediction equation.
# y = -4.860e-18 + 2.569e-01x

#Question 5: Multivariate Regression
#a) Run a regression where the outcome variable is the incumbent's voteshare and the explanatory variables are difflog and presvote.
multi_reg <- lm(incumbents_subset$voteshare ~ incumbents_subset$difflog + incumbents_subset$presvote, data=incumbents_subset)
summary(multi_reg)

#b) Write the prediction equation.
# y = 0.449 + 0.355(x1) + 0.257(x2)

#c) What is it in this output that is identical to Question 4? Why is this the case?
#The residual standard error for both regressions are the exact same, at 0.0734.
#This is because the degrees of freedom in both models are the same (because there is the same
#number of observations), and because the mean of both model residuals is about zero.

