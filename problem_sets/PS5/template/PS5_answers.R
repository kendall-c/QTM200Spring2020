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
library("car")
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS5/template")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)

# a) Check the constant variance assumption for the errors by plotting the residuals
#   versus the fitted values.
model1.resid <- resid(model1)
pdf("ResidFVplot.pdf")
plot(model1, model1.resid, which = 1)
dev.off()

# The line of the graph is not very straight, indicating that there might not be a linear
# relationship among residuals. It also appears that residuals aren't necessarily constantly
# variable along the line-- they seem to have increasing variance as y increases.

# b) Check the normality assumption with a Q-Q plot of the studentized residuals.
pdf("QQplot.pdf")
plot(model1, which = 2)
dev.off()

# The tails deviate from the diagonal line, having "heavier" values in the upper and lower
# ends of the line and creating a steeper-looking slope. This suggests that our data might
# not be distributed normally, and actually have more variation.

# c) Check for large leverage points by plotting the h values.

identify(gamble$sex, gamble$status, gamble$income, gamble$verbal, row.names(gamble))
hat <- lm.influence(model1)$hat
pdf("hvalueplot.pdf")
plot(hat)
abline(h=2*5/47)
abline(h=3*5/47)
identify(1:47, hat, row.names(gamble))
dev.off()

# There appears to be four large leverage points above the 3(k +1)/n threshold, even though
# for some reason they are not labelled with their row name.

# d) Check for outliers by running an outlierTest.
outlierTest(model1, data=gamble)
# The p-value is 4.1041e-07, which is very low. We can conclude that this is an extreme residual.

# e) Check for influential points by creating a "bubble plot" with the hat values and
#   studentized residuals.
pdf("bubbleplot.pdf")
plot(hat, rstudent(model1), type = "n")
cook <- sqrt(cooks.distance(model1))
points(hat, rstudent(model1), cex=10*cook/max(cook))
abline(h=c(-2, 0, 2))
abline(v=c(2,3)*5/47)
identify(hat, rstudent(model1), row.names(gamble))
dev.off()

# There are a few points that seem to be influential in the model, but for some reason they are
# unlabelled.

