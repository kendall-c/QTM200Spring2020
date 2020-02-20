#####################################################
## File: Lab12.R                                   ##
## Interactions with Dummary Variables             ##
#####################################################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
# set wd
setwd('~/GitHub/QTM200Spring2020/labs/Lab12')

# load libraries
pkgTest <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg)) 
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

lapply(c("faraway"), pkgTest)


#######################
# Problems
#######################


# Some studies show that politically irrelevant events, such as 
# sports events and shark attacks, affect voters' retrospective
# evaluation of government performance. For example, Busby et al.
# (2017) find that the outcome of a college football game affects
# presidental job approval among students.

load("Busby_Football.RData")
colnames(x)


# 1. Run a linear model with papprove as a dependent variable and 
#    Post, osu, and the interaction of the two as independent variables.
lm(papprove ~ Post + osu + Post:osu, data=x)


# 2. Answer the following questions based on the results.

# 2a. What is the predicted presidential approval of OSU students who received
#     the survey BEFORE the game?

# Yi = 4.562 + -0.378(osuXi)

# 2b. What is the predicted presidential approval of OSU students who received
#     the survey AFTER the game?

# Yi = -0.4469(PostXi) + -0.378(osuXi) + 0.896(PostXi, osuXi)

# 2c. What is the predicted presidential approval of UO students who received
#     the survey BEFORE the game?

# Yi = 4.5619

# 2d. What is the predicted presidential approval of UO students who received
#     the survey AFTER the game?

# Yi = 4.5619 + -0.4469(PostXi)

# 3e. What is the marginal effect of Post on presidential approval
#     when osu=1?

# pre-osu - post-osu