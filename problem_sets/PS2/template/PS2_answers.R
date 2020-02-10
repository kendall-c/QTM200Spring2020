library(readr)

#Question 1
#a) Calculate the x^2 statistic by hand.
#H0: The variables are statistically independent.
#Ha: The variables are statistically dependent.
#Recreate table in R:
bribe.table <- matrix(c(14, 6, 7, 27, 7, 7, 1, 15, 21, 13, 8, 42), ncol=4, byrow = TRUE)
rownames(bribe.table) <- c("Upper class", "Lower class", "Total")
colnames(bribe.table) <- c("Not stopped", "Bribe requested", "Stopped/given warning", "Total")
bribe.table <- as.table(bribe.table)
bribe.table
#The table already has fo (the raw count observed frequency). Find the fe (expected frequency) for each value.
#fe = ((row total)/(grand total))*(column total)
#upper class, not stopped:
(27/42)*21
#13.5
#upper class, bribe requested:
(27/42)*13
#8.36
#upper class, stopped/given warning:
(27/42)*8
#5.14
#lower class, not stopped:
(15/42)*21
#15
#lower class, bribe requested:
(15/42)*13
#4.64
#lower class, stopped/given warning:
(15/42)*8
#2.88
bribe.expected <- matrix(c(13.5, 8.36, 5.14, 7.5, 4.64, 2.88), ncol=3, byrow = TRUE)
rownames(bribe.expected) <- c("Upper class", "Lower class")
colnames(bribe.expected) <- c("Not stopped", "Bribe requested", "Stopped/given warning")
bribe.expected <- as.table(bribe.expected)
bribe.expected

bribes <- matrix(c(14, 6, 7, 7, 7, 1), ncol=3, byrow = TRUE)

chisq.values <- (((bribes - bribe.expected)^2)/bribe.expected)
sum(chisq.values)
#The x^2 test statistic is 3.818.
#Double-check with the chisquare function:
chisq.test(bribes)

#b) Now calculate the p-value. We can use df = (rows -1)(columns - 1)
#df = (2-1)(3-1)
df <- 2
pchisq(3.818, df = 2, lower.tail = FALSE)
#The p-value is 0.148. If alpha is 0.1, we cannot reject the null hypothesis at this time.
#c) Calculate the standardized residuals for each cell.
#standardized residuals = (fo - fe)/(fe(sq(1- row prop.)(1-col prop.)))
#upper class, not stopped:
(14-13.5)/sqrt((13.5*(1-27/42)*(1-21/42)))
#0.322
#upper class, bribe requested:
(6-8.36)/sqrt((8.36*(1-27/42)*(1-13/42)))
#-1.643
#upper class, stopped/given warning:
(7-5.14)/sqrt((5.14*(1-27/42)*(1-8/42)))
#1.526
#lower class, not stopped:
(7-7.5)/sqrt((7.5*(1-15/42)*(1-21/42)))
#-0.322
#lower class, bribe requested:
(7-4.64)/sqrt((4.64*(1-15/42)*(1-13/42)))
#1.644
#lower class, stopped/given warning:
(1-2.88)/sqrt((2.88*(1-15/42)*(1-8/42)))
#-1.536
#The standardized residuals show which cells are contributing the most to our chi-square value (the bigger they are, the more influential).

#Question 2
#a) State a null and alternative two-tailed hypothesis.
#Ho: There is no linear relationship between reservation policy and the number of repaired drinking water facilities in villages.
#Ha: There is a linear relationship between reservation policy and the number of new/repaired drinking water facilities in the villages.

#b) Run a bivariate regression to test this hypothesis.

women <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
View(women)

#Find the mean of reserved and water
mean.reserved <- mean(women$reserved)
mean.water <- mean(women$water)
#Sum reserved and water
sum.reserved <- sum(women$reserved)
sum.water <- sum(women$water)
#Find slope:
sum((women$water - mean(women$water))*(women$reserved - mean(women$reserved)))
sum((women$reserved - mean(women$reserved))^2)
664.1056/71.7764
#The slope is 9.252.
#The alpha is ymean - beta(xmean):
alpha <- mean.water - 9.252*(mean.reserved)
#Alpha is 14.738.
#Check with lm function:
lm(women$water~women$reserved, data=women)
#c) Interpret the coefficient estimate for reservation policy.
# Reserving GP positions for women leaders is associated with an average 9.252 increase in new or repaired drinking water facilities.

#Question 3
#1) Import the data set and obtain summary statistics and examine the distribution of the overall lifespan of the fruitflies.
fruitfly <- read_csv("~/GitHub/QTM200Spring2020/problem_sets/PS2/fruitfly.csv")
View(fruitfly)
summary(fruitfly)
boxplot(fruitfly$lifespan)
#2) Plot lifespan vs. thorax. Does it look like there is a linear relationship? What is the correlation coefficient between these two variables?
lifespan.thorax <- plot(fruitfly$lifespan, fruitfly$thorax)
lifespan.thorax
#It looks like there may be a weak, positive linear relationship between fruitfly lifespan and thorax length.
cor(fruitfly$lifespan, fruitfly$thorax)
#The correlation coefficient is 0.636.
#3) Regress lifespan on thorax. Interpret the slope of the fitted model.
lifespan.regress <- lm(fruitfly$thorax~fruitfly$lifespan)
#Each additional day a fruitfly lives is associated with, on average, a 0.0028 mm increase in the length of its thorax.
#4) Test for a significant linear relationship between lifespan and thorax.Provide and interpret your results of your test.
summary(lifespan.regress)
n <- dim(fruitfly)
alpha <- 0.6597
beta <- 0.0028
se_alpha <- 0.0184
se_beta <- 0.0003
t.stat.a <- alpha/se_alpha
t.stat.b <- beta/se_beta
#The test statistic is 35.853 and 9.333. Now get p-values:
2*pt(-abs(t.stat.a), df=nrow(fruitfly)-ncol(fruitfly))
2*pt(-abs(t.stat.b), df=nrow(fruitfly)-ncol(fruitfly))
#The p-values are 5.785e-66 and 6.6589e-16. These are very small, meaning we can reject the null hypothesis that there is no significant linear relationship between lifespan and thorax length.
#5. Provide the 90% confidence interval for the slope of the fitted model.
confint(lifespan.regress, level=.9)
#The confidence interval is (0.0023, 0.0033). 90% of these confidence intervals will contain the true slope of the regression.
#6. Predict an individual fruitfly's lifespan when thorax = 0.8, and the average lifespan of fruitflies when thorax = 0.8 by the fitted model.
thorax.pred <- fruitfly; thorax.pred$thorax <- 0.8
predict(lm(fruitfly$thorax~fruitfly$lifespan), newdata = thorax.pred, se.fit = T)
predict(lm(fruitfly$thorax~fruitfly$lifespan), newdata = thorax.pred, interval="confidence", level=0.95)
predict(lm(fruitfly$thorax~fruitfly$lifespan), newdata=thorax.pred, interval = "prediction", level=0.95)
#7.For a sequence of thorax values, draw a plot with their fitted values for lifespan, as well as the prediction intervals and confidence intervals.
lifespan.res <- resid(lifespan.regress)
plot(fruitfly$lifespan, lifespan.res, ylab="Residuals", xlab="Lifespan")
abline(a=0.6597, b=0.0028)