# Homework 4, Problem 1
library(dplyr)
## Read the data
houseWork <-read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/houseWork.csv')
str(houseWork)

## a)
summary(houseWork$sex)

## b)
muf <- mean(houseWork$hours[houseWork$sex == 'f'])
mum <- mean(houseWork$hours[houseWork$sex == 'm'])

## c)
houseWork$female <- ifelse(houseWork$sex == 'f', 'TRUE', 'FALSE')
houseWork$male <- ifelse(houseWork$sex == 'm', 'TRUE', 'FALSE')

## d)
fit <- lm(hours ~ female, data = houseWork)
summary(fit)

## e)
## The intercept is positive and it shows that if no women are involved in housework
## the work that will be done by all men is the average value of work hours by men per week
## The other coefficient (beta1) is negative which, so we can conclude that
## women overall work less on average so the addition of another woman would decrease the
## overall average working hours per week.



## f)

## We can rewrite the null hypothesis as: H0 beta1 >= 0
## in this case, the alternative hypothesis would be: H1 beta1 < 0
## This means that the null hypothesis states that women's work hours contribute to the average population work hours
## and the alternative hypothesis states that it is the opposite
## Given the results from the linear model, we can reject the null hypothesis, thus stating that
## women do not contribute and in fact decrease the overall average work hours

## g)
populationMean <- mean(houseWork$hours)
testStatistic <- sqrt(11016)*(muf - populationMean) / 0.3186
pt(testStatistic, df = 11014)

## h)
##There is almost zero probability for us to make a mistake when rejecting the 
##null hypothesis if it is true. Thus we can reject the null hypothesis.

## i)
## The test assumes that the population follows a t-distribution, however our expectations are that the
## distribution is normal or normal exponential, because we are estimating time values

## j)
fit1 <- lm(hours ~ female + male, data = houseWork)
summary(fit1)
##Our model is:
## Y = beta0 + beta1*X1 + beta2*X2. But we assume that x2 exists, i.e. that it is TRUE (equals 1) 
##then by default X2 is FALSE (it is zero) and we cannot state its influence on the average
##work hours.

