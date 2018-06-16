## Homework 4, Problem 1

library(dplyr)

## Read the data
houseWork <- read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/houseWork.csv')
str(houseWork)

## a)

table(houseWork$sex)
## or 
summary(houseWork$sex)

## b)

summarise(group_by(houseWork, sex),
          averageHours = mean(hours),
          )

## c)

houseWork <- within(houseWork, {
  female <- sex == 'f'
  male <- sex == 'm'
})

## d)

fit <- lm(hours ~ female, data = houseWork)

## e)

summary(fit)

## On average, men worked (in the house) 32.8 hours per week
## Women worked (in the house) on average 14.45 hours less than men

## f)

## H0: mu_f <= mu_m <=> mu_f- mu_m <= 0
## beta1 = mu_f - mu_m (due to the coding of the variable female which is TRUE (1) for women and FALSE (0) for men)
## it follows that H0: beta1 <= 0

## g)
## Test-statistic is (from the regression output )
## t = -45.38
## the p-value of the test is
n <- nrow(houseWork)
1 - pt(-45.38, df = n - 2)
## because the large values of the test-statistic go against the 
## null hypothesis

## h)
## We do not reject the null hypothesis, because the p-value calculated
## above is larger than 0.05.

## i)
## The t-test assumes that either that the hours spent in housework are
## normally distributed in each group (men, women) and that the 
## variance of hours is the same in the two groups (homoskedasticity)

## The normality assumption is less problematic here because we have large sample sizes (see a)) we can rely on the central limit

summarise(group_by(houseWork, sex),
          sdHours = var(hours)
          )

## The equality of variance assumption however appears to be problematic
## because the estimated variance of hours for men is roughly two times
## larger than the estimated variance for women.

## j)
## The points for this question will be substracted from
## the required score count

lm(hours ~ female + male, data = houseWork)

## The coefficient for male cannot be estimated because
## of multicollinearity: the dummy variables female and male add up to 1
## for each observation and their sum is thus equal to the variable 
## corresponding to the intercept.
