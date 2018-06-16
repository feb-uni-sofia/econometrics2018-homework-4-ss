## Homework 4, Problem 2
crime <- read.delim('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/crime.csv', stringsAsFactors = FALSE)
str(crime)

## a)

fit <- lm(C ~ HS, data = crime)
summary(fit)

## The estimated coefficient for HS equals 1.48. This means that
## we expect counties that differ by one percentage point
## in high school graduates to have a difference of 1.48 crimes per 1000 persons.
## and counties with more high school graduates to have higher crime rates 
## (because the coefficient is positive)

min(crime$HS)
## The intercept corresponds to the expected crime rate in a county
## with no high school graduates and is not a meaningful estimate
## because it is very far away from any observed county (the smallest
## observed share of high-school graduates is 54.5%!)

## b)
## The points for this question will be substracted from
## the required score count
pairs(~ C + U  + I + HS, data = crime)

## c)

fit1 <- lm(C ~ HS + U, data = crime)
summary(fit1)

## We see that the coefficient for HS is negative and not significantly 
## different from 0 (see the t-test p-value).

## This happens because the more HS and U are positively correlated
## (see the scatterplot) and
## because the level of urbanisation is positively correlated with 
## the crime rate.

## Accounting for the level of urbanisation there is no significant
## (linear) association between crime rates and the share of high 
## school graduates.

## d)

## The model from a) reveals a positive (linear) association between
## the share of high school graduates and crime rates. However, it does not
## imply causation. The model from c) shows that the entire effect of HS
## on crime rates is accounted for by the level of urbanisation.
## Therefore the model from a) does not in any case support the polititian's 
## proposition.

## e)
## The points for this question will be substracted from
## the required score count

fit2 <- lm(C ~ HS + U + I, data = crime)
fit3 <- lm(C ~ U, data = crime)

anova(fit2, fit3)

## The null hypothesis that both coefficients
## are simultaneously equal to 0 cannot be rejected here
## because the p-value = 0.43 > 0.05.

