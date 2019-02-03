install.packages('ISLR')
library(ISLR)
data(Weekly)
summary(Weekly)
attach(Weekly)

I_dir <- Direction == "Up"

plot(Year, Today)

plot(Year, Volume)

plot(Year, Lag1)

plot(Lag1, Lag2)

plot(Volume, Today)

logistic <- glm(I_dir ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = "binomial")
summary(logistic)

beta <- coef(logistic)
nu <- beta[1] + beta[2]*Lag1 + beta[3]*Lag2 + beta[4]*Lag3 + beta[5]*Lag4 + beta[6]*Lag5 + beta[7]*Volume
pred <- exp(nu)/(1+exp(nu)) > 0.5

result <- sum(I_dir == pred)/1089
#-----------------------------------------------------------------------------
#small test
logistic2 <- glm(I_dir ~ Lag2, family = "binomial")
summary(logistic2)

beta2 <- coef(logistic2)
nu2 <- beta2[1] + beta2[2]*Lag2
pred2 <- exp(nu2)/(1+exp(nu2)) > 0.5

result2 <- sum(I_dir == pred2)/1089
#-----------------------------------------------------------------------------

y <- (1990<=Year)&(2008>=Year)
yy <- 2009<=Year
logistic3 <- glm(I_dir[y] ~ Lag2[y], family = "binomial")
summary(logistic3)

beta3 <- coef(logistic3)
nu3 <- beta3[1] + beta3[2]*Lag2[yy]
pred3 <- exp(nu3)/(1+exp(nu3)) > 0.5
result3 <- sum(I_dir[yy] == pred3)/104
