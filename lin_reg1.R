library(car)
pr <- Prestige
library(ggplot2)


# beta_1(pr)

X = pr$income
Y = pr$prestige

beta1 <- sum( (Y - mean(Y)) * (X - mean(X)) ) / sum((X - mean(X))**2)
beta0 <- mean(Y) - beta1*mean(X)

pred <- (beta0 +  seq(0, 20000, 100) + beta1)

ggplot(data = pr, aes(x=income)) +
  geom_point(color="red", aes(y=prestige)) +
  geom_point(color="blue", aes(y=(beta0 +  income * beta1)))

resi <- (beta0 + beta1 * X) - Y

ggplot(data=data.frame(resi), aes(x=resi)) + geom_histogram(binwidth = 2)


reg1 <- lm(prestige ~ income, data = pr, y = T)
summary(reg1)
