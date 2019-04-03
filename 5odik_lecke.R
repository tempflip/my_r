library(car)
pr <- data.frame(Prestige)
data(Prestige)
library(sandwich)
library(lmtest)

reg <- lm(prestige ~ women + income, pr)

V_b <- vcovHC(reg)

coeftest(reg, vcov= V_b)



#################### T-test
Y <- Prestige$prestige
X1 <- Prestige$income
X2 <- Prestige$women
X <- cbind()


beta_h <- solve(t(X) %*% X) %*% t(X) %*% Y

y_h <- X%*%beta_h

U_h <- Y - y_h

n <- (length(Y))

K <- ncol(X) - 1

s_2 <- sum(U_h^2)/(n-K-1)

SE <- sqrt(s_2*diag(solve(t(X)%*%X)))

t_0 <- beta_h/SE
t_0
###############################

R <- rbind(c(0,1,0), c(0,0,1))
R

Q <- nrow(R)
Q


