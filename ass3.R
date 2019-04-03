#1a,
m1 <- cbind(c(1,1,1,1), c(3,4,5,6), c(5,5,3,1))
m1
m2 <- rbind(c(1,3,5), c(1,4,5), c(1,5,3), c(1,6,1))
m2
m3 <- matrix(c(1,1,1,1,3,4,5,6,5,5,3,1), ncol=3)

X = m1

#1b,
XX <- t(X) %*% X
XX

#1c
XX_i <- solve(XX)
XX_i
round(XX %*% XX_i)

# 2

#a, There are 4 samples (4 rows)
#b, K = 2. (3 columns, but 1st is the intercept)

#c, Compute the least squares estimator of β

Y <- matrix(c(1,2,3,5))
B_matrix <- solve(t(X) %*% X) %*% t(X) %*% Y

df <- data.frame(X)
names(df) <- c('intercept1', 'first', 'second')
control_mod <- lm(Y ~ df$first + df$second)

control_mod
B_matrix

###############

summary(control_mod)


#################### T-test
beta_h = B_matrix

y_h <- X%*%beta_h

U_h <- Y - y_h

n <- (length(Y))

K <- ncol(X) - 1

s_2 <- sum(U_h^2)/(n-K-1)

SE <- sqrt(s_2*diag(solve(t(X)%*%X)))

t_0 <- beta_h/SE
t_0

##e, P-value 
2*pt(abs(t_0),df=(n-K-1),lower.tail=F)

This is a huge P-value, bc there are not enough datapoints.

##f
summary(control_mod)


