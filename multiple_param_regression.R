pr <- Prestige
X1 <- pr$income
X2 <- pr$women

length(X2)
X <- cbind(rep(1,length(X1)), X1, X2)
Y <- cbind(pr$prestige)
  
solve(t(X) %*% X) %*% t(X) %*% Y

m <- lm(prestige ~ income + women, data=pr)
summary(m)
