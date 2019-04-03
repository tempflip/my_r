library(car)
data(Mroz)
attach(Mroz)
library(ggplot2)

df<- Mroz
df$lfp <- ifelse(lfp=='yes', 1, 0)
df$wc <- ifelse(wc=='yes', 1, 0)
df$hc <- ifelse(hc=='yes', 1, 0)

reg <- glm(lfp ~ k5 + k618 + wc, data=df, family=binomial("logit"))
summary(reg)

## first prediction
pr1 <- predict(reg, df, type="response")
summary(pr1)


n <- nrow(df)

ggplot(data=df, aes(y = lfp, x=seq(1, n))) + geom_point() + geom_point(aes(y=pr1), color="blue")

ggplot(data=data.frame(pr1), aes(x=pr1))+geom_histogram(binwidth = 0.005) 
�

