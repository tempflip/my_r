library(car)
pr <- data.frame(Prestige)
data(Prestige)
library(sandwich)

reg <- lm(prestige ~ women + income, pr)

