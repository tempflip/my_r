library(car)
library(AER)
library(sandwich)
library(lmtest)
library(ggplot2)
library(gridExtra)

#### 1
# Y = β0 + β1X1 + β2X2 + X3β3 + U

#a H0 : β0 = 0 & β1 = 0 & β2 = 0 & β3 = 0
R = diag(4)
r = cbind(c(0,0,0,0))
R
r

#b H0 : β0 = 0 & β1 = 0
R = rbind(c(1,0,0,0), c(0,1,0,0))
r = cbind(c(0,0))
R
r

#c H0 : β0 = 1 & β1 = 1
R = rbind(c(1,0,0,0), c(0,1,0,0))
r = cbind(c(1,1))
R
r

#d H0 : β1 = 0
R = rbind(c(0,1,0,0))
r = cbind(c(0))
R
r

#e H0 : β1 + β2 = 1
R = rbind(c(0,1,1,0))
r = cbind(c(1))
R
r

### 2

data("CPS1985")
# 
# Consider the data CPS1985 in the package AER and the following
# two models
# 
# Y = exp(Xβ + U) (1)
# 
# Y = Xβ + U (2)
# 
# where the dependent variable is hourly wage and the design matrix X
# includes the unit vector for the intercept, education, married, gender
# and a polynomial of degree three of experience

# a, Make any necessary variable transformations and use lm() and
# estimate both models.

CPS1985$experience_2 <- CPS1985$experience^2
CPS1985$experience_3 <- CPS1985$experience^3
CPS1985$experience_4 <- CPS1985$experience^4
CPS1985$experience_5 <- CPS1985$experience^5

mod1 <- lm(log(wage) ~ education + married + gender + experience + experience_2 + experience_3, data = CPS1985)
summary(mod1)

mod2 <- lm(wage ~ education + married + gender + experience + experience_2 + experience_3, data = CPS1985)
summary(mod2)





# b
# First (exponential) model fits better -- we have more significant parameters:
# experience_2 and experience_3 here are also significant.
#
# 

summary(mod1)
summary(mod2)

p1 <- ggplot() + geom_point(aes(x=fitted(mod1), y=resid(mod1)))
p2 <- ggplot() +  geom_point(aes(x=fitted(mod2), y=resid(mod2)), color="red")
grid.arrange(p1, p2, nrow=2)

# Outliers
ggplot(aes(x=seq(534), y= cooks.distance(mod1))) + geom_point()
p1 <- qplot(cooks.distance(mod1), binwidth = 0.001, xlim=c(0, 0.05))
p2 <- qplot(cooks.distance(mod2), binwidth = 0.001, xlim=c(0, 0.05))
grid.arrange(p1, p2, nrow=2)

# cut the outliers
mod1_cutted <- CPS1985[cooks.distance(mod1) < 0.01,]
mod2_cutted <- CPS1985[cooks.distance(mod2) < 0.01,]

mod12 <- lm(log(wage) ~ education + married + gender + experience + experience_2 + experience_3, data = mod1_cutted)
summary(mod12)

mod22 <- lm(log(wage) ~ education + married + gender + experience + experience_2 + experience_3, data = mod2_cutted)
summary(mod22)


##### Mod1 (mod12) with cutted outliers at cook's distance 0.01 yields a model with 0.39 R2 value.

## c

coeftest(mod12, vcov = vcovHC(mod12))
## d, Yes, removing the outliers improve a lot. See b.

## 3a,
mod_prof <- lm(log(wage) ~ education + married + gender + experience + experience_2 + experience_3 + experience_4 + experience_5, data = CPS1985)

summary(mod_prof)
# Adding the same parameter on higher power does not increades the performance -- even the experience_2 and experience_3
# (which were relevant) got non-relevant. In my option the effect is because the 4th and the 5th degrees makes these parameters
# too dominant, so they outperform the effect of the other parameters
#
#
# However it is important that we also can add experience_3 + experience_4 + experience_5 BUT NOT linear and experience_2, and get this
# parameters linear.
# 
> CPS1985$exp2 <- CPS1985$experience^2
> CPS1985$exp3 <- CPS1985$experience^3
> CPS1985$mar <- as.numeric(CPS1985$married)
> CPS1985$gen <- as.numeric(CPS1985$gender)
> cor(CPS1985[c('experience', 'wage', 'education', 'age', 'mar', 'gen')])

## 3b,

mm <- lm(wage ~ education + experience, data = CPS1985)

R = rbind(c(0,1,0,0,0,0,0,0,0), c(0,0,1,0,0,0,0,0,0))
lht(mod_prof, R)
