library(AER)
library(ggplot2)
data("CPS1985")




############# b -- the model

mod <- lm(wage ~ education, data = CPS1985)
beta0 = mod$coefficients[1]
beta1 = mod$coefficients[2]

ggplot(data=CPS1985, aes(x=education)) + 
  geom_jitter(aes(y=wage), alpha=0.4, height=0.2, width = 0.2, color="red") +
  geom_line(aes(y= (beta0 + beta1*education ) ), color="blue")

############# c -- wage after 10y edu
wage_for_10_years_edu = (beta0 + beta1*10 )
wage_for_10_years_edu


########### d -- 
# null hypthonesis: wage does NOT depend on education ->b1 = 0
# alternative hypothesis -> b1 != 0
# p value: 2.2e-16, which is less than 0.05 -- GOOD!


#####################
#####################

############# a
wageGenderMod <- lm(wage ~ gender, data=CPS1985)
CPS1985$gender_num <- as.numeric(CPS1985$gender)

################ b
# b1 =  -2.116; and because 'female' factor is the second (female=2, male=1), it makes a smaller value (as b1 is negative)

################# c
yes: p-value: 1.703e-06

############### d

wageGenderMod2 <- lm(wage ~ gender_num, data=CPS1985)

wageGenderMod2$coefficients
wageGenderMod$coefficients

## The coefficients are different, but the intercepts different. So you will get a slightly different result (+3 dollar)
## this is becase the different coding

############# e
d_male <- subset(CPS1985, gender=="male")
d_female <- subset(CPS1985, gender=="female")


b0 = wageGenderMod2$coefficients[1]
b1 = wageGenderMod2$coefficients[2]

mean(CPS1985$wage)
mean( b0 + b1 * CPS1985$gender_num)

mean(d_male$wage)
mean( b0 + b1 * d_male$gender_num)
mean(d_female$wage)
mean( b0 + b1 * d_female$gender_num)




