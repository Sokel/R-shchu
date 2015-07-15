
library(ggplot2)

DV ~IV # one-way

DV ~ IV1 + IV2 # Two-way

DV ~ IV1:IV2 # Two-way interaction

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction

DV ~ IV1 * IV2 # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2

DV ~ (IV1 + IV2 + IV3)^2 # main effect and all possible  interaction

DV ~ IV1 + Error(subject/IV1) # repeated measures

mydata <- read.csv("shops.csv")
getwd()
str(mydata)

boxplot(price ~ origin, data=mydata)

ggplot(mydata, aes(x = origin, y = price))+
  geom_boxplot()

fit <- aov(price ~ origin, data= mydata)
summary(fit)

# TWO-way ANOVA

fit1 <- aov(price ~ origin + store, mydata)
summary(fit1)
model.tables(fit, "means")

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data = mydata)
summary(fit4)

# trouble №1
View(npk)
str(npk)

#trouble №2
mfit <- aov(yield ~ N + P + K + N * P, data=npk)
summary(mfit)

ggplot(mydata, aes(x = food, y = price))+
  geom_boxplot()

fit5 <- aov(price ~ food, data = mydata)
summary(fit5)

TukeyHSD(fit5)

# trouble №3
mfit2 <- aov(Sepal.Width~ Species, iris)
summary(mfit2)
TukeyHSD(mfit2)

mydata2 <- read.csv("therapy_data.csv")
str(mydata2)
mydata2$subject <- as.factor(mydata2$subject)

fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy*price + Error(subject/therapy*price), data = mydata2)
summary(fit1b)

ggplot(mydata2, aes(x = price, y = well_being))+
  geom_boxplot()+
  facet_grid(~subject)

fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/therapy*price), data = mydata2)
summary(fit3b)


# trouble №4
dd <- read.csv("Pillulkin.csv")
str(dd)
dd$patient <- as.factor(dd$patient)

a4 <- aov(temperature ~ pill + Error(doctor/patient), data = dd)
summary(a4)

dd <- read.csv("Pillulkin (1).csv")
dd$patient <- as.factor(dd$patient)
a5 <- aov(temperature ~ pill*doctor + Error(patient/(pill+doctor)) , data = dd)
summary(a5)

# trouble №5
library(ggplot2)
library(Hmisc)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj
