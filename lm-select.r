rm(swiss)

swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ . , data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Agriculture + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced2)

library(psych)
optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit)
optimal_fit$coefficients

# trouble №1,2

model_full <- lm(rating ~ ., data = attitude) 

model_null <- lm(rating ~ 1, data = attitude)

step(model_full,direction = 'backward')

model_opt <- lm(formula = rating ~ complaints + learning, data = attitude)

anova(model_full,model_opt)

# trouble №3
lm(sr~(.)^2,data=LifeCycleSavings)
