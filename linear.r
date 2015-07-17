
df <- read.csv("train.csv", sep=";")

ggplot(df, aes(read, math, col = gender))+
  geom_point()+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25, face="bold"))

fit <- glm(hon~read + math + gender, df, family = "binomial")
summary(fit)

exp(fit$coefficients)

head(predict(object = fit))

head(round(predict(object = fit, type = "response")*100))

df$prob <- predict(object = fit, type = "response")

# trouble №1

ddf <- glm(am ~ disp+vs+mpg, mtcars, family = "binomial")
print(ddf$coefficients)

# trouble №2
df <- data.frame(ToothGrowth)


library("ggplot2")

ggplot(ToothGrowth, aes(x = supp,y=len ))+
  geom_boxplot(aes(fill = factor(dose)))

obj


library(ROCR)

pred_fit <- prediction(df$prob, df$hon)
pref_fit <- performance(pred_fit, "tpr","fpr")
plot(pref_fit, colorize = T, print.cutoffs.at= seq(0.1, by=0.1))

