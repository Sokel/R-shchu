?swiss

swiss <- data.frame(swiss)
str(swiss)

hist(swiss$Fertility,col="Red")

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

fit <- lm(Fertility ~ Examination * Catholic, data = swiss)
summary(fit)

confint(fit)

# trouble №1
x1 <- rnorm(50) # создадим случайную выборку из 50 элементов
x2 <- rnorm(50) # создадим случайную выборку из 50 элементов
y <- rnorm(50) # создадим случайную выборку из 50 элементов
na_index <- sample(1:30, runif(1,5,15)) # случайно сгенерируем количество и позиции NA в векторе y
y[na_index] <- NA # создадим NA в векторе y
my_df <- data.frame(x_1 = x1, x_2 = x2, y = y) # создадим dataframe 

dfit <- lm(my_df$y~my_df$x_1+my_df$x_2)
my_df$y_full <- predict(dfit, newdata = my_df)

my_df

# trouble №2
str(mtcars)
df <- mtcars[c(1,3,4,5,6)]
dfit <- lm(wt~.,data=df)
summary(dfit)
step(dfit, direction = 'backward')
model <- lm(wt ~ mpg + disp + hp,data = df)



# trouble №3

ddf <- attitude
sddf <- lm(ddf$rating~ddf$complaints*ddf$critical)
summary(sddf)  
sddf$coefficients
  

hist(swiss$Catholic)
swiss$religious <- ifelse(swiss$Catholic > 60, "Lots", "Few")
swiss$religious <- as.factor(swiss$religious)
  
fit3 <- lm(Fertility ~ Examination + religious, data = swiss)  
summary(fit3)

fit4 <- lm(Fertility ~ Examination * religious, data = swiss)  
summary(fit4)

ggplot(swiss, aes(x = Examination, y = Fertility, col=religious)) + 
  geom_point()+
  geom_smooth(method = 'lm')

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)


# trouble №4,5

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))

dfit <- lm(mpg~wt*am,data = mtcars)
summary(dfit)


# trouble №6
library(ggplot2)
my_plot <- ggplot(mtcars, aes(x=wt,y=mpg,col=factor(am)))+
  geom_smooth(method="lm")


my_plot
