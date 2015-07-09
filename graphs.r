df <- mtcars

df$vs <-  factor(df$vs, labels = c("V","S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))


hist(df$mpg)
hist(df$mpg, breaks = 20, xlab = "MPG")

boxplot(mpg ~ am, df, ylab = " MPG")

plot(df$mpg, df$hp)
plot(df$mpg, df$am)

library(ggplot2)

ggplot(df, aes(x = mpg))+
  geom_histogram(fill = "white", col = "black", binwidth = 2)

ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot(binwidth = 3)

ggplot(df, aes(x = mpg, fill=am))+
  geom_density(alpha(0.1))

ggplot(df, aes(x = am,y=hp, col= vs))+geom_boxplot()
  
my_plot <- ggplot(df, aes(x=mpg,y=hp,col=vs, size = qsec))+geom_point()

my_plot2 <- ggplot(df, aes(x = am,y=hp, col= vs))
my_plot2 + geom_point()


# trouble №1
boxplot(airquality$Ozone ~ airquality$Month, airquality, breaks = 5)

# trouble №2
plot1 <- ggplot(mtcars, aes(x=mpg,y=disp,col=hp))+geom_point()

# last trouble today
ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point(aes(size = Petal.Length))


#save any result
df <- mtcars

getwd()
setwd(getwd())


