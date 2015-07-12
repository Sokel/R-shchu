
df <- iris

df1 <- subset(iris, Species != "setosa")
table(df1$Species)

hist(df1$Sepal.Length)

library(ggplot2)
ggplot(df1, aes(x = Sepal.Length))+
  geom_histogram(fill = 'white', col = 'black', binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(x= Sepal.Length, fill = Species))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(x = Species, Sepal.Length))+geom_boxplot()

shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

bartlett.test(Sepal.Length ~ Species, df1)

t.test(Sepal.Length ~ Species, df1)

test1 <- t.test(Sepal.Length ~ Species, df1)
str(test1)
test1$p.value

t.test(Sepal.Length ~ Species, df1, var.equal = TRUE)

t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

# trouble №1
str(ToothGrowth)
View(ToothGrowth)
sub1 <- subset(ToothGrowth, supp == "OJ" & dose == 0.5)
sub2 <-  subset(ToothGrowth, supp == "VC" & dose == 2)

tsub <- t.test(sub2$len, sub1$len)
print(tsub$statistic)

# trouble №2 

mydata <- read.csv("lekarstva.csv")
t.test(mydata$Pressure_before, mydata$Pressure_after, paired = TRUE)



t.test(Sepal.Length ~ Species, df1, var.equal = TRUE)

install.packages("Hmisc")

ggplot(df1, aes(x = Species, y = Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom ='errorbar', width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(x = Species, y = Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 2)

?wilcox.test()

test2 <- wilcox.test(Petal.Length~Species, df1)
pv <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()

wilcox.test(Petal.Length~Species, df1, mu = 6.267)
pairWtest <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = TRUE)

#trouble №3

dd <- read.table("dataset_11504_15.txt")

bartlett.test(dd$V1~dd$V2)

tdd <- t.test(dd$V1~dd$V2, var.equal = FALSE)
tdd$p.value

wdd <- wilcox.test(dd$V1~dd$V2)
wdd$p.value 

#trouble №4

dd1 <- read.table("dataset_11504_16.txt")
str(dd1)
t.test(dd1$V1, dd1$V2, var.equal = FALSE)

