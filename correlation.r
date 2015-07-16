
df <- mtcars

cor.test(df$mpg, df$hp)
fit <- cor.test(df$mpg, df$hp)
# OR 
fit <- cor.test(~ mpg + hp, df)
fit$statistic
fit$p.value
str(fit)

plot(df$mpg, df$hp)
library(ggplot2)
ggplot(df, aes(x = mpg, y = hp, col=factor(cyl)))+
  geom_point(size = 5)

df_numeric <- df[,c(1,3:7)] 
pairs(df_numeric)

cor(df_numeric)

library(psych)
fit <- corr.test(df_numeric)
fit$t



# trouble №1
corr.calc <- function(x){
  fit <- cor.test(x[,1],x[,2])
  #ffit <- t.test(x[,1],x[,2])
  return(c(fit$estimate, fit$p.value))
}

vct <- corr.calc(iris[,1:2])
vct

dsf <- cor.test(iris[,1], iris[,2])
dsf$estimate
dd <- iris[,1:2]

str(iris)
str(dd)
dit <- corr(dd[,1],dd[,2])
dit$p.value
dit$statistic

# trouble №2

ddf <- read.csv("step6.csv")
str(ddf)

nddf <- ddf[,sapply(ddf, is.numeric)]
cor(nddf, use = 'complete.obs')

filtered.cor <- function(ddf){
  library(psych)
  nddf <- ddf[,sapply(ddf, is.numeric)]
  corddf <- corr.test(nddf)
  nddf <- corddf$r
  diag(nddf) <- 0
  if(abs(max(nddf)>abs(min(nddf)))){
    return(max(nddf))
  }else if(abs(max(nddf)<abs(min(nddf)))){
    return(min(nddf))
  }
}

filtered.cor(ddf)
filtered.cor(iris)

ir <- iris
ir$Petal.Length <- -ir$Petal.Length
filtered.cor(ir)

?apply

# trouble №3
vec1 <- c(24.985, 8.425 ,14.992 ,18.096 ,16.664 ,2.695 ,10.919 ,12.912 ,0.926 ,-2.941 ,27.019 ,31.122 ,10.999 ,37.391 ,0.069 ,32.565 ,18.737 ,12.030 ,15.988 ,29.278 ,20.641 ,17.138 ,27.051 ,36.600 ,23.380 ,12.726 ,28.429 ,31.066 ,41.038 ,25.110 ,-4.407 ,20.313 ,16.531 ,25.782 ,24.680 ,18.422 ,34.917 ,22.477 ,16.982 ,18.531 ,20.138 ,30.896 ,32.664 ,34.821 ,11.421 ,6.543,39.009, 24.499 ,13.345 ,5.28)
vec2 <- c(4.247, 3.272, 7.384, -3.743, 10.315, 19.066, -9.901, 6.418, 7.287, 2.714, 5.895, 23.421, 12.151, 15.379, 13.808, 4.635, 11.795, 9.409, -0.799, 22.509, 16.575, 6.880, 24.828, 21.983, 13.111, 0.928, 12.409, 4.864, 6.040, 24.878, -5.797, -1.974, 4.576, 8.737, 2.773, 18.012, 16.747, 6.928, 4.748, 18.557, 8.633, 22.755, 5.759, 26.877, 13.310, 5.642, 14.142, 10.015, 15.290, 19.842)

vec1  <- ir$Sepal.Length
vec2 <- ir$Sepal.Width

sh1 <- shapiro.test(vec1)
sh2 <- shapiro.test(vec2)

if((sh1$p.value>=0.05) | (sh2$p.value>=0.05)){
  corate <- cor.test(vec1, vec2)
  print(corate$estimate)
}else{
  corrate <- cor.test(vec1, vec2, method = "spearman")
  print(corrate$estimate)
}

df <- mtcars
df_numeric <- df[,c(1,3:7)]

fit <- lm(mpg ~ hp, df_numeric)
fit

summary(fit)

ggplot(df, aes(hp, mpg))+
  #geom_point(size = 5)+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

new_hp <- data.frame(hp = c(100,150,129,300))
new_hp$mpg <- predict(fit, new_hp)

predict(fit, new_hp)

my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels = c("four", "six", "eight"))

fit <- lm(mpg ~ cyl, my_df)
summary(fit)

ggplot(my_df, aes(cyl, mpg))+
  geom_point()+
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))

aggregate(mpg ~ cyl, my_df, mean)
fit$coefficients

# trouble №4

ddf <- read.table("dataset_11508_12.txt")
dfit <- lm(V1~V2,ddf)
dfit$coefficients

# trouble №5
library(ggplot2)
ddf <- diamonds
dddf <- subset(diamonds, cut == "Ideal" & carat == 0.46 )

dfit <- lm(dddf$price ~ dddf$depth)
print(dfit$coefficients)

# trouble №5
regr.calc <- function(ddf){
  cor_rate <- cor.test(ddf[,1],ddf[,2])
  if(cor_rate$p.value < 0.05){
    lddf <- lm(ddf[,1]~ddf[,2])
    ddf$fit <-  data.frame(fit = lddf$fitted.values)
    return(ddf)
  }else{
    return("There is no sense in prediction")
  }
}

regr.calc(iris[,c(1,4)])

# trouble №6
my_plot <- ggplot(iris, aes(x = Sepal.Width, Petal.Width, col = Species))+
  geom_point()+
  geom_smooth(method="auto")
my_plot

