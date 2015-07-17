

data(swiss)
str(swiss)

pairs(swiss)

library(ggplot2)
ggplot(swiss, aes(x = Examination, y = Education))+
  geom_point()+
  theme(axis.text = element_text(size = 25),
        axis.title =element_text(size = 25, face = 'bold'))

library(ggplot2)
ggplot(swiss, aes(x = Examination, y = Education))+
  geom_point()+
  theme(axis.text = element_text(size = 25),
        axis.title =element_text(size = 25, face = 'bold'))+
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x=log(Education)))+
  geom_histogram()

ggplot(swiss, aes(x=Examination))+
  geom_histogram()


# trouble №1
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
my_vector <- 1/my_vector
shapiro.test(my_vector)

# trouble №2

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
sdf <- scale(mtcars[,c(1,3)])
mean(sdf)
sdf
ssdf <- lm(sdf[,1]~sdf[,2])
ssdf$coefficients

beta.coef <- function(x){
  sdf <- scale(x)
  ssdf <- lm(sdf[,1]~sdf[,2])
  return(ssdf$coefficients)
}

beta.coef(mtcars[,c(1,3)])

library(psych)
lm.beta(lm(x[,1]~x[,2], x))

# trouble №3
str(mtcars)
t <- shapiro.test(mtcars[,1])
sdd <- append(t$p.value)

for(i in 1:ncol(mtcars[,1:6])){
  sdd <- vector(mode = "numeric")
  t <- shapiro.test(mtcars[,i])
  append(sdd,t$p.value)
}
labels(iris)

normality.test  <- function(x){
  sdd <- vector()
  for(i in 1:ncol(x)){ 
    t <- shapiro.test(x[,i])
    sdd <- c(sdd,t$p.value)
  }
  labx <- labels(x)
  names(sdd) <- as.vector(labx[[2]])
  return(sdd)
}

normality.test(iris[,-5])
normality.test(mtcars[,1:6])



