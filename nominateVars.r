
df <- read.csv("grants.csv")
str(df)

df$status <- as.factor(df$status)
levels(df$status) <- c("Not Funded", "Funded")

df$status <- factor(df$status, labels = c("Not Funded", "Funded"))


# 1d Table
t1 <- table(df$status)
t1

dim(t1)

# 2d Table
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)
t2

t2 <- table(df$field, df$status)
t2

dim(t2)

prop.table(t2)
prop.table(t2, 1)
prop.table(t2, 2)

# 3d Table

t3 <- table(Years = df$years_in_uni, Fields = df$field, Status = df$status)
t3

dim(t3)

# trouble №1 
HairEyeColor['Red','Blue','Male']
dim(HairEyeColor)
print(prop.table(HairEyeColor['Red','Blue','Male'])/10)
sum(HairEyeColor[,'Green','Female'])

# plots
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

# trouble №2

mydata <- as.data.frame(HairEyeColor)
hc <- subset(mydata, Sex == "Female")

library("ggplot2")
obj <- ggplot(data = hc, aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

obj


# Binominal Test
binom.test(x = 5, n = 20,p = 0.5)
binom.test(t1)

# Chi-Square
t1
chisq.test(t1)
chi <- chisq.test(t1)
chi$exp
chi$obs

t2
chisq.test(t2)

# Fisher's Exact Test

t2
fisher.test(t2)

# trouble №3
hc <- HairEyeColor['Brown',,'Female']
chisq.test(hc)

# trouble №4

library(ggplot2)
t1 <- xtabs(~cut+color, data=diamonds)
t2 <- chisq.test(t1)
print(t2$statistic)

# trouble №5
library(ggplot2)
dd <- diamonds
sub1 <- subset(dd, price >= mean(dd$price))
sub2 <- subset(dd, price < mean(dd$price))
sub1$factor_price <- 1
sub2$factor_price <- 0
dd <- rbind(sub1,sub2)

sub1 <- subset(dd, carat >= mean(dd$carat))
sub2 <- subset(dd, carat < mean(dd$carat))
sub1$factor_carat <- 1
sub2$factor_carat <- 0
dd <- rbind(sub1,sub2)

chidm <- chisq.test(dd$factor_carat,dd$factor_price)
print(chidm$statistic)

# trouble №6
ddd <- table( mtcars$am, mtcars$vs)
fddd <- fisher.test(ddd)
print(fddd$p.value)
