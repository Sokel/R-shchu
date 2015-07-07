
?mtcars

df <- mtcars

str(df)


# creating factor about num
df$vs <- factor(df$vs, labels=c("V", "S"))

df$am <- factor(df$am, labels=c("Auto", "Manual"))

# simple static functions
median(df$mpg)

mean(df$disp)

sd(df$hp)

range(df$cyl)

mean_disp <-mean(df$disp)
print(mean_disp)

# example 1
mean(df$mpg[df$cyl == 6])

# example 2
mean(df$mpg[df$cyl == 6 & df$vs == "V"])

# example 3
sd(df$hp[df$cyl != 3 & df$am =="Auto"])

# trouble №1
mean(df$qsec[df$cyl != 3 & df$mpg >= 20])

?aggregate

# create realesed dataframe by FUN func
mean_hp_vs <- aggregate(x = df$hp, by= list(df$vs), FUN= mean)
# named 
colnames(mean_hp_vs) <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs + am, df, mean)

aggregate(x=df$hp, b=list(df$vs,df$am), FUN = mean)

aggregate(x=df[,-c(8,9)], by=list(df$am), FUN=median)

# trouble №2
aggregate(hp ~ disp + am, df, sd)
aggregate(x=df[c(3,4)],by=list(df$am), FUN=sd)


library(psych)

?describe

describe(x=df)

descr <- describe(x=df[,-c(8,9)])

?describeBy()

descr2 <- describeBy(x = df[,-c(8,9)], group = df$vs)

descr2 <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = TRUE, digits = 1)

descr3 <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = TRUE, digits = 1, fast = TRUE)

describeBy(df$qsec, group = list(df$vs,df$am), mat = TRUE, digits = 1, fast = TRUE)



#trouble №3
df <- airquality

dfd <- subset(df, Month == 7 | Month == 8 | Month == 9)
aggregate(Ozone ~ Month,dfd, FUN=length)

#trouble №4
df <- airquality
dfd <- describeBy(x= df, group = df$Month, digits = 1)
print(dfd)

sum(is.na(df))
