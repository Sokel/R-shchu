
# help manual of "Data input" functions
?read.table
?read.csv

# read csv to frame
my_data <- read.csv('evals.csv')

# open data table in window
View(mydata)

# structure of frame
str(mydata)

# return vector of value's names
names(mydata)

# create new var in frame
mydata$nvar <- 0

# amount of rows in frame
nrow(mydata)

# return amount of vars(cols) in frame
ncol(mydata)

# identity rows in frame by num
mydata$id <- 1:nrow(mydata)

# summaty data of all vars in frame
summary(mydata)

# use one of them var in frame
mydata$score *2

# create logical vector
mydata$gender == 'female'

# select ops by logical vector
mydata[mydata$gender == 'female',1]

# create set about query
subset(mydata, gender == 'female')
subset(mydata, score > 3.5)

# separate frame to set
mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
# joins sets to frame
mydata4 <- rbind(mydata2, mydata3)

# separete frame by cols
mydata5 <- mydata[,1:10]
mydata6 <- mydata[,11:23]
# join frame by cols
mydata7 <- cbind(mydata5, mydata6)

# show avaible system datasets
library(help="datasets")

df <- mtcars

df$even_gear <- 1- df$gear %% 2
print(df$even_gear)


print(df[df$cyl == 4,1])


df[c(3,7,10,12,nrow(df)),1:ncol(df)]
