
mydata <- read.csv('evals.csv')


a <- -11

# simple if
if(a > 0){
  print('positive')
}else if(a == 0){
  print('null')
}else{
  print('not positive')
}

# smart ifelse
ifelse(a > 0, 'positive','not positive')
#and vector too
a <- c(-1,1)
ifelse(a > 0, 'positive','not positive')


# simple for
for(i in 1:100){
  print(i)
}

for(i in 1:nrow(mydata)){
  print(mydata$score[i])
}

# for+if
for(i in 1:nrow(mydata)){
  if(mydata$gender[i] == 'male'){
    print(mydata$score[i])
  }
}

# for+if VS ifelse

mydata$quality <- rep(NA, nrow(mydata))

for(i in 1:nrow(mydata)){
  if(mydata$score[i] > 4){
    mydata$quality[i] <- 'good'
  }else mydata$quality[i] <- 'bad'
}

mydata$quality <- ifelse(mydata$score >4, 'good', 'bad')



# simple while
i <- 1
while(i < 51){
  print(mydata$score[i])
  i <- i+1
}





# №1

df <- mtcars

df$new_var <- ifelse(df$carb >= 4 | df$cyl > 6, 1,0)
print(df$new_var)

if(mean(my_vector) > 20){
  print('My mean is great')
}else{
  print('My mean is not so great')
}


# №2 

ap <- AirPassengers
ap[144]
str(ap)

ap <- AirPassengers
ap[1]
lag(ap, -1)
ap[1]


now <- 0

ap <- AirPassengers

for(i in 2:144){
  #print(i)
  if(ap[i-1] < ap[i]){
    print(ap[i])
    #print(i)
  }
}


# №3 

mean(ap[1]:ap[10])

ap <- AirPassengers
i <- 1
while(i+10 <= 145){
  print(mean(ap[i:(i+9)]))
  i=i+1;
}

print(mean(ap[i:(i+9)]))
  


