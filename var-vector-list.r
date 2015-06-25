# int array-vector
age  <- c(16, 18 , 22 , 27)

# float array-vector
  age  <- c(16, 18 , 22 , 27)

# bool array-vector
is_marriage  <- c(FALSE, FALSE, TRUE, TRUE)

# string array-vector
name <- c("Olga", "Maria", "Nastya", "Polina")

# list definition
data <- list(age, is_marriage)

# list element access
data[[1]][1]


my_vector <- c(19,18, 29 ,20, 22, 18, 16, 16, 13, 29, 21, 18, 19, 17, 13, 23, 17, 20, 23, 13, 20, 23, 20, 24, 11, 11, 16, 20, 22, 19, 16 ,18 ,26 ,19 ,17 ,23 ,18 ,28 ,25 ,28 ,16 ,19 ,16 ,19 ,26 ,18 ,24 ,20 ,20 ,18)

# mean value of vector's values
mean(my_vector)

# standart value of vector's values
sd(my_vector)

# all values in sd-mean+sd space 
sta <- my_vector[mean(my_vector)-sd(my_vector) < my_vector & mean(my_vector)+sd(my_vector) > my_vector]

# console out
print(sta)
