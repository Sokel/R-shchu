
my_calc <- function(x, y){
  s <- x+y
  return(s)
}

result <- my_calc(1,1)

my_calc <- function(x, y){
  s <- x+y
  d <- x-y
  return(c(s,d))
}

result <- my_calc(1,1)

my_calc2 <- function(x, y, z = 10){
  s <- x+y+z
  d <- x-y-z
  return(c(s,d))
}

my_calc2(1,2)

distr1 <- rnorm(100)
hist(distr1)
distr1[1:30] <- NA

distr1[is.na(distr1)] <- mean(distr1, na.rm = TRUE)

my_na_rm <- function(x){
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

rr <- my_na_rm(x=distr1)
rr

my_na_rm <- function(x){
  if(is.numeric(x)){
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  }else{
    print("X is not numeric")
  }
}

my_na_rm <- function(x){
  if(is.numeric(x)){
    stat_test <- shapiro.test(x)
    if(stat_test$p.value > 0.05){
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    }else{
      x[is.na(x)] <- median(x, na.rm = TRUE)
    }
    return(x)
    
  }else{
    print("X is not numeric")
  }
}

#trouble №2
dd <- c(-15.399, -19.577, 10.211, -15.013, 3.411, 5.092, 3.491, 7.952, 22.564, 15.529, 4.768, 23.097, -3.180, 0.827, -8.256, 31.052, -8.126, -9.064, 18.046, -0.116, NA, NA, NA, 20.240, NA, NA, 18.887, NA, NA, 3.192, NA, NA, 0.215, NA, NA, 21.160, NA, NA, -19.588 ,NA)

NA.position <- function(x){
  length(x[is.na(x)])
}



dir()
dir(pattern = "*.csv")


grants <- data.frame()



read_data <- function(){
  df <- data.frame()
  number <<- 0                                 # volotile !!!
  for(i in dir(pattern = "*.csv")){
    temp_df <- read.csv(i)
    df <- rbind(temp_df, df)
    number <- number+1
  }
  print(paste(as.character(number),"files were combined"))
  return(df)
}
