library(magrittr)
library(readr)
library(tidyr)
library(dplyr)
myName <- "Danya Zhang"

#1
print_order <- function(vec){
  x <- vec[1]
  y <- vec[2]
  z <- vec[3]
  if (x>=y & y>=z){
    return(c(x,y,z))
  } else if(x>=z & z>=y){
    return(c(x,z,y))
  } else if(y>=x & x>=z){
    return(c(y,x,z))
  } else if(y>=z & z>=x){
    return(c(y,z,x))
  } else if(z>=x & x>=y){
    return(c(z,x,y))
  } else{
    return(c(z,y,x))
  }
}

#2
print_string <- function(n){
  list <- list()
  for (i in 1:n) {
    if ((i %% 3 == 0) && (i %% 5 == 0)){
      list <- append(list, "Unknown")
    }
    else if (i %% 3 == 0) {
      list <- append(list, "Yes")
    }
    else if (i %% 5 == 0){
      list <- append(list, "No")
    }
    else {list <- append(list, i)}
  }
  return(list)
}

#3
find_factors <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0]
  factors <- c(factors)
  return(factors)
}

square <- function(s) {
  return(s^2)
}

calc_sum_of_factor <- function(num){
  vec_fac <- find_factors(num)
  return(sum(sapply(vec_fac, square)))
}

#4
find_intersect <- function(a,b,c){
  tst <- c(unique(a),unique(b),unique(c))
  tst <- tst[duplicated(tst)]
  return(tst[duplicated(tst)])
}

#5
factorial_base <- function(f){
  if(f <= 1) {
    return(1)
  } 
  else { 
    return(f*factorial_base(f-1))
  }
}

#6.1
T <- function(n){
 vec <- c(1:n)
 return(sum(vec))
}

#6.2
perfect_sqr <- function(x){
  sqrt(x) %% 1 == 0
}

#6.3
num_tri_sqr <- function(n){
  vec <-  sapply(1:n,T)
  new_vec <- NULL
  for (i in 1:n){
     if (perfect_sqr(vec[i])){
      new_vec <- c(new_vec, vec[i])
    }
  }
  return(new_vec)
}
#sum(num_tri_sqr(1500000)) not included because long run time
q6_sum <- 57101607436

#2022 H-1B Employer Data Hub:
#1
h1b_2022 <- read_csv("/Users/dz/Documents/MSSP/GitHub/MA615-R/h1b.csv")

#3
h1b_2022 <- as.data.frame(h1b_2022)
na_num <- sum(is.na(h1b_2022))

h1b_2022a <- h1b_2022[complete.cases(h1b_2022), ]
h1b_2022a <- h1b_2022a[!(h1b_2022a$Employer==""), ]
h1b_2022a <- h1b_2022a[!(h1b_2022a$City==""), ]
h1b_2022a <- h1b_2022a[!(h1b_2022a$City=="-"), ]

#4
df_num <- h1b_2022a %>% 
  group_by(State) %>% 
  summarise(
      Conti_App = sum(`Continuing Approval`, `Continuing Denial`),
      Approve = sum(`Initial Approval`),
      Denial = sum(`Initial Denial`),
      Init_App = sum(Approve, Denial)) %>%
  as.data.frame()
names(df_num)[2] <- "Conti App"
names(df_num)[5] <- "Init App"
  
df_num <- df_num[,c(1,5,2,3,4)]
  
#5
app_num <- sum(df_num$Approve)
den_num <- sum(df_num$Denial)
 
#6
city_num <- h1b_2022a %>% 
  count(City)
names(city_num)[2] <- "Count"

#7
visa_num <- h1b_2022a %>% 
  count(NAICS, name = "Number")

visa_num$Percentage = round(visa_num$Number/dim(h1b_2022a)[1]*100,3)






  