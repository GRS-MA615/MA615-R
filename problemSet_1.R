myName <- "Danya Zhang"
---
  
  #1. 
  v1 <- c(1:20)
  v2 <- c(20:1)
  v3 <- seq(1,19,2)
  v4 <- rep(c(3,7,11),10)
  v5 <- c(rep(c(3,7,11),10),3)
  
  
  #2
  x1 <- exp(seq(3,6,by=0.1))*sin(seq(3,6,by=0.1))
  
  #3
  i <- 10:100
  sum1 <- sum(i^3+4*i^2)
  
  #4
  str1 <- paste("label",1:30,sep = ' ')
  
  str2 <- paste("function",1:30, sep = "")
  
  
  #5
  vs <- print('c(1,‘function’,NA, seq(1,5,2), 0.125)')
  
  
  #6
  A <- matrix(1:9, nrow = 3, ncol = 3)
  m1_ans <- A %*% A %*% A
  
  
  #7
  B <- matrix(c(12, -12, 12), nrow=12, ncol=3,byrow=TRUE)
  m2_ans <- t(B) %*% B
  
  
  #8
  y <- c(7,-1,-3,5,17)
  A <- matrix(0,nrow=5,ncol=5)
  Ax <- abs(col(A)-row(A))+1
  m3_ans <- solve(Ax,y)
  
  
  #9
  xv <- seq(0.0,1, by=0.1)
  function1 <- function(xv){
    xv^(1:length(xv))
  }
  func1_ans <- function1(xv)
  
  function2 <- function(xv){
    len <- length(xv)
    (xv^(1:len))/(1:len)
  }
  func2_ans <- function2(xv)
  
  function3 <- function(x, n){
    1 + sum((x^(1:n))/(1:n))
  }
  func3_ans <- function3(2, 3)
  
  
  #10
  cel_to_far <- function(c){
    (c* 9/5) + 32
  }
  
  far_to_cel <- function(f){
    (f - 32)*5/9
  }
  
  
  
  #11
  odd <- function(x){ 
    x[x %% 2 == 1]
  }
  odd_ans <- odd(c(1:2000))
  
  
  #12
  inner_sum <- function(r){
    sum(((1:r)^2)/(10+4*r^3))
  }
  
  outer_sum <- function(n){
    sum(sapply(1:n, inner_sum))
  }
  sum_ans <- outer_sum(10)
  
  
  #13
  modNumber <- function(x,y){
    if (x %% y == 0){
      result <- x
    }
    else{
      up <- x
      while (up %% y != 0) {
        up = up+1
        result <- up
      }
    }
    return(result)
  }
  modNumber(100,3)
  
  
  #14
  numberOfWheels <- function(w){
    switch(w,"unicycle"=1,"bike"=2,"car"=4,"truck"=10,
           "tricycle"=3,"motorcycle"=2)
  }
  numberOfWheels("unicycle")
  numberOfWheels("bike")
  numberOfWheels("car")
  numberOfWheels("truck")
  numberOfWheels("tricycle")
  numberOfWheels("motorcycle")
  
  
  #15
  myFactorial <- function(num){
    if (num==1){return(1)}
    k <- num
    while(k>1){
      return(k*myFactorial(k-1))
      k=k-1
    }
  }
  myFactorial(10)
  
  
  #16
  myCustomFactorial <- function(x,y){
    sum <- 0
    for (i in (x+1):(y-1)){
      sum = sum + i
    }
    return(sum)
  }
  myCustomFactorial(1,10)
  
  
  #17
  data("rivers")
  customRiverMean <- function(rivlen){
    riv <- sum(rivers<rivlen)
    tot <- sum(sort(rivers)[1:riv])
    return(tot/riv)
  }
  customRiverMean(250)
  
  
  #18
  data("ToothGrowth")
  longTeeth <- c()
  for (i in ToothGrowth$len){
    if (i >= 15){
      longTeeth <- append(longTeeth, i)
    }
  }
  longTeeth
  
  
  #19
  data(mtcars)
  avg <- apply(mtcars, MARGIN=2, FUN=mean)
  averageHorsePower <- avg["hp"]
  averageWeight <- avg["wt"]
  
  
  #20
  func20 <- function(xvec, yvec){
    sapply(xvec, FUN=function(x){sum(x>yvec)})
  }
  func20(c(1,2,3),c(1,2))
  
  