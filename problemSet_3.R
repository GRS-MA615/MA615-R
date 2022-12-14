myName <- "Danya Zhang"

library(datasets)
data("iris")

#1
iris.vers <- iris[iris$Species=="versicolor",]
ans_1 <- iris.vers

#2
sepal.dif <- iris.vers$Sepal.Length - iris.vers$Sepal.Width
ans_2 <- sepal.dif

#3
iris.vers$sepal.dif <- sepal.dif
ans_3 <- iris.vers


data("mtcars")
#4 
ans_4 <- sapply(mtcars, class)


#5
mtcars$am<- as.integer(mtcars$am)  
mtcars$cyl<- as.integer(mtcars$cyl)  
mtcars$vs<- as.integer(mtcars$vs)  
newmtc <- mtcars
ans_5 <- sapply(newmtc, class)

library(dplyr)
#6
ans_6 <- round(newmtc, 1)

#7
ans_7 <- iris %>%
  filter(Species == "virginica", Sepal.Width > 3.5)

#8
ans_8 <- iris %>%
  filter(Species == "virginica", Sepal.Width > 3.5) %>%
  select(Sepal.Length:Petal.Width)

#9
ans_9 <- iris %>%
  with(which(Species == "virginica" & Sepal.Width > 3.5))

library(ggplot2)
data("diamonds")

#10
ten <- diamonds %>%
  count(cut == "Ideal" & carat < 0.21)
ans_10 <- unlist(ten[2,2])

#11
eleven <- diamonds %>%
  count((x+y+z) > 40)
  ans_11 <- unlist(eleven[2,2])

#12
twelve <- diamonds %>%
  count(price > 10000 | depth >= 70)
  ans_12 <- unlist(twelve[2,2])

#13
ans_13 <- diamonds[c(67,982),] %>%
  select(color,y)

#14
ans_14 <- diamonds[c(453,792,10489),]

#15
ans_15 <- diamonds %>% 
  head(10) %>%
  select(x,y,z)

#16
newdiam <- diamonds %>%
  head(1000)
ans_16 <- newdiam


#17
ans_17 <- arrange(newdiam, price)

#18
set.seed(56)
diam750 <- diamonds %>%
  sample_n(750)
ans_18 <- diam750

#19
ans_19 <- summary(diam750)

#20
plot(diam750$depth, diam750$price, xlab="Depth (%)", ylab="Price ($)",
     main ="Price vs. Depth")


diam750 %>%
  ggplot(aes(x = depth, y = price)) +
  labs(title = "Price vs. Depth", x = "Depth (%)", y = "Price ($)") +
  geom_point(size=1, color = "#004753") +
  theme_bw()






