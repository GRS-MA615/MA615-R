library(readxl)
strawberry_og <- read_excel("/Users/dz/Documents/MSSP/GitHub/MA615-R/strawberries.xlsx")
berry_clean <- strawberry_og[, -c(3,4,5,7,8,9,10,11,12,13,15,16)]
