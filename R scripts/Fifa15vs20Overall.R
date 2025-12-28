library(readxl)
Fifa15 <- read_excel("C:/Users/jaman/Desktop/Seminar/archive15-20/Fifa15.xlsx")
Fifa15

library(readxl)
Fifa20 <- read_excel("C:/Users/jaman/Desktop/Seminar/archive15-20/Fifa20.xlsx")
View(Fifa20)

overall_2020 <- hist(Fifa20$overall, xlim = c(85,95), ylim = c(0,350),
                     main = "Histogram of players' value in 2020", 
                     xlab = "Players' overall rating in 2020",
                     col = "blue")
overall_2020

overall_2015 <- hist(Fifa15$overall, xlim = c(85,95),ylim = c(0,350),
                     main = "Histogram of players' value in 2015", 
                     xlab = "Players' overall rating in 2015",
                     col="darkmagenta")

overall_2015
