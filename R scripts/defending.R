
library(readxl)
Fifa20 <- read_excel("C:/Users/jaman/Desktop/Seminar/archive15-20/Fifa20.xlsx")
data_CB <- filter(Fifa20, player_positions =="CB")

defending <- plot(data_CB$defending,(data_CB$value_eur)/1000000, xlab = "Defenders' defending rating", 
                  ylab = "Players' value [Mio. Euro]",
                  main = "the correlation between the value of the defenders and their rating in defence")

defending



