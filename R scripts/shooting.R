library(readxl)
Fifa20 <- read_excel("Fifa20_Mio.xlsx")
data_ST <- subset(Fifa20, player_positions == "ST")
data_ST_valuerange <- subset(data_ST, value_mio < 80 & value_mio >5.00)

data_ST_valuerange_shooting <- subset(data_ST_valuerange, shooting < 101 & shooting >74)

value_mio <- data_ST_valuerange_shooting$value_mio

log_value_mio <- log(data_ST_valuerange_shooting$value_mio)
shooting <- data_ST_valuerange_shooting$shooting


plot(shooting, value_mio, xlab = "shooting [75 - 100]", ylab = "value [Mio. Euro]")

prüfen <- lm(value_mio ~ shooting)

summary(prüfen)

abline(prüfen, col = "red")

