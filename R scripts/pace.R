#Prüfen pace value (log)

library(readxl)
Fifa20 <- read_excel("Fifa20_Mio.xlsx")
data_ST <- subset(Fifa20, player_positions == "ST")
data_ST_valuerange <- subset(data_ST, value_mio < 20 & value_mio >1.00)

data_ST_valuerange_pace <- subset(data_ST_valuerange, pace < 101 & pace >74)


value_mio <- data_ST_valuerange_pace$value_mio

log_value_mio <- log(data_ST_valuerange_pace$value_mio)

pace <- data_ST_valuerange_pace$pace

plot(pace, value_mio, xlab = "pace [75-100]", ylab = "value [Mio. Euro]")

prüfen <- lm(value_mio ~ pace)

summary(prüfen)

abline(prüfen, col = "red")

#wenig Zusammenhang
