library(readxl)
Fifa20 <- read_excel("Fifa20_Mio.xlsx")
data_ST <- subset(Fifa20, player_positions == "ST")
data_ST_valuerange <- subset(data_ST, value_mio < 80 & value_mio >5.00)

data_ST_valuerange_passing <- subset(data_ST_valuerange, passing < 101 & passing >50)

value_mio <- data_ST_valuerange_passing$value_mio

log_value_mio <- log(data_ST_valuerange_passing$value_mio)
passing <- data_ST_valuerange_passing$passing


plot(passing, value_mio, xlab = "passing [50 - 100]", ylab = "value [Mio. Euro]")

prüfen <- lm(value_mio ~ passing)

summary(prüfen)

abline(prüfen, col = "red")

