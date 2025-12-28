#test
plot(data_ST$overall, data_ST$logvalue)
overallreg <- lm(data_ST$logvalue~data_ST$overall)
summary(overallreg)

#plots von overall mit value und wage
plot(data_ST$overall, data_ST$wage_eur)
plot(data_ST$overall, log(data_ST$wage_eur))

#plots von wage mit value
plot(data_ST$wage_eur, data_ST$value_eur)
plot(log(data_ST$wage_eur), data_ST$logvalue)

#lineare regression mit wage
wagereg <- lm(data_ST$logvalue~ data_ST$logwage)
abline(wagereg, col="red")
summary(wagereg)

#multiple lineare regression mit wage und overall, wieder Zstandardisiert
combined <- lm(data_ST$Zlogvalue~ data_ST$Zoverall+data_ST$Zlogwage)
summary(combined) #ergebnis: Overall ist mehr "wert" als wage
