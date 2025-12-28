#liga testen

#deutschland
plot(data_Bundesliga$overall, log(data_Bundesliga$value_eur))
lmdeutschland <- lm(log(data_Bundesliga$value_eur) ~ data_Bundesliga$overall)
abline(lmdeutschland, col="red")
summary(lmdeutschland)

mean(data_Bundesliga$value_eur)
mean(data_Bundesliga$overall)

#türkei
plot(data_Superlig$overall, log(data_Superlig$value_eur))
lmtürkei <- lm(log(data_Superlig$value_eur) ~ data_Superlig$overall)
summary(lmtürkei)
abline(lmtürkei, col = "red")

mean(data_Superlig$value_eur)
mean(data_Superlig$overall)

#spanien
plot(data_Laliga$overall, log(data_Laliga$value_eur))
lmspanien <- lm(log(data_Laliga$value_eur) ~ data_Laliga$overall)
summary(lmspanien)
abline(lmspanien, col= "red")

mean(data_Laliga$value_eur)


#england
plot(data_Premierleague$overall, log(data_Preierleague$value_eur))
lmengland <- lm(log(data_Premierleague$value_eur) ~ data_Premierleague$overall) 
summary(lmengland)
abline(lmengland, col ="red")

mean(data_Premierleague$value_eur)
mean(data_Premierleague$overall)

#frankreich
plot(data_Ligue1$overall, log(data_Ligue1$value_eur))
lmfrankreich <- lm(log(data_Ligue1$value_eur) ~ data_Ligue1$overall)
summary(lmfrankreich)

mean(data_Ligue1$value_eur)


#italien
lmitalien <- lm(log(data_Seriea$value_eur) ~ data_Seriea$overall)

mean(data_Seriea$value_eur)


#niederlande
lmniederlande <- lm(log(data_Eredevise$value_eur) ~ data_Eredevise$overall)

mean(data_Eredevise$value_eur)


#portugal
lmportugal <- lm(log(data_Liganos$value_eur) ~ data_Liganos$overall)

mean(data_Liganos$value_eur)




plot(data_Bundesliga$overall, log(data_Bundesliga$value_eur), col = "blue", cex=0.5, xlab = "Players' skills (Overall)" , ylab = "Logarithmic value of football players" )
points(data_Superlig$overall, log(data_Superlig$value_eur), col = "red", cex=0.5)
points(data_Laliga$overall, log(data_Laliga$value_eur), col = "violet", cex=0.5)
points(data_Premierleague$overall, log(data_Premierleague$value_eur), col = "orange", cex=0.5)
points(data_Seriea$overall, log(data_Seriea$value_eur), col = "yellow", cex=0.5)
points(data_Ligue1$overall, log(data_Ligue1$value_eur), col = "pink", cex=0.5)
points(data_Eredevise$overall, log(data_Eredevise$value_eur), col = "green", cex=0.5)
points(data_Liganos$overall, log(data_Liganos$value_eur), col = "brown", cex=0.5)

abline(lmdeutschland, col="blue")
abline(lmtürkei, col = "red")
abline(lmspanien, col = "violet")
abline(lmengland, col= "orange")
abline(lmitalien, col = "yellow")
abline(lmfrankreich, col ="pink")
abline(lmniederlande, col="green")
abline(lmportugal, col = "brown")

plot(data_Premierleague$overall, data_Premierleague$value_eur, col = "blue", cex=0.5, xlab = "Players' skills (Overall)" , ylab = "Value of football players" )
points(data_Superlig$overall, data_Superlig$value_eur, col = "red", cex=0.5)