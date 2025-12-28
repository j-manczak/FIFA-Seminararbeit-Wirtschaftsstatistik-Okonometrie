a <- Fifa20$mentality_aggression
b <- ((Fifa20$value_eur)/1000000)

aggression <-plot(a, b, type = "h", xlab = "Aggression Rating" , ylab="Players' value [Mio. Euro]", 
                  main = "Correlation between aggression and players' value"
                  , xlim = c(0, 100) ,
                  ylim = c(0, 120))
abline(aggression, col="blue")
summary(aggression)