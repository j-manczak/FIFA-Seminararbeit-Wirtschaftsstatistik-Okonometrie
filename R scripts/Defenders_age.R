#bevor es losgeht "scrips for R" as working directory

# Nur Stürmer und nur Wert zwischen a und b

library(readxl)
Fifa20 <- read_excel("Fifa20.xlsx")
data_CB <- subset(Fifa20, player_positions == "CB")
data_CB_valuerange <- subset(data_CB, value_eur < 20000000 & value_eur >1000000)

# variablen festlegen
y <- (data_CB_valuerange$value_eur)/1000000

x <- data_CB_valuerange$age


#Altersvariable quadrieren

xsq <- x^2

xcub = x^3

plot(x,y,  xlab = "Defenders' age", ylab = "Defenders' value [Mio. Euro]")

#model fit

fit1 <- lm(y~x)

anova(fit1)

abline(fit1, col = "blue")

#model2 mit Quadrat

fit4 <- lm(y~ x+xsq)

anova(fit2)

xv <- seq(min(x), max(x), 0.01)

yv <- predict(fit2, list(x = xv, xsq = xv^2))

lines(xv, yv, col = "violet")


#Residuen
plot(fit2, which=1)



plot(fit2, which=2)




------------------------------------------------------------------------------------------------------
  
  
