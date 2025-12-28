#bevor es losgeht "scrips for R" as working directory

# Nur Stürmer und nur Wert zwischen a und b

library(readxl)
Fifa20 <- read_excel("Fifa20_Mio.xlsx")
data_ST <- subset(Fifa20, player_positions == "ST")
data_ST_valuerange <- subset(data_ST, value_mio < 20 & value_mio >1)

# variablen festlegen

y <- data_ST_valuerange$value_mio

x <- data_ST_valuerange$age

#Altersvariable quadrieren

xsq <- x^2

xcub = x^3

plot(x,y, xlab = "age [years]", ylab = "value [Mio. Euro]")

#model fit

fit1 <- lm(y~x)

anova(fit1)

abline(fit1, col = "red")

#model2 mit Quadrat

fit2 <- lm(y~ x+xsq)

anova(fit2)

xv <- seq(min(x), max(x), 0.01)

yv <- predict(fit2, list(x = xv, xsq = xv^2))

lines(xv, yv, col = "red")

#Residuen
plot(fit2, which=1)



plot(fit2, which=2)

