# multiple lm für logumwandlung
MREG <-  lm(logvalue ~ shooting+pace+passing+dribbling+defending+physic, data=data_ST)

summary(MREG)

u <- residuals(MREG)  #u = residuen

Var <- var(u)  #Var = Varianz der Residuen


#hier habe ich (glaube ich) die einfache smearing transformation gemacht, die ergebnisse verstehe ich aber nicht. Im Buch ist es Formel [6.40]

#smearing <- function(shooting, pace, passing, dribbling, defending, physic) {
#  transformation <- exp(Var/2)*exp(0.851646+0.127135*shooting+ 0.127135*pace + -0.007305*passing + 0.048081*dribbling + -0.003305*defending + 0.016120*physic)
#  return(transformation)
#}

#smearing(data_ST$shooting, data_ST$pace, data_ST$passing, data_ST$dribbling, data_ST$defending, data_ST$physic)




a <- (1/1783)*sum(exp(u))  #a nach Formel [6.43] ausgerechnet
print(a)



#das ist eine Funktion die die Ergebnisse für Formel [6.42] ausgibt. 
vers642 <- function(shooting, pace, passing, dribbling, defending, physic) {
      y <- a*exp(0.851646 + 0.127135*shooting+ 0.015270*pace + -0.007305*passing + 0.048081*dribbling + -0.003305*defending + 0.016120*physic)
      return(y)
}

#weil das oben eine funktion ist, muss man hier noch mal die daten einfüttern.
#Predictions ist das finale ergebnis
predictions <- vers642(data_ST$shooting, data_ST$pace, data_ST$passing, data_ST$dribbling, data_ST$defending, data_ST$physic)
predictions


plot(vers642(data_ST$shooting, data_ST$pace, data_ST$passing, data_ST$dribbling, data_ST$defending, data_ST$physic), data_ST$value_eur)
abline(a=0, b=1)



#für diese Goodness-of-fit geschichte braucht man dieses m 
erzeugeM <- function(shooting, pace, passing, dribbling, defending, physic) {
    zwischenschritt <- exp(0.851646 + 0.127135*shooting+ 0.015270*pace + -0.007305*passing + 0.048081*dribbling + -0.003305*defending + 0.016120*physic)
    return(zwischenschritt)
}

#wieder daten in die funktion füttern
m <- erzeugeM(data_ST$shooting, data_ST$pace, data_ST$passing, data_ST$dribbling, data_ST$defending, data_ST$physic)


#unser modell erklärt value_eur zu 84%  (= (correlation zwischen value und m)^2)
goodnessoffit <- cor(data_ST$value_eur, m)^2
goodnessoffit



#Fun fact (steht auch im lehrbuch): ich hätte das m nicht extra nehmen müssen, denn auch wenn man das a drinnen lässt, kommt dasselbe raus
goodnessGLEICH <- (cor(data_ST$value_eur, predictions))^2





#regressionstesten
#schlechte wete weggelassen
reg1 <- lm(logvalue ~ shooting+pace+dribbling+physic, data=data_ST)
summary(reg1)

uw1 <- cor(data_ST$value_eur, exp(0.971096 + 0.124414*data_ST$shooting+ 0.016199*data_ST$pace + 0.041979*data_ST$dribbling + 0.014719*data_ST$physic))


#deltapotential
reg2 <- lm(logvalue ~ shooting+pace+dribbling+physic+delta_potential, data=data_ST)
summary(reg2)

uw2 <- cor(data_ST$value_eur, exp(-0.920685 + 0.133700*data_ST$shooting+ 0.012557*data_ST$pace + 0.047869*data_ST$dribbling + 0.028013*data_ST$physic+ 0.053200*data_ST$delta_potential))


#internationale reputation
reg3 <- lm(logvalue ~ shooting+pace+dribbling+physic+delta_potential+international_reputation, data=data_ST)
summary(reg3)

uw3 <- cor(data_ST$value_eur, exp(-0.893206 + 0.132680*data_ST$shooting+ 0.012557*data_ST$pace + 0.047869*data_ST$dribbling + 0.028013*data_ST$physic+ 0.053200*data_ST$delta_potential))
#wage
reg4 <- lm(logvalue ~ shooting+pace+dribbling+physic+delta_potential+wage_eur, data=data_ST)
summary(reg4)

uw4 <- cor(data_ST$value_eur, exp(-3.994e-01 + 1.276e-01*data_ST$shooting+ 1.219e-02*data_ST$pace + 4.628e-02*data_ST$dribbling + 2.754e-02*data_ST$physic+ 5.081e-02*data_ST$delta_potential+ 3.990e-06*data_ST$wage_eur))


#overall
reg5 <- lm(logvalue ~ shooting+pace+dribbling+physic+delta_potential+overall, data=data_ST)
summary(reg5)

#data_LigaST also anderes datenset
reg6 <- lm(logvalue ~ shooting+pace+dribbling+physic+delta_potential, data=data_LigaST)
summary(reg6)

uw6 <- cor(data_LigaST$value_eur, exp(-0.638719 + 0.126452*data_LigaST$shooting+ 0.011210*data_LigaST$pace + 0.051515*data_LigaST$dribbling + 0.032296*data_LigaST$physic+ 0.035074*data_LigaST$delta_potential))

#WTF passiert
reg7 <- lm(logvalue ~ shooting+pace+dribbling+physic+delta_potential+ligaskala, data=data_LigaST)
summary(reg7)

reg8 <- lm(logvalue ~ shooting+pace+dribbling+physic+delta_potential+age+I(age^2), data=data_ST)
summary(reg8)

#wir suchen unsere ergebnisse mit dem höchsten goodnes of fit 
uST <- residuals(reg2)
VarST <- var(uST)

aST <- (1/1783)*sum(exp(uST))

vers642ST <- function(shooting, pace, passing, dribbling, physic, delta_potential) {
  x <- aST*exp(-0.920685 + 0.133700*data_ST$shooting+ 0.012557*data_ST$pace + 0.047869*data_ST$dribbling + 0.028013*data_ST$physic+ 0.053200*data_ST$delta_potential)
  return(x)
}

predictionsST <- vers642ST(data_ST$shooting, data_ST$pace, data_ST$passing, data_ST$dribbling, data_ST$physic, data_ST$delta_potential)
predictionsST



#Verteidiger
#reg ohne Passing weil nicht signifikant und winzig
CBreg1 <- lm(logvalue ~ shooting+pace+dribbling+defending+physic, data=data_CB)
summary(CBreg1)

CBuw1 <- cor(data_CB$value_eur, exp(0.3955976 + -0.0058340*data_CB$shooting+ 0.0195874*data_CB$pace + 0.0106430*data_CB$dribbling + 0.1538001*data_CB$defending + 0.0245198*data_CB$physic))


CBreg2 <- lm(logvalue ~ shooting+pace+passing+dribbling+defending+physic+delta_potential, data=data_CB)
summary(CBreg2)

CBuw2 <- cor(data_CB$value_eur, exp(-1.7286403 + -0.0044944*data_CB$shooting+ 0.0106919*data_CB$pace+ 0.0065566*data_CB$passing + 0.0097136*data_CB$dribbling + 0.1623112*data_CB$defending + 0.0444130*data_CB$physic + 0.0681908*data_CB$delta_potential))
#ohne passing, weil es beim ersten nicht signifikant war 
CBreg2b <- lm(logvalue ~ shooting+pace+dribbling+defending+physic+delta_potential, data=data_CB)
summary(CBreg2b)
CBuw2b <- cor(data_CB$value_eur, exp(-1.7335790 + -0.0025852*data_CB$shooting+ 0.0104235*data_CB$pace + 0.0131746*data_CB$dribbling + 0.1637088*data_CB$defending + 0.0444515*data_CB$physic + 0.0674954*data_CB$delta_potential))
#auch hne shooting weil jetzt auch schlechte ssignifikanzniveau
CBreg2c <- lm(logvalue ~ pace+dribbling+defending+physic+delta_potential, data=data_CB)
summary(CBreg2c)
CBuw2c <- cor(data_CB$value_eur, exp(-1.7334730 + 0.0106442*data_CB$pace + 0.0118806*data_CB$dribbling + 0.1634217*data_CB$defending + 0.0441238*data_CB$physic + 0.0678745*data_CB$delta_potential))


#internat rep (wieder mit passing und schooting)
CBreg3 <- lm(logvalue ~ shooting+pace+passing+dribbling+defending+physic+delta_potential+international_reputation, data=data_CB)
summary(CBreg3)

CBuw3 <- CBuw2 <- cor(data_CB$value_eur, exp(-1.7737933 + -0.0038508*data_CB$shooting+ 0.0107478*data_CB$pace+ 0.0066020*data_CB$passing + 0.0096307*data_CB$dribbling + 0.1651418*data_CB$defending + 0.0436623*data_CB$physic + 0.0684235*data_CB$delta_potential+ -0.1018100*data_CB$international_reputation))


CBreg4 <- lm(logvalue ~ shooting+pace+passing+dribbling+defending+physic+delta_potential+international_reputation+age+I(age^2), data=data_CB)
summary(CBreg4)

CBuw4 <- cor(data_CB$value_eur, exp(-3.803e+00 + -2.583e-05*data_CB$shooting+ 2.216e-03*data_CB$pace+ 9.322e-03*data_CB$passing + 8.764e-03*data_CB$dribbling + 1.601e-01*data_CB$defending + 3.735e-02*data_CB$physic + 3.815e-02*data_CB$delta_potential+ 5.750e-02*data_CB$international_reputation+ 2.975e-01*data_CB$age+ -6.725e-03*I(data_CB$age^2)))




#wir suchen unsere ergebnisse
uCB <- residuals(CBreg4)
VarCB <- var(uCB)

aCB <- (1/2289)*sum(exp(uCB))

vers642CB <- function(shooting, pace, passing, dribbling, defending, physic, delta_potential, international_reputation, age) {
  z <- a*exp(-3.803e+00 + -2.583e-05*data_CB$shooting+ 2.216e-03*data_CB$pace+ 9.322e-03*data_CB$passing + 8.764e-03*data_CB$dribbling + 1.601e-01*data_CB$defending + 3.735e-02*data_CB$physic + 3.815e-02*data_CB$delta_potential+ 5.750e-02*data_CB$international_reputation+ 2.975e-01*data_CB$age+ -6.725e-03*I(data_CB$age^2))
  return(z)
}

predictionsCB <- vers642CB(data_CB$shooting, data_CB$pace, data_CB$passing, data_CB$dribbling, data_CB$defending, data_CB$physic, data_CB$delta_potential, data_CB$international_reputation, data_CB$age)
predictionsCB

