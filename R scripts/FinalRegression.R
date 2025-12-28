# final und entrümpelt

#Stürmer 1
STreg1 <-  lm(logvalue ~ shooting+pace+passing+dribbling+physic+delta_potential+age+I(age^2), data=data_ST)
summary(STreg1)

STuw1 <- cor(data_ST$value_eur, exp(-3.0142593 + 0.1336049*data_ST$shooting+ 0.0069052*data_ST$pace + 0.0071963*data_ST$passing + 0.0460197*data_ST$dribbling + 0.0256360*data_ST$physic+ 0.0372381*data_ST$delta_potential + 0.2367396*data_ST$age + -0.0053144*I(data_ST$age^2)))

#oder 2

STreg2 <-  lm(logvalue ~ shooting+pace+passing+dribbling+physic+international_reputation+delta_potential+age+I(age^2), data=data_ST)
summary(STreg2)

STuw2 <- cor(data_ST$value_eur, exp(-3.0471515 + 0.1307435*data_ST$shooting+ 0.0068784*data_ST$pace + 0.0054395*data_ST$passing + 0.0462607*data_ST$dribbling + 0.0257944*data_ST$physic+ 0.1414113*data_ST$international_reputation + 0.0357015*data_ST$delta_potential + 0.2494411*data_ST$age + -0.0055993*I(data_ST$age^2)))


#mal mit stadartisierten variablen probieren





 

#Verteidiger

CBreg <- lm(logvalue ~ pace+passing+dribbling+defending+physic+international_reputation+delta_potential+age+I(age^2), data=data_CB)
summary(CBreg)

CBuw <- cor(data_CB$value_eur, exp(-3.8034828 + 0.0022166*data_CB$pace+ 0.0093074*data_CB$passing + 0.0087610*data_CB$dribbling + 0.1600689*data_CB$defending + 0.0373455*data_CB$physic + 0.0574561*data_CB$international_reputation+ 0.0381540*data_CB$delta_potential+  0.2975412*data_CB$age+ -0.0067250*I(data_CB$age^2)))


#Ergebnisse Stürmer 2
uST <- residuals(STreg2)
VarST <- var(uST)

aST <- (1/1783)*sum(exp(uST))

vers642ST <- function(shooting, pace, passing, dribbling, physic, delta_potential, international_reputation, age) {
  y <- aST*exp(-3.0471515 + 0.1307435*data_ST$shooting+ 0.0068784*data_ST$pace + 0.0054395*data_ST$passing + 0.0462607*data_ST$dribbling + 0.0257944*data_ST$physic+ 0.1414113*data_ST$international_reputation + 0.0357015*data_ST$delta_potential + 0.2494411*data_ST$age + -0.0055993*I(data_ST$age^2))
  return(y)
}

predictionsST <- vers642ST(data_ST$shooting, data_ST$pace, data_ST$passing, data_ST$dribbling, data_ST$physic, data_ST$international_reputation, data_ST$delta_potential, data_ST$age)
predictionsST

MpredictionsST <- predictionsST/1000 

plot(MpredictionsST,data_ST$value_eur/1000)
abline(a=0,b=1, col="red")

#ERgebnisse Verteidiger
uCB <- residuals(CBreg)
VarCB <- var(uCB)

aCB <- (1/2289)*sum(exp(uCB))

vers642CB <- function(pace, passing, dribbling, defending, physic, delta_potential, international_reputation, age) {
  z <- aCB*exp(-3.8034828 + 0.0022166*data_CB$pace+ 0.0093074*data_CB$passing + 0.0087610*data_CB$dribbling + 0.1600689*data_CB$defending + 0.0373455*data_CB$physic + 0.0574561*data_CB$international_reputation+ 0.0381540*data_CB$delta_potential+  0.2975412*data_CB$age+ -0.0067250*I(data_CB$age^2))
  return(z)
}

predictionsCB <- vers642CB(data_CB$pace, data_CB$passing, data_CB$dribbling, data_CB$defending, data_CB$physic, data_CB$international_reputation, data_CB$delta_potential, data_CB$age)
predictionsCB

MpredictionsCB <- predictionsCB/1000 

plot(MpredictionsCB, data_CB$value_eur/1000)
abline(a=0,b=1, col="red")

