#Multiple Regression mit Logvalues
LogReg <- lm(logvalue~ shooting+pace+passing+dribbling+defending+physic, data=data_ST)
summary(LogReg)


#GM 6 ; Normalverteilte Residuen
residuals(LogReg)
rstandard(LogReg)

hist(rstandard(LogReg))
qqnorm(rstandard(LogReg))
qqline(rstandard(LogReg))
shapiro.test(rstandard(LogReg))   #nicht aussagekräftig weill zu große Stichprobe

#Vorraussetzung gegeben


#GM 5 Homoskedastizität gegeben? -> Konstanz der Varianz der Fehlerterme (Residuen)

plot(fitted.values(LogReg),residuals(LogReg))
plot(fitted.values(LogReg),rstandard(LogReg))
plot(LogReg,1)

#Heteroskedastizität gegeben, jetzt müssen wir beheben
install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)


coeftest(LogReg, vcov=vcovHC(LogReg, type=c("HC3")))
#mit diesen "robusten Standartfehlern" weitermachen. aber wie?
#stopp, vielleicht doch keine heteroskedastizität
plot(LogReg) #3 plott mit rstandard




#GM 3 MultiKolinearität
library(car)

vif(LogReg)
1/vif(LogReg)

#vif Werte (varianz inflations faktoren) unter 10, also annehmbar (einer über 5)?

#Autokorrelation ; nur bei Zeitreihen wichtig???

dwtest(LogReg) #Durbin Watson TEst schaut schlecht aus. Mindestens zw. 1 u. 3.
