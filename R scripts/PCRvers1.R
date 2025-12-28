#test
plot(data_ST$overall, data_ST$logvalue)
model <- lm(logvalue ~ overall, data = data_ST)
abline(model, col="red")
summary(model)

testmod <- lm(Zlogvalue ~ Zoverall+Zrep, data = data_ST)
summary(testmod)

testmod2 <- lm(Zlogvalue ~.,data=data_overrep)
summary(testmod2)

#Anfang PCR   Das beste Youtube Video darüber, woraus ich den Code 1:1 kopiert habe: https://www.youtube.com/watch?v=HxUj3MgBRjI
reduced.pca <- prcomp(data_STreduced, center = TRUE, scale = TRUE)  #prcomp Routine macht die PrincipalComponentsAnalysis für dich. Center und Scale sollen die Variablen STandardisieren und zentrieren, allerdings weiß ich nict, ob ich das überhaupt brauche, da sie ja gleich bewertet sind.
plot(reduced.pca, type="l") #macht dieses coole Plot vom type "l" 
summary(reduced.pca)
print(reduced.pca$rotation) #zeigt die Zusammensetzung der Componenten (Mathematisch irgendwas mit rotationsmatrix)
cor(reduced.pca$x)  #hier sieht man, dass durch die PCA die components eben nicht mehr voneinander abhängig sind

#value + compontents verbinden
value.pca <- cbind(data_ST$Zlogvalue,data.frame(reduced.pca$x))
colnames(value.pca)[1] <- "Zlogvalue"   #die spaltennamen (wieder) auf Zlogvalue ändern
cor(value.pca)[,1]

#PC-Regression erstellen. Der . nach dem ~ heißt, dass er alle anderen Varibalen da rein schreibt. (hoffe ich)
value.pcr <-lm(Zlogvalue~., data=value.pca)
summary(value.pcr)

coefficients.pcr <- reduced.pca$rotation %*% value.pcr$coefficients[-1] #Durch eine Matrixmultiplikatoin bekommen wir die Coeffizienten ausgedrückt in den ursprünglichen Variablen
coefficients.pcr
