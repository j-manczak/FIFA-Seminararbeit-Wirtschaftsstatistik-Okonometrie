#test
plot(data_ST$shooting, data_ST$overall)

prüfen <- lm(data_ST$overall~data_ST$shooting)
abline(prüfen, col="red")
summary(prüfen)


plot(data_ST$attacking_finishing, data_ST$shooting)
prüfen2 <- lm(data_ST$shooting~data_ST$attacking_finishing)
abline(prüfen2, col ="red")
summary(prüfen2)

#stimmt die zusammensetzung der Skill-Übervariablen, die der Jakob gefunden hat? ja!
prüfen3 <- lm(shooting~ attacking_finishing+ attacking_volleys+power_long_shots+ power_shot_power+mentality_positioning+mentality_penalties,data = data_ST)
summary(prüfen3)

#stimmt zusammensetzung des overall Werts für STürmer, die der Patrik uns gezeigt hat? quasi!
prüfen4 <- lm(data_ST$overall~data_ST$attacking_finishing+data_ST$attacking_heading_accuracy+data_ST$attacking_short_passing+data_ST$attacking_volleys+data_ST$skill_dribbling+data_ST$skill_ball_control+data_ST$movement_acceleration+data_ST$movement_sprint_speed+data_ST$movement_reactions+data_ST$power_shot_power+data_ST$power_strength+data_ST$power_long_shots+data_ST$mentality_positioning)
summary(prüfen4)

#lm mit die Skill-Überkategorien
prüfen5 <- lm(logvalue~ shooting+pace+passing+dribbling+defending+physic, data=data_ST)
summary(prüfen5)

