#hier erstelle ich alle Variablen und Datensets die ich brauche

data_fifa$delta_potential <- data_fifa$potential - data_fifa$overall




data_ST <- subset(data_fifa, player_positions == "ST")
data_CB <- subset(data_fifa, player_positions ==  "CB")


data_ST$logvalue <- log(data_ST$value_eur)
data_ST$logwage <- log(data_ST$wage_eur)
data_CB$logvalue <- log(data_CB$value_eur)

#alle Z variablen mit scale befehl standardisrert
data_ST$Zoverall <- scale(data_ST$overall)
data_ST$Zrep <- scale(data_ST$international_reputation)
data_ST$Zlogvalue <- scale(data_ST$logvalue)
data_ST$Zlogwage <- scale(data_ST$logwage)

#test
data_overrep <- subset(data_ST, select = c(Zlogvalue, Zoverall, Zrep))

#datenset nur mit den Skill-Überkategorien erstellt
data_STreduced <- subset(data_ST, select = c(pace, shooting, passing, dribbling,defending, physic)) 



#LIGA
install.packages("dplyr")
library(dplyr)


bundesliga <- c(
  "1. FC NĂĽrnberg", "1. FSV Mainz 05", "Bayer 04 Leverkusen", "FC Bayern MĂĽnchen",
  "Borussia Dortmund", "Borussia MĂ¶nchengladbach", "Eintracht Frankfurt",
  "FC Augsburg", "FC Schalke 04", "Fortuna DĂĽsseldorf", "Hannover 96",
  "Hertha BSC", "RB Leipzig", "SC Freiburg", "TSG 1899 Hoffenheim",
  "VfB Stuttgart", "VfL Wolfsburg", "SV Werder Bremen"
)

premierLeague <- c(
  "Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
  "Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
  "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
  "Manchester United", "Newcastle United", "Southampton", 
  "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers"
)

laliga <- c(
  "Athletic Club de Bilbao", "AtlĂ©tico Madrid", "CD Leganés",
  "Deportivo AlavĂ©s", "FC Barcelona", "Getafe CF", "Girona FC", 
  "Levante UD", "Rayo Vallecano", "RC Celta", "RCD Espanyol", 
  "Real Betis", "Real Madrid", "Real Sociedad", "Real Valladolid CF",
  "SD Eibar", "SD Huesca", "Sevilla FC", "Valencia CF", "Villarreal CF"
)

seriea <- c(
  "Atalanta","Bologna","Cagliari","Chievo Verona","Empoli", "Fiorentina","Frosinone","Genoa",
  "Inter","Juventus","Lazio","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL",
  "Torino","Udinese"
  
)

superlig <- c(
  "Akhisar Belediyespor","Alanyaspor", "Antalyaspor","Medipol BaĹźakĹźehir FK","BB Erzurumspor","BeĹźiktaĹź JK",
  "Bursaspor","Ă‡aykur Rizespor","FenerbahĂ§e SK", "Galatasaray SK","GĂ¶ztepe SK","KasimpaĹźa SK",
  "Kayserispor","Atiker Konyaspor","MKE AnkaragĂĽcĂĽ", "Sivasspor","Trabzonspor","Yeni Malatyaspor"
)

ligue1 <- c(
  "Amiens SC", "Angers SCO", "AS Monaco", "AS Saint-Ă‰tienne", "Dijon FCO", "En Avant de Guingamp",
  "FC Nantes", "FC Girondins de Bordeaux", "LOSC Lille", "Montpellier HSC", "NĂ®mes Olympique", 
  "OGC Nice", "Olympique Lyonnais","Olympique de Marseille", "Paris Saint-Germain", 
  "RC Strasbourg Alsace", "Stade Malherbe Caen", "Stade de Reims", "Stade Rennais FC", "Toulouse Football Club"
)

eredivisie <- c(
  "ADO Den Haag","Ajax", "AZ Alkmaar", "De Graafschap","Excelsior","FC Emmen","FC Groningen",
  "FC Utrecht", "Feyenoord","Fortuna Sittard", "Heracles Almelo","NAC Breda",
  "PEC Zwolle", "PSV","SC Heerenveen","Vitesse","VVV-Venlo","Willem II"
)

liganos <- c(
  "Os Belenenses", "Boavista FC", "CD Feirense", "CD Tondela", "CD Aves", "FC Porto",
  "CD Nacional", "GD Chaves", "Clube Sport Marítimo", "Moreirense FC", "Portimonense SC", "Rio Ave FC",
  "Santa Clara", "SC Braga", "SL Benfica", "Sporting CP", "VitĂłria GuimarĂŁes", "VitĂłria de SetĂşbal"
)



data_fifa$liga = case_when( 
      data_fifa$club %in% bundesliga ~ "Deutschland",
      data_fifa$club %in% premierLeague ~ "England",
      data_fifa$club %in% laliga ~ "Spanien",
      data_fifa$club %in% seriea ~ "Italien",
      data_fifa$club %in% superlig ~ "Türkei",
      data_fifa$club %in% ligue1 ~ "Frankreich",
      data_fifa$club %in% eredivisie ~ "Niederlande",
      data_fifa$club %in% liganos ~ "Portugal"
  )


test <- subset(data_fifa, club != "NA")

#stürmer mit ligadaten
#data_LigaST <- subset(data_ST, liga != "NA")  #390 datenpunkte



#deutsche liga alle spieler 

data_Bundesliga <- subset(data_fifa, liga == "Deutschland")
data_Laliga <- subset(data_fifa, liga == "Spanien" )
data_Superlig <- subset(data_fifa, liga == "Türkei")

data_Premierleague <- subset(data_fifa, liga =="England")

data_Seriea <- subset(data_fifa, liga == "Italien")
data_Ligue1 <- subset(data_fifa, liga == "Frankreich")
data_Eredevise <- subset(data_fifa, liga == "Niederlande")
data_Liganos <- subset(data_fifa, liga == "Portugal")





#ligaskala einführen
data_fifa$ligaskala = case_when( 
  data_fifa$liga == "Deutschland" ~ 71.3633,
  data_fifa$liga == "England" ~ 100,
  data_fifa$liga == "Spanien" ~ 96.7979,
  data_fifa$liga == "Italien" ~ 67.328,
  data_fifa$liga == "Türkei" ~ 1,
  data_fifa$liga == "Frankreich" ~ 37.6525,
  data_fifa$liga == "Niederlande" ~ 1.228,
  data_fifa$liga == "Portugal" ~ 20.11
)




#nur für jetzt
data_ST$ligaskala = case_when( 
  data_ST$liga == "Deutschland" ~ 71.3633,
  data_ST$liga == "England" ~ 100,
  data_ST$liga == "Spanien" ~ 96.7979,
  data_ST$liga == "Italien" ~ 67.328,
  data_ST$liga == "Türkei" ~ 1,
  data_ST$liga == "Frankreich" ~ 37.6525,
  data_ST$liga == "Niederlande" ~ 1.228,
  data_ST$liga == "Portugal" ~ 20.11
)

#nur für jetzt CB
data_CB$ligaskala = case_when( 
  data_CB$liga == "Deutschland" ~ 71.3633,
  data_CB$liga == "England" ~ 100,
  data_CB$liga == "Spanien" ~ 96.7979,
  data_CB$liga == "Italien" ~ 67.328,
  data_CB$liga == "Türkei" ~ 1,
  data_CB$liga == "Frankreich" ~ 37.6525,
  data_CB$liga == "Niederlande" ~ 1.228,
  data_CB$liga == "Portugal" ~ 20.11
)


#erzeuge subset fpr stürmer mit liga
data_LigaST <- subset(data_ST, liga != "NA")

data_LigaCB <- subset(data_CB, liga != "NA")