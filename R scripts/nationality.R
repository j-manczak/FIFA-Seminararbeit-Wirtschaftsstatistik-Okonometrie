#hier erstelle ich alle Variablen und Datensets die ich brauche

install.packages("readxl")
library(readxl)
data_fifa <- read_excel("data_fifa.xlsx", na="NA", sheet = 1)
#data_fifa <- select(data_fifa$value_eur > 1)  Ich habe diese Spieler manuell aus Excel entfernt


require(stats)
library(tidyverse)
library(ggplot2)
library(dplyr)


germany <- subset(data_fifa, nationality == "Germany")
poland <- subset(data_fifa, nationality =="Poland")
england <- subset(data_fifa, nationality == "England")
Czech_Republic <- subset(data_fifa, nationality == "Czech Republic")
Turkey <- subset(data_fifa, nationality == "Turkey")
Serbia <- subset(data_fifa, nationality == "Serbia")
Slovenia <-subset(data_fifa, nationality == "Slovenia")


#hier erstelle ich x & y Achse - Werte - log_Werte und Overall

#Alle Spieler

all_value_log <- log(data_fifa$value_eur)
all_value_log
all_plot <- plot(all_value_log, data_fifa$overall, col = "brown")

all_lm <- lm(data_fifa$overall ~ all_value_log)
abline(all_lm, col ="brown")

#Germany

germany_value_log <- log(germany$value_eur)
germany_value_log
germany_plot <- plot(germany_value_log, germany$overall, col = "red")

germany_overall <- germany$overall
germany_overall

germany_lm <- lm(germany$overall ~ germany_value_log)
#summary(germany_lm)
abline(germany_lm, col= "red")

#Poland

poland_value_log <- log(poland$value_eur)
poland_value_log
poland_plot <- plot(poland_value_log, poland$overall, col = "blue")

poland_lm <- lm(poland$overall ~ poland_value_log)
abline(poland_lm, col ="blue")

#England

england_value_log <- log(england$value_eur)
england_value_log
england_plot <- plot(england_value_log, england$overall, col = "pink")

england_lm <- lm(england$overall ~ england_value_log)
abline(england_lm, col ="pink")


#Czech_Republic

Czech_Republic_value_log <- log(Czech_Republic$value_eur)
Czech_Republic_value_log
Czech_Republic_plot <- plot(Czech_Republic_value_log, Czech_Republic$overall, col = "black")

england_lm <- lm(Czech_Republic$overall ~ Czech_Republic_value_log)
abline(england_lm, col ="black")


#Turkey


Turkey_value_log <- log(Turkey$value_eur)
Turkey_value_log
Turkey_plot <- plot(Turkey_value_log, Turkey$overall, col = "yellow")

Turkey_lm <- lm(Turkey$overall ~ Turkey_value_log)
abline(Turkey_lm, col ="yellow")



#Serbia


Serbia_value_log <- log(Serbia$value_eur)
Serbia_value_log
Serbia_plot <- plot(Serbia_value_log, Serbia$overall, col = "orange")

Serbia_lm <- lm(Serbia$overall ~ Serbia_value_log)
abline(Serbia_lm, col ="orange")



#Slovenia


Slovenia_value_log <- log(Slovenia$value_eur)
Slovenia_value_log
Slovenia_plot <- plot(Slovenia_value_log, Slovenia$overall, col = "violet")

Slovenia_lm <- lm(Slovenia$overall ~ Slovenia_value_log)
abline(Slovenia_lm, col ="violet")



# Mehrere Plots in 1


plot(poland_lm, col = "blue")
points(germany_value_log, germany_overall, col = "green")

print(abline(germany_lm))   #, germany_lm$germany_value_log, col ="green")





