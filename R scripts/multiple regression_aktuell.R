library(readxl)

Fifa20 <- read_excel("Fifa20_0_Mio.xlsx", na="NA")

data_ST <- subset(Fifa20, player_positions == "ST")

# level - level including delta_potential and international_reputation
# with delta_potential being (potential-overall)

model <- lm(value_eur ~ shooting + passing + pace + defending + physic + dribbling + delta_potential 
             +international_reputation, data=data_ST)

summary(model)

# negative intercept and bad Rsquared, being extremly  bad in expecting high valued players

# put every variable to z standard => making them comparable

data_ST$Zvalue_eur <- scale(data_ST$value_eur)

data_ST$Zshooting <- scale(data_ST$shooting)

data_ST$Zpassing <- scale(data_ST$passing)

data_ST$Zdefending <- scale(data_ST$defending)

data_ST$Zpace <- scale(data_ST$pace)

data_ST$Zphysic <- scale(data_ST$physic)

data_ST$Zdribbling <- scale(data_ST$dribbling)

data_ST$Zdelta_potential <- scale(data_ST$delta_potential)

data_ST$Zreputation <- scale(data_ST$international_reputation)

modelz <- lm(Zvalue_eur ~ Zshooting + Zpassing + Zpace + Zdefending + Zphysic + Zdribbling + Zdelta_potential 
            +Zreputation, data=data_ST)

summary(modelz)

# level - level, trying out squaring all skill attributes

modelsq <- lm(value_eur ~ shooting + I(shooting^2) + passing + I(passing^2) + pace + I(pace^2) +defending + 
                I(defending^2) + physic + I(physic^2) + dribbling + I(dribbling^2) + delta_potential +
                international_reputation, data=data_ST)
summary(modelsq)

# better Rsquared than unsquared attributes but way to high intercept. Still having max residuals at +47 Mio

# log - level including delta_potential and international_reputation

modellog <- lm(I(log(value_eur)) ~ shooting + passing  + pace  + defending + 
                  physic  + dribbling  + delta_potential +
                 international_reputation, data=data_ST)
summary(modellog)

# good Rsquared but bad pvalues for passing defending

# log - level but without delta_potential and international_reputation

modellog_1 <- lm(I(log(value_eur)) ~ shooting + passing  + pace  + defending + 
                 physic  + dribbling, data=data_ST)
summary(modellog_1)

# lower Rsqaured, negative ß for passing and defending

# log - level but not including passing and defending

modellog_2 <- lm(I(log(value_eur)) ~ shooting + pace +
                 physic  + dribbling  + delta_potential +
                 international_reputation, data=data_ST)
summary(modellog_2)

# negative intercept and but besides that nice result

# trying sqaured overall rating instead of attributes

modelsq_2 <- lm(value_eur ~ overall + I(overall^2) + delta_potential +
                international_reputation, data=data_ST)
summary(modelsq_2)

# extremly high intercept + negative ß for overall 

# trying only squared not including any single terms

modelsq_3 <- lm(value_eur ~  I(shooting^2) + I(passing^2) +I(pace^2) +
                I(defending^2) + I(physic^2) + I(dribbling^2) + delta_potential +
                international_reputation, data=data_ST)
summary(modelsq_3)

# medium Rsquared + quiet interesting result BUT negative intercept 

# we need to somehow put the age variable into our model

# i assume the "rating" of the league also matters e.g. english league > danish league





