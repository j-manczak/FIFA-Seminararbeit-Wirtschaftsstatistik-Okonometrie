install.packages("readxl")
library(readxl)
data_fifa <- read_excel("Fifa20.xlsx", na="NA", sheet = 1)

