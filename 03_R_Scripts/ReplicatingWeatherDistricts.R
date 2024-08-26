install.packages("BiocManager")
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

pckgs <- c("tidyverse", "readr", "ggplot2", "MODIStsp", "sf", "readxl", 'tmap', 'rmapshaper',
           'janitor', 'terra', 'geodata', "dplyr", 
           "PrevMap", "mapview", "automap", "spdep", "spatialreg",
           "geoR", 'gdistance', 'Hmisc', 'raster', "pals",
           "tmap", "readxl", "zoo", "lubridate", "padr", "hms", "purrr", "gstat" ,"sp", "exactextractr")

options(repos = BiocManager::repositories())
pacman::p_load (pckgs, character.only=TRUE)


#Set Raw Data as my working directory
setwd("~/LSHTM_23/Thesis/Lima_Dengue/")

# Import all my incidence Data
incidence <- read.csv("01_Messy_Data/Individual_Incidence.csv")
shps_districts <- st_read("02_Cleaned_Data/AdminBoundaries/Lima_Districts.shp")
plot(shps_districts)
#Demographics
demographics <- read.csv("02_Cleaned_Data/Cleaned_Demographics.csv")
#Weather
ancon <- read.csv("02_Cleaned_Data/ancon_climate_clean.csv")
carabayllo <- read.csv("02_Cleaned_Data/carabayllo_climate_clean.csv")
jesusmaria <- read.csv("02_Cleaned_Data/JesusMaria_climate_clean.csv")
lamolina <- read.csv("02_Cleaned_Data/LaMolina_climate_clean.csv")
lurigancho <- read.csv("02_Cleaned_Data/Lurigancho_Chosica_climate_clean.csv")
sanborja <- read.csv("02_Cleaned_Data/SanBorja_climate_clean.csv")
sanjuandelur <- read.csv("02_Cleaned_Data/SanJuandeLurigancho_climate_clean.csv")
sanmartin <- read.csv("02_Cleaned_Data/SanMartinDePorres_climate_clean.csv")
santaanita <- read.csv("02_Cleaned_Data/SantaAnita_climate_clean.csv")
villamaria <- read.csv("02_Cleaned_Data/VillaMariaDelTriunfo_climate_clean.csv")

#Copy each district and rename the district name to the new district
# Ancon
SantaRosa <- ancon
SantaRosa$District[SantaRosa$District == 'ANCON'] <- 'SANTA ROSA'
#Carabayllo
Puentepiedra <- carabayllo
Puentepiedra$District[Puentepiedra$District == 'CARABAYLLO'] <- 'PUENTE PIEDRA'
# San Juan del
Comas <- sanjuandelur
Comas$District[Comas$District == 'SAN JUAN DE LURIGANCHO'] <- 'COMAS'

Independencia <- sanjuandelur
Independencia$District[Independencia$District == 'SAN JUAN DE LURIGANCHO'] <- 'INDEPENDENCIA'
# San Martin
Losolivos <- sanmartin
Losolivos$District[Losolivos$District == 'SAN MARTIN DE PORRES'] <- 'LOS OLIVOS'

Rimac <- sanmartin
Rimac$District[Rimac$District == 'SAN MARTIN DE PORRES'] <- 'RIMAC'

Lima <- sanmartin
Lima$District[Lima$District == 'SAN MARTIN DE PORRES'] <- 'LIMA'
# Jesus Maria
Brena <- jesusmaria
Brena$District[Brena$District == 'JESUS MARIA'] <- 'BRENA'

PuebloLibre <- jesusmaria
PuebloLibre$District[PuebloLibre$District == 'JESUS MARIA'] <- 'PUEBLO LIBRE'

sanmiguel <- jesusmaria
sanmiguel$District[sanmiguel$District == 'JESUS MARIA'] <- 'SAN MIGUEL'

magdelmar <- jesusmaria
magdelmar$District[magdelmar$District == 'JESUS MARIA'] <- 'MAGDALENA DEL MAR'

Lince <- jesusmaria
Lince$District[Lince$District == 'JESUS MARIA'] <- 'LINCE'

Isidro <- jesusmaria
Isidro$District[Isidro$District == 'JESUS MARIA'] <- 'SAN ISIDRO'
# Santa Anita
elagust <- santaanita
elagust$District[elagust$District == 'SANTA ANITA'] <- 'EL AGUSTINO'
# Lurigancho
ate <- lurigancho
ate$District[ate$District == 'LURIGANCHO'] <- 'ATE'

Chaclacayo <- lurigancho
Chaclacayo$District[Chaclacayo$District == 'LURIGANCHO'] <- 'CHACLACAYO'

cieneguilla <- lurigancho
cieneguilla$District[cieneguilla$District == 'LURIGANCHO'] <- 'CIENEGUILLA'
# San Borja
lavictoria <- sanborja
lavictoria$District[lavictoria$District == 'SAN BORJA'] <- 'LA VICTORIA'

sanluis <- sanborja
sanluis$District[sanluis$District == 'SAN BORJA'] <- 'SAN LUIS'

surquillo <- sanborja
surquillo$District[surquillo$District == 'SAN BORJA'] <- 'SURQUILLO'

miraflores <- sanborja
miraflores$District[miraflores$District == 'SAN BORJA'] <- 'MIRAFLORES'

santiago <- sanborja
santiago$District[santiago$District == 'SAN BORJA'] <- 'SANTIAGO DE SURCO'

barranco <- sanborja
barranco$District[barranco$District == 'SAN BORJA'] <- 'BARRANCO'


# Villa Maria
chorrillos <- villamaria
chorrillos$District[chorrillos$District == 'VILLA MARIA DEL TRIUNFO'] <- 'CHORRILLOS'

juanmiraflores <- villamaria
juanmiraflores$District[juanmiraflores$District == 'VILLA MARIA DEL TRIUNFO'] <- 'SAN JUAN DE MIRAFLORES'

villaelsav <- villamaria
villaelsav$District[villaelsav$District == 'VILLA MARIA DEL TRIUNFO'] <- 'VILLA EL SALVADOR'

pachacamac <- villamaria
pachacamac$District[pachacamac$District == 'VILLA MARIA DEL TRIUNFO'] <- 'PACHACAMAC'

lurin <- villamaria
lurin$District[lurin$District == 'VILLA MARIA DEL TRIUNFO'] <- 'LURIN'

puntaherm <- villamaria
puntaherm$District[puntaherm$District == 'VILLA MARIA DEL TRIUNFO'] <- 'PUNTA HERMOSA'

puntanegra <- villamaria
puntanegra$District[puntanegra$District == 'VILLA MARIA DEL TRIUNFO'] <- 'PUNTA NEGRA'

sanbartolo <- villamaria
sanbartolo$District[sanbartolo$District == 'VILLA MARIA DEL TRIUNFO'] <- 'SAN BARTOLO'

santmadelmar <- villamaria
santmadelmar$District[santmadelmar$District == 'VILLA MARIA DEL TRIUNFO'] <- 'SANTA MARIA DEL MAR'

pucusana <- villamaria
pucusana$District[pucusana$District == 'VILLA MARIA DEL TRIUNFO'] <- 'PUCUSANA'

# merge them all together
district_list <- list(ancon, ate, barranco, Brena, carabayllo, Chaclacayo, chorrillos, cieneguilla, Comas,
                      elagust, Independencia, Isidro, jesusmaria, juanmiraflores, lamolina, lavictoria, Lima, Lince,
                      Losolivos, lurigancho, lurin, PuebloLibre, magdelmar, miraflores, pachacamac, pucusana, Puentepiedra,
                      puntaherm, puntanegra, Rimac, sanbartolo, sanborja, sanjuandelur, sanluis, sanmartin, sanmiguel,
                      santaanita, SantaRosa, santiago, santmadelmar, surquillo, villaelsav, villamaria)
all_weather <- do.call(rbind, district_list)


setwd("~/LSHTM_23/Thesis/Lima_Dengue/02_Cleaned_Data")
write.csv(all_weather, "allweather.csv", row.names = FALSE)


