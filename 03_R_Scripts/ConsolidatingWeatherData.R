## Combining Weather csv files Script

if(!require("pak")) install.packages("pak")
install.packages("pacman")

pckgs <- c("tidyverse", "readr", "ggplot2", "MODIStsp", "dplyr", "plyr", "fs")
pacman::p_load (pckgs, character.only=TRUE)


# Import and merge all my csv for weather data for Campo de marte


# Antonio Raymondi
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
antonio_dir <- "00_Raw_Data/Antonio_Raymondi/"
#list all the csv files
fs::dir_ls(antonio_dir)
#limit the directory to just csv files
antonio_files <- fs::dir_ls(antonio_dir, regexp = "\\.csv$")
antonio_files
# MAking all the file columns characters, will need to change later
antonio_combined_files <- antonio_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(antonio_combined_files, file = "antonio_raymondi_combined.csv")
###############################################################################

# Campo De Marte
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
campo_dir <- "00_Raw_Data/Campo_De_Marte/"
#list all the csv files
fs::dir_ls(campo_dir)
#limit the directory to just csv files
campo_files <- fs::dir_ls(campo_dir, regexp = "\\.csv$")
campo_files
# MAking all the file columns characters, will need to change later
campo_Del_marta_combined_files <- campo_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
write.csv(campo_Del_marta_combined_files, file = "campo_del_marta_combined.csv")
###############################################################################

# Carabayllo
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
carabayllo_dir <- "00_Raw_Data/Carabayllo/"
#list all the csv files
fs::dir_ls(carabayllo_dir)
#limit the directory to just csv files
carabayllo_files <- fs::dir_ls(carabayllo_dir, regexp = "\\.csv$")
carabayllo_files
# MAking all the file columns characters, will need to change later
carabayllo_combined_files <- carabayllo_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(carabayllo_combined_files, file = "carabaylo_combined.csv")
###############################################################################

#Nana
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
nana_dir <- "00_Raw_Data/NaNA/"
#list all the csv files
fs::dir_ls(nana_dir)
#limit the directory to just csv files
nana_files <- fs::dir_ls(nana_dir, regexp = "\\.csv$")
nana_files
# MAking all the file columns characters, will need to change later
nana_combined_files <- nana_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(nana_combined_files, file = "nana_combined.csv")
###############################################################################

# San Borja
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
san_borja_dir <- "00_Raw_Data/San_Borja_Station/"
#list all the csv files
fs::dir_ls(san_borja_dir)
#limit the directory to just csv files
san_borja_files <- fs::dir_ls(san_borja_dir, regexp = "\\.csv$")
san_borja_files
# Making all the file columns characters, will need to change later
san_borja_combined_files <- san_borja_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(san_borja_combined_files, file = "san_borja_combined.csv")
################################################################################

# San Juan De Lurigancho
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
san_juan_dir <- "00_Raw_Data/San_Juan_De_Lurigancho/"
#list all the csv files
fs::dir_ls(san_juan_dir)
#limit the directory to just csv files
san_juan_files <- fs::dir_ls(san_juan_dir, regexp = "\\.csv$")
san_juan_files
# MAking all the file columns characters, will need to change later
san_juan_combined_files <- san_juan_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(san_juan_combined_files, file = "san_juan_combined.csv")
################################################################################

# San Martin de Porres
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
san_martin_dir <- "00_Raw_Data/San_Martin_de_Porres/"
#list all the csv files
fs::dir_ls(san_martin_dir)
#limit the directory to just csv files
san_martin_files <- fs::dir_ls(san_martin_dir, regexp = "\\.csv$")
san_martin_files
# MAking all the file columns characters, will need to change later
san_martin_combined_files <- san_martin_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(san_martin_combined_files, file = "san_martin_combined.csv")
################################################################################

# "Santa Anita"
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
santa_anita_dir <- "00_Raw_Data/Santa_Anita/"
#list all the csv files
fs::dir_ls(santa_anita_dir)
#limit the directory to just csv files
santa_anita_files <- fs::dir_ls(santa_anita_dir, regexp = "\\.csv$")
santa_anita_files
# MAking all the file columns characters, will need to change later
santa_anita_combined_files <- santa_anita_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(santa_anita_combined_files, file = "santa_anita_combined.csv")
################################################################################

# Villa Maria
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
villa_maria_dir <- "00_Raw_Data/Villa_Maria_Station/"
#list all the csv files
fs::dir_ls(villa_maria_dir)
#limit the directory to just csv files
villa_maria_files <- fs::dir_ls(villa_maria_dir, regexp = "\\.csv$")
villa_maria_files
# MAking all the file columns characters, will need to change later
villa_maria_combined_files <- villa_maria_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(villa_maria_combined_files, file = "villa_maria_combined.csv")
################################################################################

# Von Humboldt Station
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
von_humboldt_dir <- "00_Raw_Data/Von_Humboldt_Station/"
#list all the csv files
fs::dir_ls(von_humboldt_dir)
#limit the directory to just csv files
von_humboldt_files <- fs::dir_ls(von_humboldt_dir, regexp = "\\.csv$")
von_humboldt_files
# MAking all the file columns characters, will need to change later
von_humboldt_combined_files <- von_humboldt_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))
#Save in Messy Data
setwd("01_Messy_Data/")
write.csv(von_humboldt_combined_files, file = "von_humboldt_combined.csv")
########################################################################################
# Manual Weather Stations

# Manual Campo
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
manualcampo_dir <- "00_Raw_Data/Manual_Campo_De_Marte/"
#list all the csv files
fs::dir_ls(manualcampo_dir)
#limit the directory to just csv files
manualcampo_files <- fs::dir_ls(manualcampo_dir, regexp = "\\.csv$")
manualcampo_files
# MAking all the file columns characters, will need to change later
manualcampo_combined_files <- manualcampo_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))


# Manual Nana
#Storing all the csv files
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
Manualnana_dir <- "00_Raw_Data/Manual_Nana/"
#list all the csv files
fs::dir_ls(Manualnana_dir)
#limit the directory to just csv files
Manualnana_files <- fs::dir_ls(Manualnana_dir, regexp = "\\.csv$")
Manualnana_files
# MAking all the file columns characters, will need to change later
Manualnana_combined_files <- Manualnana_files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "character")))

# Change heading names into english
colnames(manualcampo_combined_files) <- c("Date", "T_Max","T_Min","Relative_Humidity_(%)",
                                          "Precipitation_(mm/h)")

colnames(Manualnana_combined_files) <- c("Date", "T_Max","T_Min","Relative_Humidity_(%)",
                                         "Precipitation_(mm/h)")
#Change data back into numeric
#Campo
manualcampo_combined_files <- manualcampo_combined_files %>% mutate_at(c("T_Max","T_Min","Relative_Humidity_(%)",
                                         "Precipitation_(mm/h)"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) 
str(manualcampo_combined_files)
#Nana
Manualnana_combined_files <- Manualnana_combined_files %>% mutate_at(c("T_Max","T_Min","Relative_Humidity_(%)",
                                                                         "Precipitation_(mm/h)"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) 
str(Manualnana_combined_files)


#Take the average of both temperature columns and remove the old temp columns
library(dplyr)
#Campo
manualcampo_combined_files <- manualcampo_combined_files %>%
  mutate(`Temperature_(C)` = (T_Max + T_Min) / 2)

manualcampo_combined_files <- manualcampo_combined_files %>%
  select(-T_Max, -T_Min)
#Nana
Manualnana_combined_files <- Manualnana_combined_files %>%
  mutate(`Temperature_(C)` = (T_Max + T_Min) / 2)

Manualnana_combined_files <- Manualnana_combined_files %>%
  select(-T_Max, -T_Min)

#Add missing columns
library(dplyr)
#campo
manualcampo_combined_files <- manualcampo_combined_files %>%
  mutate(Wind_Direction_Degrees = NA,
         `Wind_Speed_m/s` = NA,
         Time = NA)
#Nana
Manualnana_combined_files <- Manualnana_combined_files %>%
  mutate(Wind_Direction_Degrees = NA,
         `Wind_Speed_m/s` = NA,
         Time = NA)

#Save in Raw Campo Data
setwd("01_Messy_Data/")
write.csv(Manualnana_combined_files, file = "1manualnana_combined.csv")
#Save in Raw Nana Data
setwd("01_Messy_Data/")
write.csv(manualcampo_combined_files, file = "1manualcampo_combined.csv")
problems(manualcampo_combined_files)