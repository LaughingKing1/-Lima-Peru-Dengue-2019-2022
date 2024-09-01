## Raw Data importation and stage 2 of cleaning after consolidating weather data script
# In this I finish cleaning demographics data sets and work through a lot of weather but move on to Finishing Weather 
# Script to finish up cleaning the weather data, started a new script for ease of use and so I could shut down R and start working more easily.
#I also clean incidence so it only down to individual listed data

if(!require("pak")) install.packages("pak")

install.packages("padr")

library(padr)
library(tidyverse)
library(ggplot2)
library(readr)
library(hms)
library(dplyr)

#Set Raw Data as my working directory
setwd("~/LSHTM_23/Thesis/Lima_Dengue/")

#Import my data
# Weather Station Data
antonio_data <- read.csv("01_Messy_Data/antonio_raymondi_combined.csv")
campo_data <- read.csv("01_Messy_Data/campo_del_marta_combined.csv")
carbayllo_data <- read.csv("01_Messy_Data/carabaylo_combined.csv")
nana_data <- read.csv("01_Messy_Data/nana_combined.csv")
sanborja_data <- read.csv("01_Messy_Data/san_borja_combined.csv")
sanjuan_data <- read.csv("01_Messy_Data/san_juan_combined.csv")
sanmartin_data <- read.csv("01_Messy_Data/san_martin_combined.csv")
santaanita_data <- read.csv("01_Messy_Data/santa_anita_combined.csv")
villamaria_data <- read.csv("01_Messy_Data/villa_maria_combined.csv")
vonhumboldt_data <- read.csv("01_Messy_Data/von_humboldt_combined.csv")
# Manual Weather Data
ManCamp_data <- read.csv("01_Messy_Data/1manualcampo_combined.csv")
ManNana_data <- read.csv("01_Messy_Data/1manualnana_combined.csv")

# Incidence data
raw_incidence <- read.csv("00_Raw_Data/Peru_2000_2022.csv")
# Demographic data
raw_demographics <- read.csv("00_Raw_Data/TranslatedGeoPeru-peru_distritos.csv")

#############################################################################################
#Translate the Spanish headings to English

#Translate for Climate Data
colnames(antonio_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(campo_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(carbayllo_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(nana_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(sanborja_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(sanjuan_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(sanmartin_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(santaanita_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(villamaria_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")
colnames(vonhumboldt_data) <- c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                            "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                            "Wind_Speed_m/s")

#Translate Headings for Incidence Data
colnames(raw_incidence) <- c("Department", "Province", "District", "Local_ID",
                             "Disease", "Year", "Week", "Diagnosis", "Diresa",
                             "Ubigeo", "Local_Code", "Age", "Age_Type", "Sex")
head(raw_incidence)

#Translate Headings for demographic Data
colnames(raw_demographics) <- c("Pop_Occupied", "Pop_1_NBI", "%_Pop_1_NBI", "Pop_Inadequate_Housing",
                                "%_Inadq_Housing", "Pop_Overcrowded_Housing", "%_Overcrowding","Pop_No_Hygiene_Service",
                                "%_Pop_No_Hygeine", "HH_Children_notatschool", "%_children_notatshcool",
                                "Pop_High_Econ_Depend", "%_High_Econ_Depend", "Department_Code",
                                "Department", "Province_Code", "Province", "District_Code", "District",
                                "Total_Households", "Pop_NBI_hab", "XPop_NBI", "Pop_2NBI", "XPop_2NBI",
                                "Pop_3NBI", "XPop_3NBI", "Pop_4NBI", "XPop_4NBI", "c5nbi_abs", "c5nbi_porc",
                                "private_dwellings", "Number_of_dwellings", "Homes_no_public_Water", "%_Pop_No_pubwater",
                                "Homes_No_Hygiene", "%_Homes_No_Hygiene", "Home_No_Light", "%_No_Light", "Homes_Dirt_Floor",
                                "%_Homes_Dirt_Floor", "Homes_CookFirewood", "%_Homes_Firewood", "no_computer",	"%_no_computer",
                                "no_cellphone",	"%_no_cellphone", "Homes_No_Internet",	"%_no_internet", "Housing_1_room",	
                                "%_Homes_1_room", "h_cocin",	"ph_cocin",	"sup_tot",	"Sensed_Population",	"c5_p2_1",	"c5_p2_2",
                                "gr_quin_1",	"gr_quin_2",	"gr_quin_3",	"gr_quin_4",	"gr_quin_5", "0-14", "15-29",
                                "30-44", "45-64", "65+", "Pop_Health_Insured", "Pop_Not_Insured", "Hard_Speech", "Pop_Bad_Mobility",
                                "Pop_learn_deficit", "Pop_limit_interactions",	"c5_p10_1",	"c5_p10_4", "He_literate",
                                "Pop_Illiterate","Pop_Working_Age",	"Econ_active_pop", "Employed_pop", "Unemployed_Pop", 
                                "Econ_inactive_pop", "Percent_female",	"Percent_male",	"Percent_0-14",	"Percent_15-29",	"Percent_30-44", "Percent_45-64",
                                "Percent_65+", "p_af_sis",	"p_af_ning",	"p_dl_hab",	"%_Difficulty_Moving",	"p_dl_ent", "%_limited_interact_wothes",
                                "p_dni",	"p_no_docum",	"p_lees_si",	"Illiteracy_Rate",	"p_pet",	"p_pea",	
                                "p_pea_o",	"p_pea_d",	"p_pei",	"pgr_quin1",	"pgr_quin2",	"pgr_quin3",
                                "pgr_quin4",	"pgr_quin5",	"Source"
)

#############################################################################################
# Fix data types in Weather Data

# Fix the data types for each column of weather data changing them from characters 
# to their respective data types
# Change Date to Posixct, time to hms and the rest to numeric 

# Anotonio
antonio_data <- antonio_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                             "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                             "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(antonio_data)

#Campo
campo_data <- campo_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                             "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                             "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(campo_data)
#Carbayllo
carbayllo_data <- carbayllo_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                             "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                             "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(carbayllo_data)
# Nana
nana_data <- nana_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                             "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                             "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(nana_data)
#San Borja
sanborja_data <- sanborja_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                             "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                             "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(sanborja_data)
#San Juan
sanjuan_data <- sanjuan_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                             "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                             "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(sanjuan_data)
# San Martin
sanmartin_data <- sanmartin_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                             "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                             "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(sanmartin_data)
#Santa Anita
santaanita_data <- santaanita_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                         "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                         "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(santaanita_data)
#Villa Maria
villamaria_data <- villamaria_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                                   "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                                   "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(villamaria_data)
#Von Humboldt
vonhumboldt_data <- vonhumboldt_data %>% mutate_at(c("Temperature_(C)", "Precipitation_(mm/h)", 
                                                   "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                                   "Wind_Speed_m/s"), as.numeric) %>%
  mutate_at (c("Date"), tz = "GMT-5", as.Date.POSIXct) %>%
  mutate(Time = hms::as_hms(sprintf('%s:00', Time)))
str(vonhumboldt_data)


###############################################################################################################
# Initial Inspections and reducing data to Lima Province within our time frame

#Look at each data set now that the headings are translated
#Temp etc..
dim(antonio_data)
str(antonio_data)

# Demographic Data
dim(raw_demographics)
str(raw_demographics)
# Incidence DAta
dim(raw_incidence)
str(raw_incidence)


# Demographic, Exclude data that is not within Lima
Messy_Demographics <- filter(raw_demographics, Province == "LIMA")
# Split apart district and just total population
district_pop <- data.frame(Messy_Demographics$District, Messy_Demographics$Pop_Occupied)
# save this
write.csv(district_pop, "PopulationDistricts.csv")

# Incidence, Filter by province and Year
Messy_incidence <- raw_incidence %>%
  filter(Province == "LIMA" & Year >= 2019 & Year <= 2022)

###############################################################################################################
#Check for duplicates

#Check weather data for duplicates
sum(duplicated(antonio_data)) 
sum(duplicated(campo_data)) 
sum(duplicated(carbayllo_data))
sum(duplicated(nana_data))
sum(duplicated(sanborja_data))
sum(duplicated(sanjuan_data))
sum(duplicated(sanmartin_data))
sum(duplicated(santaanita_data))
sum(duplicated(villamaria_data))
sum(duplicated(vonhumboldt_data))
#There are no duplicates in the weather data

# Demographic Data
sum(duplicated(Messy_Demographics)) #There are 0 duplicates

# Incidence Data
sum(duplicated(Messy_incidence)) #There are 4 duplicates but there are not unique ID numbers so it could just be someone of the same age and sex got infected in the same week
duplicated(Messy_incidence) #Check the individual duplicates (above)

###############################################################################################################
# Demographics and Incidence remove unwanted columns

Messy_incidence %>% distinct(Age_Type) #Check if there are any under 1's
Messy_incidence$Age_Type

# Remove from Incidence
Messy_incidence <- Messy_incidence  %>% dplyr::select(-c(Local_Code, Local_ID, Diresa, Ubigeo))

# Remove from Demographics
Messy_Demographics <- Messy_Demographics %>% dplyr::select(-c(
  Pop_Occupied, Pop_1_NBI, Pop_Inadequate_Housing,
  Pop_Overcrowded_Housing,Pop_No_Hygiene_Service,
  HH_Children_notatschool, `%_children_notatshcool`,
  Pop_High_Econ_Depend, Department_Code, Province_Code, District_Code,
  Total_Households, Pop_NBI_hab, XPop_NBI, Pop_2NBI, XPop_2NBI,
  Pop_3NBI, XPop_3NBI, Pop_4NBI, XPop_4NBI, c5nbi_abs, c5nbi_porc,
  private_dwellings, Number_of_dwellings, Homes_no_public_Water,
  Homes_No_Hygiene, `%_Homes_No_Hygiene`, Home_No_Light, Homes_Dirt_Floor,
  Homes_CookFirewood, no_computer,
  no_cellphone, Homes_No_Internet, Housing_1_room,
  h_cocin,	ph_cocin,	sup_tot,	Sensed_Population,	c5_p2_1,	c5_p2_2,
  gr_quin_1,	gr_quin_2,	gr_quin_3,	gr_quin_4,	gr_quin_5, `0-14`, `15-29`,
  `30-44`, `45-64`, `65+`, Pop_Health_Insured, Pop_Not_Insured, Hard_Speech, Pop_Bad_Mobility,
  Pop_learn_deficit,	Pop_limit_interactions,	c5_p10_1,	c5_p10_4, He_literate,
  Pop_Illiterate,Pop_Working_Age,	Econ_active_pop, Employed_pop, Unemployed_Pop, 
  Econ_inactive_pop, Percent_female,	Percent_male,	`Percent_0-14`,	`Percent_15-29`,	`Percent_30-44`, `Percent_45-64`,
  `Percent_65+`, p_af_sis,	p_af_ning,	p_dl_hab,	p_dl_ent,
  p_dni,	p_no_docum,	p_lees_si,	p_pet,	p_pea,	
  p_pea_o,	p_pea_d,	p_pei,	pgr_quin1,	pgr_quin2,	pgr_quin3,
  pgr_quin4,	pgr_quin5,	Source
  
))

###############################################################################################################
# Change Demographic data from numeric values to percentages

numeric_cols <- colnames(Messy_Demographics)[sapply(Messy_Demographics, is.numeric)]
Messy_Demographics[numeric_cols] <- Messy_Demographics[numeric_cols] / 100
Messy_Demographics <- Messy_Demographics %>% 
  relocate(where(is.character), .before = 1)

setwd("02_Cleaned_Data/")
write.csv(Messy_Demographics, file = "Cleaned_Demographics.csv")

###############################################################################################################
#Checking precipitation data

str(Messy_Demographics)
sum(antonio_data$`Precipitation_(mm/h)`, na.rm = TRUE)
sum(campo_data$`Precipitation_(mm/h)`, na.rm = TRUE)
sum(carbayllo_data$`Precipitation_(mm/h)`, na.rm = TRUE)
sum(nana_data$`Precipitation_(mm/h)`, na.rm = TRUE)
nana_data %>% distinct(`Precipitation_(mm/h)`)
sum(sanborja_data$`Precipitation_(mm/h)`, na.rm = TRUE) #very high
#Check data
sanborja_data %>% distinct(`Precipitation_(mm/h)`)
sum(sanjuan_data$`Precipitation_(mm/h)`, na.rm = TRUE)
sum(sanmartin_data$`Precipitation_(mm/h)`, na.rm = TRUE) #very high
#Check data
sanmartin_data %>% distinct(`Precipitation_(mm/h)`)
sum(santaanita_data$`Precipitation_(mm/h)`, na.rm = TRUE) #very high
#Check data
santaanita_data %>% distinct(`Precipitation_(mm/h)`)
sum(villamaria_data$`Precipitation_(mm/h)`, na.rm = TRUE) #very high
#Check data
villamaria_data %>% distinct(`Precipitation_(mm/h)`)
sum(vonhumboldt_data$`Precipitation_(mm/h)`, na.rm = TRUE)
################################################################################################3
# Adding in manual data to Campo and Nana 

#Campo
#First create unique IDs for the datasets,
ManCamp_data['new_id'] = ManCamp_data['X'] + 100000
#Then remove old Column and rename new columns plus match campo data column names
ManCamp_data <- select(ManCamp_data, -X)
ManCamp_data <- ManCamp_data %>% 
  dplyr::rename(ID = new_id, `Temperature_(C)` = Temperature_.C.,
                `Precipitation_(mm/h)` = Precipitation_.mm.h.,
                `Relative_Humidity_(%)` = Relative_Humidity_...,
                `Wind_Speed_m/s` = Wind_Speed_m.s)


# Now, the column names in ManCamp_data should match those in campo_data
print(colnames(ManCamp_data))
#Merge Data
campo_data <- merge(campo_data, ManCamp_data, by = c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                                                      "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                                      "Wind_Speed_m/s"), all = TRUE)
#Nana
# Adding in manual data 
#First create unique IDs for the datasets,
ManNana_data['new_id'] = ManNana_data['X'] + 100000
#Then remove old Column and rename new columns plus match nana data column names
ManNana_data <- select(ManNana_data, -X)
ManNana_data <- ManNana_data %>% 
  dplyr::rename(ID = new_id, `Temperature_(C)` = Temperature_.C.,
                `Precipitation_(mm/h)` = Precipitation_.mm.h.,
                `Relative_Humidity_(%)` = Relative_Humidity_...,
                `Wind_Speed_m/s` = Wind_Speed_m.s)


# Now, the column names in ManCamp_data should match those in campo_data
print(colnames(ManCamp_data))
#Merge Data
nana_data <- merge(nana_data, ManNana_data, by = c("ID", "Date", "Time", "Temperature_(C)", "Precipitation_(mm/h)", 
                                                     "Relative_Humidity_(%)", "Wind_Direction_Degrees", 
                                                     "Wind_Speed_m/s"), all = TRUE)


################################################################################################
# fix any data errors in weather data before consolidation

#San Borja has some 0's and 1's in wind data that need to be changed to NA, there were several data sets where these were recorded for a long period of time inconsistant with the rest of the data so I'm treating it like a broken instrument and giving a NA value if there is a pare of 0 and 1 for the wind data columns.
sanborja_data <- sanborja_data %>%
  mutate(Wind_Direction_Degrees = ifelse(Wind_Direction_Degrees == 1 | `Wind_Speed_m/s` == 0, NA, Wind_Direction_Degrees),
         `Wind_Speed_m/s` = ifelse(Wind_Direction_Degrees == 1 |
                                     `Wind_Speed_m/s` == 0, NA, `Wind_Speed_m/s`))

# Same as above needs to be done for Santa Anita
santaanita_data <- santaanita_data %>%
  mutate(Wind_Direction_Degrees = ifelse(Wind_Direction_Degrees == 1 | `Wind_Speed_m/s` == 0, NA, Wind_Direction_Degrees),
         `Wind_Speed_m/s` = ifelse(Wind_Direction_Degrees == 1 |
                                     `Wind_Speed_m/s` == 0, NA, `Wind_Speed_m/s`))
##################################################################################################################
#Finish up cleaning Incidence Data

#Change all children under 1 year old to 1 (there are only 3) by using the month symbol "M" in Age Type to mutate them 
Messy_incidence <- mutate(Messy_incidence, Age = ifelse(Age_Type == "M", 1, Age))
Messy_incidence <- mutate(Messy_incidence, Age = ifelse(Age_Type == "D", 1, Age))


#Remove age_type column from messy incidence
Messy_incidence <- Messy_incidence  %>% dplyr::select(-c(Age_Type))

#Set Epi week for each year in my data set, had to break down each year seperatly by filtering for the desired year then choosing the last day of the first week of each epidemiological calendar year
#For 2019
Messy_incidence$Date <- paste(Messy_incidence$Year, Messy_incidence$Week, sep = "-") # Format: YYYY-WW
Messy_incidence_19 <- Messy_incidence %>%
  filter(Year %in% 2019) %>% # Filter rows for 2019
  mutate(Date = as.Date(paste0(Year, "-01-05")) + 7 * (Week - 1))
#For 2020
Messy_incidence_20 <- Messy_incidence %>%
  filter(Year %in% 2020) %>% # Filter rows for the years 2019 to 2022
  mutate(Date = as.Date(paste0(Year, "-01-04")) + 7 * (Week - 1))
#For 2021
Messy_incidence_21 <- Messy_incidence %>%
  filter(Year %in% 2021) %>% # Filter rows for the years 2019 to 2022
  mutate(Date = as.Date(paste0(Year, "-01-09")) + 7 * (Week - 1))
#For 2022
Messy_incidence_22 <- Messy_incidence %>%
  filter(Year %in% 2022) %>% # Filter rows for the years 2019 to 2022
  mutate(Date = as.Date(paste0(Year, "-01-08")) + 7 * (Week - 1))

# I have each year broken into seperate tables above so they need to be merged back into a single table
Incidence_List <- list(Messy_incidence_19, Messy_incidence_20, Messy_incidence_21, Messy_incidence_22)
Merged_Messy_incidence <- Reduce(function(x, y) merge(x, y, all=TRUE), Incidence_List)

#Remove the year and week column
Merged_Messy_incidence <- Merged_Messy_incidence %>% dplyr::select(-c(Year, Week, Diagnosis))
# Set the Date column as a date type of data
Merged_Messy_incidence$Date <- as.Date(Merged_Messy_incidence$Date, 
                                       format = "%Y-%m-%d", tz = "GMT-5")
# Check data types
str(Merged_Messy_incidence)
#Updating the Disease column into binary Dengue Without Alarm Signs = 0, Dengue With Alarm Signs = 1
Merged_Messy_incidence <- Merged_Messy_incidence %>%
  mutate(Disease = case_when(
    Disease == "DENGUE SIN SEÑALES DE ALARMA" ~ "0",
    Disease == "DENGUE CON SEÑALES DE ALARMA" ~ "1",
    Disease == "DENGUE GRAVE" ~ "2",
    TRUE ~ Disease
  ))%>%
  mutate_at(c("Disease"), as.numeric)


#Create binary variables for sex 1 = Male, 2 = Female
Merged_Messy_incidence <- Merged_Messy_incidence %>%
  mutate(Sex = case_when(
    Sex == "M" ~ "1",
    Sex == "F" ~ "2",
    TRUE ~ Sex
  )) %>%
  mutate_at(c("Sex"), as.numeric)

setwd("~/LSHTM_23/Thesis/Lima_Dengue/01_Messy_Data")
#Save Cleaned Data
write.csv(Merged_Messy_incidence, file = "Individual_Incidence.csv")
##################################################################################################################
# Start to consolidate weather data by first finding the average of each data point per week and month.

#Need to drop down and run the vonhumnoldt monthly data before running this script to fill in the dates for the dataframes
# Manually creating a dataframe that has all the date I need
all_combinations <- crossing(seq(min(vonhumboldt_month_av$year), max(vonhumboldt_month_av$year)), 1:12)
colnames(all_combinations) <- c("year", "month")


# Antonio Data
#Start by aggregating by week
Antonio_wkly_av <- antonio_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
                     `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
                     `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
                     Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
                     `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
                     mutate(Date = paste0(year, "-", week))

#Change Date Column into a date format
Antonio_wkly_av <- Antonio_wkly_av %>%
  mutate(Date = case_when(
    year >= 2023 ~ as.Date(paste0(year, "-01-07")) + 7 * (week - 1), # Then for 2023 and later
    year >= 2022 ~ as.Date(paste0(year, "-01-01")) + 7 * (week - 1), # Next specific condition for 2022 and later
    year >= 2021 ~ as.Date(paste0(year, "-01-02")) + 7 * (week - 1), # Specific condition for 2021 and later
    year >= 2020 ~ as.Date(paste0(year, "-01-04")) + 7 * (week - 1), # Finally, for 2020 and later, excluding 2019
    TRUE ~ as.Date(paste0(year, "-01-05")) + 7 * (week - 1) # Default behavior for earlier years
  ))

# Then aggregate by month
Antonio_month_av <- antonio_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
Antonio_month_av$Date <- as.Date(paste(Antonio_month_av$Date, "-01", sep=""), "%Y-%m-%d")

# Left join original data to fill in missing months
Antonio_month_av <- left_join(all_combinations, Antonio_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))

write.csv(Antonio_month_av, "antonio_monthly.csv")

# Campo Data
#Start by aggregating by week
Campo_wkly_av <- campo_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
Campo_month_av <- campo_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
Campo_month_av$Date <- as.Date(paste(Campo_month_av$Date, "-01", sep=""), "%Y-%m-%d")

write.csv(Campo_month_av, "campo_monthly.csv")

# Carbayllo
#Start by aggregating by week
Carbayllo_wkly_av <- carbayllo_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
Carbayllo_month_av <- carbayllo_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
Carbayllo_month_av$Date <- as.Date(paste(Carbayllo_month_av$Date, "-01", sep=""), "%Y-%m-%d")

# Left join original data to fill in missing months
Carbayllo_month_av <- left_join(all_combinations, Carbayllo_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))

write.csv(Carbayllo_month_av, "carbayllo_monthly.csv")

# Nana
#Start by aggregating by week
Nana_wkly_av <- nana_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
nana_month_av <- nana_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
nana_month_av$Date <- as.Date(paste(nana_month_av$Date, "-01", sep=""), "%Y-%m-%d")

# Left join original data to fill in missing months
nana_month_av <- left_join(all_combinations, nana_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))

write.csv(nana_month_av, "nana_monthly.csv")

# San Borja
#Start by aggregating by week
sanborja_wkly_av <- sanborja_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
sanborja_month_av <- sanborja_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
sanborja_month_av$Date <- as.Date(paste(sanborja_month_av$Date, "-01", sep=""), "%Y-%m-%d")

# Left join original data to fill in missing months
sanborja_month_av <- left_join(all_combinations, sanborja_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))

write.csv(sanborja_month_av, "sanborja_monthly.csv")

# San Juan DE LURIGANCHO
#Start by aggregating by week
sanjuan_wkly_av <- sanjuan_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
sanjuan_month_av <- sanjuan_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
sanjuan_month_av$Date <- as.Date(paste(sanjuan_month_av$Date, "-01", sep=""), "%Y-%m-%d")

# Left join original data to fill in missing months
sanjuan_month_av <- left_join(all_combinations, sanjuan_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))

write.csv(sanjuan_month_av, "sanjuan_monthly.csv")

# San Martin DE PORRES
#Start by aggregating by week
sanmartin_wkly_av <- sanmartin_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
sanmartin_month_av <- sanmartin_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
sanmartin_month_av$Date <- as.Date(paste(sanmartin_month_av$Date, "-01", sep=""), "%Y-%m-%d")

# Left join original data to fill in missing months
sanmartin_month_av <- left_join(all_combinations, sanmartin_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))

write.csv(sanmartin_month_av, "sanmartin_monthly.csv")

# Santa Anita
#Start by aggregating by week
santaanita_wkly_av <- santaanita_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
santaanita_month_av <- santaanita_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
santaanita_month_av$Date <- as.Date(paste(santaanita_month_av$Date, "-01", sep=""), "%Y-%m-%d")

# Left join original data to fill in missing months
santaanita_month_av <- left_join(all_combinations, santaanita_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))

write.csv(santaanita_month_av, "santaanita_monthly.csv")


# Villa Maria
#Start by aggregating by week
villamaria_wkly_av <- villamaria_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
villamaria_month_av <- villamaria_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
villamaria_month_av$Date <- as.Date(paste(villamaria_month_av$Date, "-01", sep=""), "%Y-%m-%d")

# Left join original data to fill in missing months
villamaria_month_av <- left_join(all_combinations, villamaria_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))

write.csv(villamaria_month_av, "villa_maria_monthly.csv")

# VON HUMBOLDT
#Start by aggregating by week
vonhumboldt_wkly_av <- vonhumboldt_data %>%
  dplyr::mutate(year = year(Date),
         week = week(Date)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(`Precipitation_(mm/h)`= sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", week))

# Then aggregate by month
vonhumboldt_month_av <- vonhumboldt_data %>%
  dplyr::mutate(year = year(Date), month = month(Date)) %>% # Extract the month from the Date column
  dplyr::group_by(year, month) %>% # Group by year and month
  dplyr::summarise(`Precipitation_(mm/h)` = sum(`Precipitation_(mm/h)`, na.rm = TRUE),
            `Temperature_(C)` = mean(`Temperature_(C)`, na.rm = TRUE),
            `Wind_Speed_m/s` = mean(`Wind_Speed_m/s`, na.rm = TRUE),
            Wind_Direction_Degrees = mean(Wind_Direction_Degrees, na.rm = TRUE),
            `Relative_Humidity_(%)` = mean(`Relative_Humidity_(%)`, na.rm = TRUE)) %>%
  mutate(Date = paste0(year, "-", month)) # Create a unique identifier for each month

#Change Date Column into a date format
vonhumboldt_month_av$Date <- as.Date(paste(vonhumboldt_month_av$Date, "-01", sep=""), "%Y-%m-%d")


# Left join original data to fill in missing months
vonhumboldt_month_av <- left_join(all_combinations, vonhumboldt_month_av, by = c("year", "month")) %>%
  replace_na(list(value = NA))


write.csv(vonhumboldt_month_av, "von_humboldt_mothly.csv")

# Move over to Finishing Weather Script to continue cleaning the weather datasets that I
# just finished writing and saving into messy data up above. 


