## Raw Data importation 

if(!require("pak")) install.packages("pak")

new_packages <- c("tidyverse", "readr", "ggplot2", "MODIStsp")

library(tidyverse)
library(ggplot2)
library(readr)

#Set Raw Data as my working directory
setwd("~/LSHTM_23/Thesis/Lima_Dengue/")

#Import my data
rain_temp_rh_data <- read.csv("Precipitation.RelativeHumidity.Temperature.csv")
dim(rain_temp_rh_data)
str(rain_temp_rh_data)

raw_incidence <- read.csv("Peru_2000_2022.csv")


raw_demographics <- read.csv("TranslatedGeoPeru-peru_distritos.csv")

head(raw_incidence)


#Translate the Spanish headings to English
#Translate for Climate Data
colnames(rain_temp_rh_data) <- c("ID", "Station", "Date", "Hour", "Longitude", 
"Lattitude", "Altitude", "Temperature", "Humidity", "Precipitation", "Red",
"Department", "Province", "District", "UBIGEO", "Cut_Date" )

#Translate Headings for Incidence Data
colnames(raw_incidence) <- c("Department", "Province", "District", "Local_ID",
                             "Disease", "Year", "Week", "Diagnosis", "Diresa",
                             "Ubigeo", "Local_Code", "Age", "Age_Type", "Sex")
head(raw_incidence)

#Translate Headings for demographic Data
colnames(raw_demographics) <- c("Pop_Occupied", "Pop_1_NBI", "%_Pop_1_NBI", "Pop_Physic_Inadequate",
                                "%_Physical_Inad", "Pop_Overcrowded_Housing", "%_Overcrowding","Pop_No_Hygiene_Service",
                                "%Pop_No_Hygeine", "HH_Children_notatschool", "%_children_notatshcool",
                                "_Pop_High_Econ_Depend", "%_High_Econ_Depend", "Department_Code",
                                "Department", "Province_Code", "Provicne", "District_Code", "District",
                                "Total_Households", "Pop_NBI(hab)", "%Pop_NBI", "Pop_2NBI", "%Pop_2NBI",
                                "Pop_3NBI", "%Pop_3NBI", "Pop_4NBI", "%Pop_4NBI", "c5nbi_abs", "c5nbi_porc",
                                "private dwellings", "Homes_no_Water", "Homes_no_public_Water", "%No_public_water",
                                "Homes_No_Hygiene", "%Homes_No_Hygiene", "Home_No_Light", "%No_Light", "Homes_Dirt_Floor",
                                "%Homes_Dirt_Floor", "Homes_Cook_Lena", "%Homes_Lena", "no_computer",	"phs_pclptb",
                                "no_cellphone",	"phs_tcelu", "Homes_No_Internet",	"phs_inter", "Housing_1_room",	"pv_1hab",
                                "h_cocin",	"ph_cocin",	"sup_tot",	"Sensed Population",	"c5_p2_1",	"c5_p2_2",
                                "gr_quin_1",	"gr_quin_2",	"gr_quin_3",	"gr_quin_4",	"gr_quin_5", "0-14", "15-29",
                                "30-44", "45-64", "65+", "Pop_Health_Insured", "Pop_Not_Insured", "Hard_Speech", "Pop_Bad_Mobility",
                                "Pop_learn_deficit",	"Pop_limit_interactions",	"c5_p10_1",	"c5_p10_4", "He_literate",
                                "Pop_Illiterate","Pop_Working_Age",	"Econ_active_pop", "Employed_pop", "Unemployed_Pop", 
                                "Econ_inactive_pop", "p_sex_h",	"p_sex_m",	"p_ge_0a14",	"p_ge_15a29",	"p_ge_30a44",	"p_ge_45a64",	"p_ge_65ym",	
                                "p_af_sis",	"p_af_ning",	"p_dl_hab",	"p_dl_mov",	"p_dl_ent",	"p_dl_rel",
                                "p_dni",	"p_no_docum",	"p_lees_si",	"p_lees_no",	"p_pet",	"p_pea",	
                                "p_pea_o",	"p_pea_d",	"p_pei",	"pgr_quin1",	"pgr_quin2",	"pgr_quin3",
                                "pgr_quin4",	"pgr_quin5",	"Source"
)

library(dplyr)
#Look at each data set now that the headings are translated
#Temp etc..
dim(rain_temp_rh_data)
str(rain_temp_rh_data)
# I want to see how many diffent station I have in this data set
stations <- unique(rain_temp_rh_data$Station)
stations
departments <- unique(rain_temp_rh_data$Department)
departments
districts <- unique(rain_temp_rh_data$District)
districts
#None of the other districts in this data set are adjacent to Lima so I can restrict the data to only those within the Lima Department

# Demographic Data
dim(raw_demographics)
str(raw_demographics)
# Incidence DAta
dim(raw_incidence)
str(raw_incidence)


#Exclude data that is not within Lima

Messy_Demographics <- filter(raw_demographics, Department == "LIMA")
#Check for duplicates
sum(duplicated(Messy_Demographics)) #There are 0 duplicates


#First filter precip by department, there are two stations within Lima so need to filter by date so filtered by any date above 
# Jan 1 2019 and any date below Jan 1 2020

Messy_Prp_RH_Temp <- filter(rain_temp_rh_data, Department == "LIMA", Date > 20190100, Date < 20230101)
#change the Date column in weather data from an integer to a Date Format
Messy_Prp_RH_Temp$Date <- ymd(Messy_Prp_RH_Temp$Date)
#Check for duplicates
sum(duplicated(Messy_Prp_RH_Temp)) #There are 0 duplicates




# Filter by Department and Year
Messy_incidence <- filter(raw_incidence, Department == "LIMA", Year == 2019:2022)
#Check for duplicates
sum(duplicated(Messy_incidence)) #There are 4 duplicates but there are not unique ID numbers so it could just be someone of the same age and sex got infected in the same week
duplicated(Messy_incidence)

#Check Campo duplicates
sum(duplicated(campo_Del_marta_combined_files))
#Check San Borja duplicates
sum(duplicated(san_borja_combined_files))
#Check Villa Maria duplicates
sum(duplicated(villa_maria_combined_files))

