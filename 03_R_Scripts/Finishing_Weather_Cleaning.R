# Starting a new script to simplify the data cleaning into stage 3
# Follow this script after consolidating weather data then cleaning nonspatial data
if(!require("pak")) install.packages("pak")

new_packages <- c("tidyverse", "readr", "ggplot2", "MODIStsp")
library(padr)
library(tidyverse)
library(ggplot2)
library(readr)
library(hms)
library(dplyr)
library(lubridate)


#Set Raw Data as my working directory
setwd("~/LSHTM_23/Thesis/Lima_Dengue/")

#Import my monthly data
# Weather Station Data
antonio_monthly <- read.csv("01_Messy_Data/antonio_monthly.csv")
campo_monthly <- read.csv("01_Messy_Data/campo_monthly.csv")
carbayllo_monthly <- read.csv("01_Messy_Data/carbayllo_monthly.csv")
nana_monthly <- read.csv("01_Messy_Data/nana_monthly.csv")
sanborja_monthly <- read.csv("01_Messy_Data/sanborja_monthly.csv")
sanjuan_monthly <- read.csv("01_Messy_Data/sanjuan_monthly.csv")
sanmartin_monthly <- read.csv("01_Messy_Data/sanmartin_monthly.csv")
santaanita_monthly <- read.csv("01_Messy_Data/santaanita_monthly.csv")
villamaria_monthly <- read.csv("01_Messy_Data/villa_maria_monthly.csv")
vonhumboldt_monthly <- read.csv("01_Messy_Data/von_humboldt_mothly.csv")

# Replace all 0's with NA's for the two data sets that had no data recordings on precipitation data. 
carbayllo_monthly$Precipitation_.mm.h.[carbayllo_monthly$Precipitation_.mm.h. == 0] <- NA
sanjuan_monthly$Precipitation_.mm.h.[sanjuan_monthly$Precipitation_.mm.h. == 0] <- NA


# Filter out 2023

antonio_monthly <- antonio_monthly %>%
  filter(year %in% 2019:2022)
campo_monthly <- campo_monthly %>%
  filter(year %in% 2019:2022)
carbayllo_monthly <- carbayllo_monthly %>%
  filter(year %in% 2019:2022)
nana_monthly <- nana_monthly %>%
  filter(year %in% 2019:2022)
sanborja_monthly <- sanborja_monthly %>%
  filter(year %in% 2019:2022)
sanjuan_monthly <- sanjuan_monthly %>%
  filter(year %in% 2019:2022)
sanmartin_monthly <- sanmartin_monthly %>%
  filter(year %in% 2019:2022)
santaanita_monthly <- santaanita_monthly %>%
  filter(year %in% 2019:2022)
villamaria_monthly <- villamaria_monthly %>%
  filter(year %in% 2019:2022)
vonhumboldt_monthly <- vonhumboldt_monthly %>%
  filter(year %in% 2019:2022)
###################################################################################
#Replace outlier data with NA's

# San Borja Data 
# use the IQR method to identify outliers in the data and confirmed with visual inspection 
Q1 <- quantile(sanborja_monthly$Precipitation_.mm.h., 0.25, na.rm = TRUE)
Q3 <- quantile(sanborja_monthly$Precipitation_.mm.h., 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
# I want to include more data than less so set SD to 3
lower_bound <- Q1 - 3 * IQR
upper_bound <- Q3 + 3 * IQR

outliers <- sanborja_monthly$Precipitation_.mm.h. < lower_bound | sanborja_monthly$Precipitation_.mm.h. > upper_bound
print(outliers)
# Replace outliers with a NA
sanborja_monthly$Precipitation_.mm.h.[outliers] <- NA

# San Martin Data
Q1 <- quantile(sanmartin_monthly$Precipitation_.mm.h., 0.25, na.rm = TRUE)
Q3 <- quantile(sanmartin_monthly$Precipitation_.mm.h., 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 3 * IQR
upper_bound <- Q3 + 3 * IQR

outliers <- sanmartin_monthly$Precipitation_.mm.h. < lower_bound | sanmartin_monthly$Precipitation_.mm.h. > upper_bound
print(outliers)
# Replace outliers with a NA
sanmartin_monthly$Precipitation_.mm.h.[outliers] <- NA

# Santa Anita Data
Q1 <- quantile(santaanita_monthly$Precipitation_.mm.h., 0.25, na.rm = TRUE)
Q3 <- quantile(santaanita_monthly$Precipitation_.mm.h., 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
#Visual inspection for this data set really only put 1 data point out of reality but the quintile range was very low because there were a lot of 0's in this data set so I increased the upper and lower bound limits to only exclude the value that was massively high
lower_bound <- Q1 - 6 * IQR
upper_bound <- Q3 + 6 * IQR

outliers <- santaanita_monthly$Precipitation_.mm.h. < lower_bound | santaanita_monthly$Precipitation_.mm.h. > upper_bound
print(outliers)
# Replace outliers with a NA
santaanita_monthly$Precipitation_.mm.h.[outliers] <- NA

# Villa Maria Data
Q1 <- quantile(villamaria_monthly$Precipitation_.mm.h., 0.25, na.rm = TRUE)
Q3 <- quantile(villamaria_monthly$Precipitation_.mm.h., 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 3 * IQR
upper_bound <- Q3 + 3 * IQR

outliers <- villamaria_monthly$Precipitation_.mm.h. < lower_bound | villamaria_monthly$Precipitation_.mm.h. > upper_bound
print(outliers)
# Replace outliers with a NA
villamaria_monthly$Precipitation_.mm.h.[outliers] <- NA
#########################################################################################################

# Combine all my data into one frame
combined_df <- do.call(rbind, list(antonio_monthly, campo_monthly, carbayllo_monthly,
                                   nana_monthly, sanborja_monthly, sanjuan_monthly, 
                                   sanmartin_monthly, santaanita_monthly, 
                                   villamaria_monthly, vonhumboldt_monthly))
# Rename columns to get rid of weird name scheme
colnames(combined_df) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                             "Date")
# Remove Date and ID Columns
combined_df <- combined_df %>%
  select(-Date, -ID)
#Give each row a unique ID
combined_df$UniqueID <- seq_len(nrow(combined_df))

#Average all the climate data for each variable for each month

average_df <- combined_df %>% 
  group_by(Year, Month) %>% 
  summarise(across(c(Precipitation, Temperature, Wind_Speed, Wind_Direction_Degrees, Relative_Humidity), ~mean(., na.rm = TRUE)), .groups = "keep")

# First Standardize column names again
colnames(antonio_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(campo_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(carbayllo_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(nana_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(sanborja_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(sanjuan_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(sanmartin_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(santaanita_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(villamaria_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")
colnames(vonhumboldt_monthly) <- c("ID", "Year", "Month", "Precipitation", "Temperature",
                           "Wind_Speed", "Wind_Direction_Degrees","Relative_Humidity",
                           "Date")

#########################################################################################################
# Add the average values to replace the missing data (NA's) 
# First add an ID column to my average data frame
average_df$ID <- seq.int(1,48)
#Specify the columns that have NA values and need to be updated
columns_to_update <- c("Precipitation", "Temperature",
                       "Wind_Speed", "Wind_Direction_Degrees", "Relative_Humidity")
# Start replacing NA's
# Antonio Raymondi
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(antonio_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of antonio_monthly
    na_index <- is.na(antonio_monthly[, col])
    
    # Replace NA values in antonio_monthly with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    antonio_monthly[na_index & rownames(antonio_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(antonio_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}


# Campo De Marte
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(campo_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of Campo De Marte
    na_index <- is.na(campo_monthly[, col])
    
    # Replace NA values in Campo De Marte with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    campo_monthly[na_index & rownames(campo_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(campo_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}

# Carbayllo
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(carbayllo_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of Carbayllo
    na_index <- is.na(carbayllo_monthly[, col])
    
    # Replace NA values in Carbayllo with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    carbayllo_monthly[na_index & rownames(carbayllo_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(carbayllo_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}

# Nana
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(nana_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of Nana
    na_index <- is.na(nana_monthly[, col])
    
    # Replace NA values in Nana with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    nana_monthly[na_index & rownames(nana_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(nana_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}

# San Borja
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(sanborja_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of San Borja
    na_index <- is.na(sanborja_monthly[, col])
    
    # Replace NA values in San Borja with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    sanborja_monthly[na_index & rownames(sanborja_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(sanborja_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}

# San Juan
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(sanjuan_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of San Juan
    na_index <- is.na(sanjuan_monthly[, col])
    
    # Replace NA values in San Juan with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    sanjuan_monthly[na_index & rownames(sanjuan_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(sanjuan_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}

# San Martin
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(sanmartin_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of San Martin
    na_index <- is.na(sanmartin_monthly[, col])
    
    # Replace NA values in San Martin with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    sanmartin_monthly[na_index & rownames(sanmartin_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(sanmartin_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}

# Santa Anita
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(santaanita_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of Santa Anita
    na_index <- is.na(santaanita_monthly[, col])
    
    # Replace NA values in Santa Anita with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    santaanita_monthly[na_index & rownames(santaanita_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(santaanita_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}

# Villa Maria
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(villamaria_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of Villa Maria
    na_index <- is.na(villamaria_monthly[, col])
    
    # Replace NA values in Villa Maria with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    villamaria_monthly[na_index & rownames(villamaria_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(villamaria_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}

# Von Humboldt
for(col in columns_to_update) {
  # Ensure the column exists in both data frames
  if(col %in% colnames(vonhumboldt_monthly) && col %in% colnames(average_df)) {
    # Create a logical index for NA values in the current column of Von Humboldt
    na_index <- is.na(vonhumboldt_monthly[, col])
    
    # Replace NA values in Von Humboldt with corresponding values from average_df
    # but only for rows that have matching row names in average_df
    vonhumboldt_monthly[na_index & rownames(vonhumboldt_monthly) %in% rownames(average_df), col] <- 
      average_df[match(rownames(vonhumboldt_monthly)[na_index], rownames(average_df)), col]
  } else {
    warning(paste("Column", col, "not found in both data frames. Skipping..."))
  }
}
###################################################################################
# Fill in the Missing time stamps
antonio_monthly$Date <- ymd(paste(antonio_monthly$Year, sprintf("%02d", antonio_monthly$Month), "01", sep="-"))
campo_monthly$Date <- ymd(paste(campo_monthly$Year, sprintf("%02d", campo_monthly$Month), "01", sep="-"))
carbayllo_monthly$Date <- ymd(paste(carbayllo_monthly$Year, sprintf("%02d", carbayllo_monthly$Month), "01", sep="-"))
nana_monthly$Date <- ymd(paste(nana_monthly$Year, sprintf("%02d", nana_monthly$Month), "01", sep="-"))
sanborja_monthly$Date <- ymd(paste(sanborja_monthly$Year, sprintf("%02d", sanborja_monthly$Month), "01", sep="-"))
sanjuan_monthly$Date <- ymd(paste(sanjuan_monthly$Year, sprintf("%02d", sanjuan_monthly$Month), "01", sep="-"))
sanmartin_monthly$Date <- ymd(paste(sanmartin_monthly$Year, sprintf("%02d", sanmartin_monthly$Month), "01", sep="-"))
santaanita_monthly$Date <- ymd(paste(santaanita_monthly$Year, sprintf("%02d", santaanita_monthly$Month), "01", sep="-"))
villamaria_monthly$Date <- ymd(paste(villamaria_monthly$Year, sprintf("%02d", villamaria_monthly$Month), "01", sep="-"))
vonhumboldt_monthly$Date <- ymd(paste(vonhumboldt_monthly$Year, sprintf("%02d", vonhumboldt_monthly$Month), "01", sep="-"))


# Remove Year and Month Column and Give each row an unique ID
antonio_monthly <- antonio_monthly %>%
  select(-Date)
campo_monthly <- campo_monthly %>%
  select(-Date)
carbayllo_monthly <- carbayllo_monthly %>%
  select(-Date)
nana_monthly <- nana_monthly %>%
  select(-Date)
sanborja_monthly <- sanborja_monthly %>%
  select(-Date)
sanjuan_monthly <- sanjuan_monthly %>%
  select(-Date)
sanmartin_monthly <- sanmartin_monthly %>%
  select(-Date)
santaanita_monthly <- santaanita_monthly %>%
  select(-Date)
villamaria_monthly <- villamaria_monthly %>%
  select(-Date)
vonhumboldt_monthly <- vonhumboldt_monthly %>%
  select(-Date)
#Unique codes in sequence of 48
campo_monthly$ID <- seq.int(49,96)
carbayllo_monthly$ID <- seq.int(97,144)
nana_monthly$ID <- seq.int(145,192)
sanborja_monthly$ID <- seq.int(193,240)
sanjuan_monthly$ID <- seq.int(241,288)
sanmartin_monthly$ID <- seq.int(289,336)
santaanita_monthly$ID <- seq.int(337,384)
villamaria_monthly$ID <- seq.int(385, 432)
vonhumboldt_monthly$ID <- seq.int(433,480)

##########################################################################################

# Add Province and District Columns for Each data set
# Antonio Station is in Ancon District
antonio_monthly <- antonio_monthly %>%
  mutate(District = "ANCON")
# Campo Station is in Jesus Maria district
campo_monthly <- campo_monthly %>%
  mutate(District = "JESUS MARIA")
# Carbayllo Station is in Carabayllo district (Finally spelled correctly lol)
carbayllo_monthly <- carbayllo_monthly %>%
  mutate(District = "CARABAYLLO")
# Nana Station is in Lurigancho district 
nana_monthly <- nana_monthly %>%
  mutate(District = "LURIGANCHO")
# San Borja Station is in San Borja district 
sanborja_monthly <- sanborja_monthly %>%
  mutate(District = "SAN BORJA")
# San Juan Station is in SAN JUAN DE LURIGANCHO district 
sanjuan_monthly <- sanjuan_monthly %>%
  mutate(District = "SAN JUAN DE LURIGANCHO")
# San Martin Station is in SAN MARTIN DE PORRES district 
sanmartin_monthly <- sanmartin_monthly %>%
  mutate(District = "SAN MARTIN DE PORRES")
# Santa Anita Station is in Santa Anita district 
santaanita_monthly <- santaanita_monthly %>%
  mutate(District = "SANTA ANITA")
# Villa Maria Station is in VILLA MARIA DEL TRIUNFO district 
villamaria_monthly <- villamaria_monthly %>%
  mutate(District = "VILLA MARIA DEL TRIUNFO")
# Von Humboldt Station is in La Molina district 
vonhumboldt_monthly <- vonhumboldt_monthly %>%
  mutate(District = "LA MOLINA")

#Change month from an intiger to the name of the month
antonio_monthly$Month <- month.name[antonio_monthly$Month]
campo_monthly$Month <- month.name[campo_monthly$Month]
carbayllo_monthly$Month <- month.name[carbayllo_monthly$Month]
nana_monthly$Month <- month.name[nana_monthly$Month]
sanborja_monthly$Month <- month.name[sanborja_monthly$Month]
sanjuan_monthly$Month <- month.name[sanjuan_monthly$Month]
sanmartin_monthly$Month <- month.name[sanmartin_monthly$Month]
santaanita_monthly$Month <- month.name[santaanita_monthly$Month]
villamaria_monthly$Month <- month.name[villamaria_monthly$Month]
vonhumboldt_monthly$Month <- month.name[vonhumboldt_monthly$Month]


##########################################################################################

#Write cleaned data .csv
setwd("~/LSHTM_23/Thesis/Lima_Dengue/02_Cleaned_Data")
# Save as disctrict name instead of weather station name
write.csv(antonio_monthly, "ancon_climate_clean.csv", row.names = FALSE)
write.csv(campo_monthly, "JesusMaria_climate_clean.csv", row.names = FALSE)
write.csv(carbayllo_monthly, "Carabayllo_climate_clean.csv", row.names = FALSE)
write.csv(nana_monthly, "Lurigancho_Chosica_climate_clean.csv", row.names = FALSE)
write.csv(sanborja_monthly, "SanBorja_climate_clean.csv", row.names = FALSE)
write.csv(sanjuan_monthly, "SanJuandeLurigancho_climate_clean.csv", row.names = FALSE)
write.csv(sanmartin_monthly, "SanMartinDePorres_climate_clean.csv", row.names = FALSE)
write.csv(santaanita_monthly, "SantaAnita_climate_clean.csv", row.names = FALSE)
write.csv(villamaria_monthly, "VillaMariaDelTriunfo_climate_clean.csv", row.names = FALSE)
write.csv(vonhumboldt_monthly, "LaMolina_climate_clean.csv", row.names = FALSE)
















