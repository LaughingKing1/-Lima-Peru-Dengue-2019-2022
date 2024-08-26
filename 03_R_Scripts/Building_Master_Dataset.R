# Aggrigating Incidence Data
install.packages("BiocManager")
install.packages("rgdal")
install.packages("terra")
library(pacman)
if (!requireNamespace("pacman", quietly = TRUE)) 

pckgs <- c("tidyverse", "readr", "ggplot2", "MODIStsp", "sf", "readxl", 'tmap', 'rmapshaper',
                  'janitor', 'terra', 'geodata', "dplyr", "rgdal",
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
all_weather <- read.csv("02_Cleaned_Data/allweather.csv")

# Elevation Raster
elevation_data <- rast("02_Cleaned_Data/Lima_District_Elevation.tif")

# Population Density Raster
PopulationDen_2019 <- rast("02_Cleaned_Data/Lima_PopDense19.tif")
PopulationDen_2020 <- rast("02_Cleaned_Data/Lima_PopDense20.tif")

vegitation01.19 <- rast("02_Cleaned_Data/Processed_Vegitation/VegitationLimaPer_MOD13C2.A2019001.061.2020286183227.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI.tif")

#Need this for master
#Show incidence in each district in each month each year
incidence_by_month_year_district <- incidence %>%
  mutate(Date = ymd(Date)) %>% # Convert Date to standard date format assuming year-month-day
  mutate(MonthYear = format(Date, "%Y-%m")) %>% # Extract year and month as "yyyy-mm"
  separate(MonthYear, c("Year", "Month"), "-") %>% # Split Year and Month into separate columns
  group_by(District, Year, Month) %>%  # Group by District, Year, and Month
  summarise(IncidenceCount = n()) # Count incidences per group

# View the summarized data
print(incidence_by_month_year_district)

################################################################################################

#Show incidence in each district in each year
incidence_by_year_district <- incidence %>%
  mutate(Year = year(Date)) %>% # Extract year from Date column
  group_by(District, Year) %>%  # Group by District and Year
  summarise(IncidenceCount = n()) # Count incidences per group

# View the summarized data
print(incidence_by_year_district)

# Incidence by district
incidence_district <- incidence %>%
  group_by(District) %>%  # Group by District and Year
  summarise(IncidenceCount = n()) # Count incidences per group

all_districts <- data.frame(
  District = c("BARRANCO", "BRENA", "CHORRILLOS", "EL AGUSTINO",
              "JESUS MARIA", "LA VICTORIA",
               "LURIN", "MAGDALENA DEL MAR", "MIRAFLORES",
               "PUCUSANA", "PUEBLO LIBRE", "PUNTA HERMOSA", "PUNTA NEGRA",
               "SAN BARTOLO", "SAN BORJA", "SAN ISIDRO",
               "SAN JUAN DE MIRAFLORES", "SAN LUIS", "SAN MIGUEL",
               "SANTA MARIA DEL MAR", "SANTA ROSA", "SURQUILLO",
               "VILLA EL SALVADOR"),
  IncidenceCount = rep(0, times = length(c("BARRANCO", "BRENA", "CHORRILLOS", "EL AGUSTINO",
                                           "JESUS MARIA", "LA VICTORIA",
                                           "LURIN", "MAGDALENA DEL MAR", "MIRAFLORES",
                                           "PUCUSANA", "PUEBLO LIBRE", "PUNTA HERMOSA", "PUNTA NEGRA",
                                           "SAN BARTOLO", "SAN BORJA", "SAN ISIDRO",
                                           "SAN JUAN DE MIRAFLORES", "SAN LUIS", "SAN MIGUEL",
                                           "SANTA MARIA DEL MAR", "SANTA ROSA", "SURQUILLO",
                                           "VILLA EL SALVADOR"))))

district_incidence <- rbind(incidence_district, all_districts)


#Show incidence in each district in each month each year
sex_by_month_year_district <- incidence %>%
  mutate(Date = ymd(Date)) %>% # Convert Date to standard date format assuming year-month-day
  mutate(MonthYear = format(Date, "%Y-%m")) %>% # Extract year and month as "yyyy-mm"
  separate(MonthYear, c("Year", "Month"), "-") %>% # Split Year and Month into separate columns
  group_by(District, Year, Month) %>%  # Group by District, Year, and Month
  summarise(IncidenceCount = n()) # Count incidences per group

# View the summarized data
print(incidence_by_month_year_district)


#Incidence by year and month
incidence_by_month_year <- incidence %>%
  mutate(Date = ymd(Date)) %>% # Convert Date to standard date format assuming year-month-day
  mutate(MonthYear = format(Date, "%Y-%m")) %>% # Extract year and month as "yyyy-mm"
  separate(MonthYear, c("Year", "Month"), "-") %>% # Split Year and Month into separate columns
  group_by(Year, Month) %>%  # Group by District, Year, and Month
  summarise(IncidenceCount = n()) # Count incidences per group

# View the summarized data
print(incidence_by_year_district)

#Sum of Incidence for each year
incidence_year <- incidence %>%
  mutate(Year = year(Date)) %>% # Extract year from Date column
  group_by(Year) %>%  # Group by District and Year
  summarise(IncidenceCount = n()) # Count incidences per group

# View the summarized data
print(incidence_year)

# Sum of incidence by month across all years
incidence_month <- incidence %>%
  mutate(Month = month(Date)) %>% # Extract year from Date column
  group_by(Month) %>%  # Group by District and Year
  summarise(IncidenceCount = n()) # Count incidences per group

# View the summarized data
print(incidence_month)

# Incidence by Quarter by Year
incidence_quarterly <- incidence %>%
  mutate(QtrYear = as.yearqtr(Date)) %>% # Convert Date to year and quarter
  group_by(Year = year(QtrYear), Quarter = quarter(QtrYear)) %>% # Group by year and quarter
  summarise(IncidenceCount = n(), .groups = "drop") # Summarize by counting incidents

##################################################################################################
# Start Plotting

# Plot cumulitive incidence per month 
ggplot(incidence_month, aes(x = Month, y = IncidenceCount)) +
  geom_line() + # Create a line plot
  labs(title = "Cummulitive Incidence Counts Over Months", x = "Month", y = "Incidence Count") +
  theme_minimal() # Apply a minimal theme

# Plotting cases per district over each month per year
ggplot(incidence_by_month_year_district, aes(x = factor(Month), y = IncidenceCount, color = District)) +
  geom_line(aes(group = District)) + # Connect points within each district
  facet_wrap(~Year, scales = "free_x") + # Separate plots for each year
  scale_x_discrete(labels = month.abb) + # Use abbreviated month names on the x-axis
  labs(
    title = "Monthly Incidence Counts by District",
    x = "Month",
    y = "Incidence Count",
    color = "District"
  ) +
  theme_minimal()


# Plot incidence over each month 
incidence_by_month_year$Date <- as.Date(paste(incidence_by_month_year$Year, incidence_by_month_year$Month, "01", sep = "-"), format = "%Y-%m-%d")

ggplot(incidence_by_month_year, aes(x = Date, y = IncidenceCount)) +
  geom_line(color = "darkblue") + # Draw a line
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") + # Customize x-axis to show months
  labs(
    title = "",
    x = "Month",
    y = "Incidence"
  ) +
  theme_minimal() # Apply a minimal theme for cleaner aesthetics
#######################################################################################################
#Quick  Demographic Checks
#Check split of Male Female
sex_counts <- table(incidence$Sex)

# Print the frequency table
print(sex_counts)

#Check age ranges
# Ensure age_ranges is a numeric vector
age_ranges <- c(-Inf, 5, 20, 30, 40, 64, Inf)
# Define the labels for each age group
labels <- c("<5", "6-20", "21-30", "31-40", "41-64", ">65")

incidence$Age_group <- cut(incidence$Age, breaks = age_ranges, labels = labels, right = FALSE)

# Count the occurrences of each age group
age_group_counts <- table(incidence$Age_group)

# Print the counts
print(age_group_counts)

# Check diagnosis type
fever_counts <- table(incidence$Disease)

##############################################################################################################################################################################################################
# Start to build master dataset
# Define the metrics I want
districts <- c("ANCON", "ATE", "BARRANCO", "BRENA", "CARABAYLLO", "CHACLACAYO", 
                            "CHORRILLOS", "CIENEGUILLA", "COMAS", "EL AGUSTINO", "INDEPENDENCIA", 
                            "JESUS MARIA", "LA MOLINA", "LA VICTORIA", "LIMA", "LINCE", "LOS OLIVOS", 
                            "LURIGANCHO", "LURIN", "MAGDALENA DEL MAR", "MIRAFLORES", "PACHACAMAC", "PUCUSANA", 
                            "PUEBLO LIBRE", "PUENTE PIEDRA", "PUNTA HERMOSA", "PUNTA NEGRA", "RIMAC", "SAN BARTOLO",
                            "SAN BORJA", "SAN ISIDRO", "SAN JUAN DE LURIGANCHO", "SAN JUAN DE MIRAFLORES", "SAN LUIS",
                            "SAN MARTIN DE PORRES", "SAN MIGUEL", "SANTA ANITA", "SANTA MARIA DEL MAR", "SANTA ROSA",
                            "SANTIAGO DE SURCO", "SURQUILLO", "VILLA EL SALVADOR", "VILLA MARIA DEL TRIUNFO")
df_districts <- data.frame(Districts = districts)

years <- 2019:2022 
all_months_years <- expand.grid(Month = month.name, Year = years)

# Create a dataframe that has 48 rows for each district for the full time frame so 12 months for 4 years (43x48)
repeats_per_district <- nrow(all_months_years)
totaldistricts <- data.frame(District = rep(districts, each = repeats_per_district), all_months_years)

# Add the Province
totaldistricts$Province <- "LIMA"

# Move it to the front
totaldistricts <- totaldistricts %>%
  dplyr::select(Province, everything())

#Change the incidence dataframe months to the names of the months
incidence_by_month_year_district <- incidence_by_month_year_district %>%
  mutate(Month = month.name[as.numeric(Month)])

# Convert "Year" column to character in totaldistricts
totaldistricts <- totaldistricts %>%
  mutate(Year = as.character(Year))
# Join my district bt m/y with my incidence data
merged_df <- left_join(totaldistricts, incidence_by_month_year_district, by = c("District", "Month", "Year"))
#Turn NA's into 0's
merged_df <- merged_df %>%
  mutate(IncidenceCount = ifelse(is.na(IncidenceCount), 0, IncidenceCount))

# Bring in Demographic Data
# Remove the Province column in demographics since i already have it
demographics <- demographics %>%
  dplyr::select(-Province, -X)
#Bring in my demographic data into master
merged_data <- left_join(merged_df, demographics, by = "District")
# Bring Department to the front
merged_data <- merged_data %>%
  dplyr::select(Department, everything())

#Change my year from a character into intiger format
merged_data$Year <- as.integer(merged_data$Year) 

#Start to bring in weather data

# Remove the ID column
all_weather <- all_weather %>%
  dplyr::select(-ID)

# Import my weather data into my master
merged_data <- left_join(merged_data, all_weather, by = c("District", "Month", "Year"))

#Add data from nearest district
setwd("~/LSHTM_23/Thesis/Lima_Dengue/01_Messy_Data")
#Write a copy of my master
write.csv(merged_data, "draft1masater.csv", row.names = FALSE)


##############################################################################################################################################################################################################
#Visualization of weather trends 

#Bring in original 10 station districts
setwd("~/LSHTM_23/Thesis/Lima_Dengue")

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
# List them and then bind them into one dataframe
district_list <- list(ancon, carabayllo, jesusmaria, lamolina, lurigancho,
                      sanborja, sanjuandelur, sanmartin,
                      santaanita, villamaria)
Station_weather <- do.call(rbind, district_list)
## Change the weather data to add a date column for visulaization
Station_weather$Month <- match(Station_weather$Month, month.name)
Station_weather$Date <- as.Date(paste(Station_weather$Year, Station_weather$Month, "01", sep = "-"))

# Plot the weather trends
ggplot(Station_weather, aes(x = Date, y = Precipitation)) +
  geom_line() +
  labs(title = "Precipitation Over Time by District", x = "Date", y = "Precipitation (mm)") +
  theme_minimal() +
  facet_wrap(~ District)

ggplot(Station_weather, aes(x = Date, y = Temperature)) +
  geom_line() +
  labs(title = "Temperature Over Time by District", x = "Date", y = "Temp (deg)") +
  theme_minimal() +
  facet_wrap(~ District)

ggplot(Station_weather, aes(x = Date, y = Relative_Humidity)) +
  geom_line() +
  labs(title = "Humidity Over Time by District", x = "Date", y = "Humidity (%)") +
  theme_minimal() +
  facet_wrap(~ District)

ggplot(Station_weather, aes(x = Date, y = Wind_Speed)) +
  geom_line() +
  labs(title = "Wind Speed Over Time by District", x = "Date", y = "Wind_Speed (m/s)") +
  theme_minimal() +
  facet_wrap(~ District)

##############################################################################################################################################################################################################
# Start working with Shapefiles and Rasters
##############################################################################################################################################################################################################
# Reduce the shapefile data to just the geomotry and the district columns
shps_districts_reduced <- shps_districts %>%
  dplyr::select(-c(GID_0, GID_1, GID_2, GID_3, COUNTRY, NAME_1, NAME_2, NL_NAME_1,
                   NL_NAME_2, NL_NAME_3, VARNAME_3, TYPE_3, HASC_3, CC_3, ENGTYPE_3))

#Fix the data in my shapefile so the file names for districts match up, changing BREÑA to BRENA and MAGDALENA VIEJA to PUEBLO LIBRE (Magdalena vieja is a historic name for the district which is not peublo libre)
shps_districts_reduced$District[shps_districts_reduced$District == "BREÑA"] <- "BRENA"
shps_districts_reduced$District[shps_districts_reduced$District == "MAGDALENA VIEJA"] <- "PUEBLO LIBRE"

#Merge the shapefiles for each district into the master dataset
merged_data <- left_join(merged_data, shps_districts_reduced, by = c("District"))


# Start with Elevation Raster

#Check corrdinate refrence systems
crs(shps_districts_reduced)
crs(elevation_data) # Elevation needs to be in the same crs as my admin boundaries

setwd("~/LSHTM_23/Thesis/Lima_Dengue/02_Cleaned_Data")
# They did not match up so adjusting elevation first into a raster than into the right crs
Elevation_districts_raster <- raster(elevation_data)
Elevation_districts_WGS84 <- projectRaster(Elevation_districts_raster, crs = "+proj=longlat +datum=WGS84 +no_defs", method = "bilinear")

# Assuming shps_districts is a SpatVector object with attributes you want to keep
# And assuming Elevation_districts_WGS84 is your cropped raster

# Extract the attributes from the vector
attributes <- shps_districts_reduced$District
str(shps_districts_reduced)
## read polygon

# cropping a raster to the extent of a shapefile
# extracting raster values along the boundaries of another shapefile 
# and then appending these values as a new attribute to the original shapefile.

clipped_elevation <- crop(Elevation_districts_WGS84, shps_districts_reduced)
plot(clipped_elevation)
head(clipped_elevation)
elevation_data <- terra::extract(clipped_elevation, shps_districts, method = 'bilinear',
                          fun = median, na.rm = TRUE, df = T)
shps_districts_reduced <- cbind(shps_districts_reduced, elevation_data)
# Plot for mean data
ggplot() +
  geom_sf(data = shps_districts_reduced, aes(fill = SouthMedian30S090W_20101117_gmted_med075)) +
  scale_fill_gradient(low = "blue", high = "green") +
  theme_minimal() +
  labs(title = "Average Elevation by District",
       fill = "Elevation Value")
# Plot for median data
ggplot() +
  geom_sf(data = shps_districts_reduced, aes(fill = SouthMedian30S090W_20101117_gmted_med075.1)) +
  scale_fill_gradient(low = "blue", high = "green") +
  theme_minimal() +
  labs(title = "Median Elevation by District",
       fill = "Elevation Value")

# I'm going with the median data as it appears to be less affeced by outlier data for elevation than the mean
shps_districts_reduced <- shps_districts_reduced %>%
  dplyr::select(-c(SouthMedian30S090W_20101117_gmted_med075, ID, ID.1))
# Rename the median elevation row
names(shps_districts_reduced)[which(names(shps_districts_reduced) == 
                                      "SouthMedian30S090W_20101117_gmted_med075.1")] <- 
  "Elevation"

merged_data <- left_join(merged_data, shps_districts_reduced, by = c("District", "geometry"))
##############################################################################################################################################################################################################
# Start on population density data
shps_districts_reduced <- shps_districts %>%
  dplyr::select(-c(GID_0, GID_1, GID_2, GID_3, COUNTRY, NAME_1, NAME_2, NL_NAME_1,
                   NL_NAME_2, NL_NAME_3, VARNAME_3, TYPE_3, HASC_3, CC_3, ENGTYPE_3))

#Fix the data in my shapefile so the file names for districts match up, changing BREÑA to BRENA and MAGDALENA VIEJA to PUEBLO LIBRE (Magdalena vieja is a historic name for the district which is not peublo libre)
shps_districts_reduced$District[shps_districts_reduced$District == "BREÑA"] <- "BRENA"
shps_districts_reduced$District[shps_districts_reduced$District == "MAGDALENA VIEJA"] <- "PUEBLO LIBRE"

# 2019
Pop2019_raster <- raster(PopulationDen_2019)
Pop2019_WGS84 <- projectRaster(Pop2019_raster, crs = "+proj=longlat +datum=WGS84 +no_defs", method = "bilinear")
#2020
Pop2020_raster <- raster(PopulationDen_2020)
Pop2020_WGS84 <- projectRaster(Pop2020_raster, crs = "+proj=longlat +datum=WGS84 +no_defs", method = "bilinear")

# 2019
clipped_PopDen_2019 <- crop(Pop2019_WGS84, shps_districts_reduced)
plot(clipped_PopDen_2019)
Pop2019_data <- terra::extract(clipped_PopDen_2019, shps_districts, method = 'bilinear',
                                 fun = mean, na.rm = TRUE, df = T)
shps_districts_pop2019 <- cbind(shps_districts_reduced, Pop2019_data)
# 2020
clipped_PopDen_2020 <- crop(Pop2020_WGS84, shps_districts_reduced)
plot(clipped_PopDen_2020)
Pop2020_data <- terra::extract(clipped_PopDen_2020, shps_districts, method = 'bilinear',
                               fun = mean, na.rm = TRUE, df = T)
# Put population data from 2020 into a seprate dataframe
shps_districts_pop2020 <- cbind(shps_districts_reduced, Pop2020_data)

# Replicate for 2021 and 2022 since 2020 is the most recent data that was available for Lima 
shps_districts_pop2021 <- shps_districts_pop2020
shps_districts_pop2022 <- shps_districts_pop2020

# Add year column to each of the new population dataframes
shps_districts_pop2019$Year <- 2019
#Change the column name for 2019 so it will match those for 2020
names(shps_districts_pop2019)[which(names(shps_districts_pop2019) == 
                                      "per_populationdensity_2019_1km")] <- 
  "per_populationdensity_2020_1km"
# Add year column to each of the new population dataframes
shps_districts_pop2020$Year <- 2020
shps_districts_pop2021$Year <- 2021
shps_districts_pop2022$Year <- 2022

# rbind all of these together into a single dataframe

all_pop_density <- rbind(shps_districts_pop2019, shps_districts_pop2020, shps_districts_pop2021,
                                 shps_districts_pop2022)
# Remove the Id Column
all_pop_density <- all_pop_density %>%
  dplyr::select(-ID)

# Add population density to merged data
merged_data <- left_join(merged_data, all_pop_density, by = c("District", "geometry", "Year"))
# Rename that column
names(merged_data)[which(names(merged_data) == 
                           "per_populationdensity_2020_1km")] <- 
  "Pop_Density"
# Write a copy of the new master data sheet
setwd("~/LSHTM_23/Thesis/Lima_Dengue/01_Messy_Data")
#Write a copy of my master
st_write(merged_data, "draft2master.geojson")
##############################################################################################################################################################################################################
# Start on Vegitation Data

file_directory <- "~/LSHTM_23/Thesis/Lima_Dengue/02_Cleaned_Data/Processed_Vegitation"
# Get a list of all .tif files in the current working directory
Vegitation_files <- list.files(path = file_directory, pattern = "MOD13C2.*\\.tif$", full.names = TRUE)

projected_vegetation <- lapply(Vegitation_files, function(file) {
  r <- raster(file)
  projectRaster(r, crs = "+proj=longlat +datum=WGS84 +no_defs", method = "bilinear")
})

clipped_vegetation <- lapply(projected_vegetation, function(r) {
  crop(r, shps_districts_reduced)
})

# Assuming shps_districts_reduced is your shapefile with administrative boundaries
# and it has an attribute 'District' that matches the filenames or names of the clipped rasters

# Initialize an empty data frame to store the extracted values
extracted_values <- data.frame()

for(i in seq_along(clipped_vegetation)) {
  # Extract raster values for each clipped raster
  values <- terra::extract(clipped_vegetation[[i]], shps_districts_reduced, method = 'bilinear', fun = mean, na.rm = TRUE, df = TRUE)
  
  # Rename columns based on the filename or district name
  colnames(values)[1] <- gsub("\\.tif$", "", basename(Vegitation_files[i]))
  
  # Append to the data frame
  extracted_values <- rbind(extracted_values, values)
}

# Add the extracted values as new attributes to your shapefile
shps_districts_reduced$extracted_values <- extracted_values

all_extracted_data <- list()

# Loop through each file, extract data, and assign unique column names
for(i in seq_along(Vegitation_files)) {
  # Extract raster values for each clipped raster
  # Assuming you want to use 'median' as the aggregation function
  # Adjust 'na.rm = TRUE' and 'df = TRUE' as needed
  values <- terra::extract(projected_vegetation[[i]], shps_districts_reduced, method = 'bilinear', fun = mean, na.rm = TRUE, df = TRUE)
  
  # Rename the column based on the filename or a part of it
  # This ensures each file's data goes into a unique column
  colnames(values)[1] <- paste0("Vegetation_", gsub("\\.tif$", "", basename(Vegitation_files[i])))
  
  # Append the data frame to the list
  all_extracted_data[[length(all_extracted_data) + 1]] <- values
}

# Convert the list of data frames into a single data frame
# Iterate over the list of data frames
# Create individual data frames and store them in the global environment
for(i in seq_along(all_extracted_data)) {
  # Create a unique name for each new data frame
  new_df_name <- paste0("df_", i)
  
  # Assign the new data frame to the global environment
  assign(new_df_name, all_extracted_data[[i]])
}

# Now give the correct month and year for each of the 48 new dataframes plus add the right district to each row
#January 2019
df_1 <- df_1 %>%
  dplyr::mutate(Year = 2019, Month = "January") %>%
  rename(Vegetation = MOD13C2.A2019001.061.2020286183227.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019001.061.2020286183227.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts)

df_1 <- df_1 %>% mutate(Vegetation = Vegetation*0.0001)
# February 2019
df_2 <- df_2 %>%
  dplyr::mutate(Year = 2019, Month = "February") %>%
  rename(Vegetation = MOD13C2.A2019032.061.2020289005752.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019032.061.2020289005752.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts)

df_2 <- df_2 %>% mutate(Vegetation = Vegetation*0.0001)
# March 2019
df_3 <- df_3 %>%
  dplyr::mutate(Year = 2019, Month = "March") %>%
  rename(Vegetation = MOD13C2.A2019060.061.2020291183248.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019060.061.2020291183248.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts)

df_3 <- df_3 %>% mutate(Vegetation = Vegetation*0.0001)
# April 2019
df_4 <- df_4 %>%
  dplyr::mutate(Year = 2019, Month = "April") %>%
  rename(Vegetation = MOD13C2.A2019091.061.2020293175428.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019091.061.2020293175428.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts)

df_4 <- df_4 %>% mutate(Vegetation = Vegetation*0.0001)
# May 2019
df_5 <- df_5 %>%
  dplyr::mutate(Year = 2019, Month = "May") %>%
  rename(Vegetation = MOD13C2.A2019121.061.2020298054312.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019121.061.2020298054312.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts)

df_5 <- df_5 %>% mutate(Vegetation = Vegetation*0.0001)

# June 2019
df_6 <- df_6 %>%
  dplyr::mutate(Year = 2019, Month = "June") %>%
  rename(Vegetation = MOD13C2.A2019152.061.2020303082503.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019152.061.2020303082503.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# July 2019
df_7 <- df_7 %>%
  dplyr::mutate(Year = 2019, Month = "July") %>%
  rename(Vegetation = MOD13C2.A2019182.061.2020304191154.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019182.061.2020304191154.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# August 2019
df_8 <- df_8 %>%
  dplyr::mutate(Year = 2019, Month = "August") %>%
  rename(Vegetation = MOD13C2.A2019213.061.2020308214657.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019213.061.2020308214657.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# September 2019
df_9 <- df_9 %>%
  dplyr::mutate(Year = 2019, Month = "September") %>%
  rename(Vegetation = MOD13C2.A2019244.061.2020314043412.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019244.061.2020314043412.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# October 2019
df_10 <- df_10 %>%
  dplyr::mutate(Year = 2019, Month = "October") %>%
  rename(Vegetation = MOD13C2.A2019274.061.2020316042726.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019274.061.2020316042726.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# November 2019
df_11 <- df_11 %>%
  dplyr::mutate(Year = 2019, Month = "November") %>%
  rename(Vegetation = MOD13C2.A2019305.061.2020319033001.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019305.061.2020319033001.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# December 2019
df_12 <- df_12 %>%
  dplyr::mutate(Year = 2019, Month = "December") %>%
  rename(Vegetation = MOD13C2.A2019335.061.2020323194535.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2019335.061.2020323194535.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

#January 2020
df_13 <- df_13 %>%
  dplyr::mutate(Year = 2020, Month = "January") %>%
  rename(Vegetation = MOD13C2.A2020001.061.2020328142931.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020001.061.2020328142931.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# February 2020
df_14 <- df_14 %>%
  dplyr::mutate(Year = 2020, Month = "February") %>%
  rename(Vegetation = MOD13C2.A2020032.061.2020335014658.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020032.061.2020335014658.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>%
  mutate(Vegetation = Vegetation*0.0001)

# March 2020
df_15 <- df_15 %>%
  dplyr::mutate(Year = 2020, Month = "March") %>%
  rename(Vegetation = MOD13C2.A2020061.061.2020335033234.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020061.061.2020335033234.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# April 2020
df_16 <- df_16 %>%
  dplyr::mutate(Year = 2020, Month = "April") %>%
  rename(Vegetation = MOD13C2.A2020092.061.2020335033238.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020092.061.2020335033238.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# May 2020
df_17 <- df_17 %>%
  dplyr::mutate(Year = 2020, Month = "May") %>%
  rename(Vegetation = MOD13C2.A2020122.061.2020336000916.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020122.061.2020336000916.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# June 2020
df_18 <- df_18 %>%
  dplyr::mutate(Year = 2020, Month = "June") %>%
  rename(Vegetation = MOD13C2.A2020153.061.2020340140629.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020153.061.2020340140629.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# July 2020
df_19 <- df_19 %>%
  dplyr::mutate(Year = 2020, Month = "July") %>%
  rename(Vegetation = MOD13C2.A2020183.061.2020342033627.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020183.061.2020342033627.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# August 2020
df_20 <- df_20 %>%
  dplyr::mutate(Year = 2020, Month = "August") %>%
  rename(Vegetation = MOD13C2.A2020214.061.2020346124348.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020214.061.2020346124348.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# September 2020
df_21 <- df_21 %>%
  dplyr::mutate(Year = 2020, Month = "September") %>%
  rename(Vegetation = MOD13C2.A2020245.061.2020349222009.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020245.061.2020349222009.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# October 2020
df_22 <- df_22 %>%
  dplyr::mutate(Year = 2020, Month = "October") %>%
  rename(Vegetation = MOD13C2.A2020275.061.2020353114309.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020275.061.2020353114309.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# November 2020
df_23 <- df_23 %>%
  dplyr::mutate(Year = 2020, Month = "November") %>%
  rename(Vegetation = MOD13C2.A2020306.061.2020357112600.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020306.061.2020357112600.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# December 2020
df_24 <- df_24 %>%
  dplyr::mutate(Year = 2020, Month = "December") %>%
  rename(Vegetation = MOD13C2.A2020336.061.2021012033415.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2020336.061.2021012033415.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

#January 2021
df_25 <- df_25 %>%
  dplyr::mutate(Year = 2021, Month = "January") %>%
  rename(Vegetation = MOD13C2.A2021001.061.2021043134142.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021001.061.2021043134142.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# February 2021
df_26 <- df_26 %>%
  dplyr::mutate(Year = 2021, Month = "February") %>%
  rename(Vegetation = MOD13C2.A2021032.061.2021068110257.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021032.061.2021068110257.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>%
  mutate(Vegetation = Vegetation*0.0001)

# March 2021
df_27 <- df_27 %>%
  dplyr::mutate(Year = 2021, Month = "March") %>%
  rename(Vegetation = MOD13C2.A2021060.061.2021098024842.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021060.061.2021098024842.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# April 2021
df_28 <- df_28 %>%
  dplyr::mutate(Year = 2021, Month = "April") %>%
  rename(Vegetation = MOD13C2.A2021091.061.2021133165855.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021091.061.2021133165855.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# May 2021
df_29 <- df_29 %>%
  dplyr::mutate(Year = 2021, Month = "May") %>%
  rename(Vegetation = MOD13C2.A2021121.061.2021165125210.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021121.061.2021165125210.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# June 2021
df_30 <- df_30 %>%
  dplyr::mutate(Year = 2021, Month = "June") %>%
  rename(Vegetation = MOD13C2.A2021152.061.2021194035425.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021152.061.2021194035425.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# July 2021
df_31 <- df_31 %>%
  dplyr::mutate(Year = 2021, Month = "July") %>%
  rename(Vegetation = MOD13C2.A2021182.061.2021226060459.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021182.061.2021226060459.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# August 2021
df_32 <- df_32 %>%
  dplyr::mutate(Year = 2021, Month = "August") %>%
  rename(Vegetation = MOD13C2.A2021213.061.2021258051026.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021213.061.2021258051026.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# September 2021
df_33 <- df_33 %>%
  dplyr::mutate(Year = 2021, Month = "September") %>%
  rename(Vegetation = MOD13C2.A2021244.061.2021322130311.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021244.061.2021322130311.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# October 2021
df_34 <- df_34 %>%
  dplyr::mutate(Year = 2021, Month = "October") %>%
  rename(Vegetation = MOD13C2.A2021274.061.2022066102004.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021274.061.2022066102004.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# November 2021
df_35 <- df_35 %>%
  dplyr::mutate(Year = 2021, Month = "November") %>%
  rename(Vegetation = MOD13C2.A2021305.061.2021338165341.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021305.061.2021338165341.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# December 2021
df_36 <- df_36 %>%
  dplyr::mutate(Year = 2021, Month = "December") %>%
  rename(Vegetation = MOD13C2.A2021335.061.2022005004102.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2021335.061.2022005004102.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

#January 2022
df_37 <- df_37 %>%
  dplyr::mutate(Year = 2022, Month = "January") %>%
  rename(Vegetation = MOD13C2.A2022001.061.2022035060912.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022001.061.2022035060912.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# February 2022
df_38 <- df_38 %>%
  dplyr::mutate(Year = 2022, Month = "February") %>%
  rename(Vegetation = MOD13C2.A2022032.061.2022066011432.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022032.061.2022066011432.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>%
  mutate(Vegetation = Vegetation*0.0001)

# March 2022
df_39 <- df_39 %>%
  dplyr::mutate(Year = 2022, Month = "March") %>%
  rename(Vegetation = MOD13C2.A2022060.061.2022108194209.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022060.061.2022108194209.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# April 2022
df_40 <- df_40 %>%
  dplyr::mutate(Year = 2022, Month = "April") %>%
  rename(Vegetation = MOD13C2.A2022091.061.2022136120313.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022091.061.2022136120313.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# May 2022
df_41 <- df_41 %>%
  dplyr::mutate(Year = 2022, Month = "May") %>%
  rename(Vegetation = MOD13C2.A2022121.061.2022168105355.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022121.061.2022168105355.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# June 2022
df_42 <- df_42 %>%
  dplyr::mutate(Year = 2022, Month = "June") %>%
  rename(Vegetation = MOD13C2.A2022152.061.2022201123617.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022152.061.2022201123617.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# July 2022
df_43 <- df_43 %>%
  dplyr::mutate(Year = 2022, Month = "July") %>%
  rename(Vegetation = MOD13C2.A2022182.061.2022232164108.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022182.061.2022232164108.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# August 2022
df_44 <- df_44 %>%
  dplyr::mutate(Year = 2022, Month = "August") %>%
  rename(Vegetation = MOD13C2.A2022213.061.2022258145129.psgs_000502262911.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022213.061.2022258145129.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# September 2022
df_45 <- df_45 %>%
  dplyr::mutate(Year = 2022, Month = "September") %>%
  rename(Vegetation = MOD13C2.A2022244.061.2022297185735.psgs_000502262893.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022244.061.2022297185735.psgs_000502262893-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# October 2022
df_46 <- df_46 %>%
  dplyr::mutate(Year = 2022, Month = "October") %>%
  rename(Vegetation = MOD13C2.A2022274.061.2022307143218.psgs_000502262893.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022274.061.2022307143218.psgs_000502262893-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# November 2022
df_47 <- df_47 %>%
  dplyr::mutate(Year = 2022, Month = "November") %>%
  rename(Vegetation = MOD13C2.A2022305.061.2022351055011.psgs_000502262893.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022305.061.2022351055011.psgs_000502262893-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# December 2022
df_48 <- df_48 %>%
  dplyr::mutate(Year = 2022, Month = "December") %>%
  rename(Vegetation = MOD13C2.A2022335.061.2023006093823.psgs_000502262893.CMG_0_05_Deg_Monthly_NDVI) %>%
  dplyr::select(-c(`Vegetation_VegitationLimaPer_MOD13C2.A2022335.061.2023006093823.psgs_000502262893-CMG_0_05_Deg_Monthly_NDVI`))%>%
  mutate(District = districts) %>% 
  mutate(Vegetation = Vegetation*0.0001)

# Bind all the data into a single data frame
all_veg <- rbind(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10, df_11, df_12,
                 df_13, df_14, df_15, df_16, df_17, df_18, df_19, df_20, df_21, df_22, df_23, df_24,
                 df_25, df_26, df_27, df_28, df_29, df_30, df_31, df_32, df_33, df_34, df_35, df_36,
                 df_37, df_38, df_39, df_40, df_41, df_42, df_43, df_44, df_45, df_46, df_47, df_48
                 )

#Check NA files for the last 4 months of 2022
testveg4 <- rast("VegitationLimaPer_MOD13C2.A2022244.061.2022297185735.psgs_000502262893-CMG_0_05_Deg_Monthly_NDVI.tif")
testveg1 <- rast("VegitationLimaPer_MOD13C2.A2022274.061.2022307143218.psgs_000502262893-CMG_0_05_Deg_Monthly_NDVI.tif")
testveg2 <- rast("VegitationLimaPer_MOD13C2.A2022305.061.2022351055011.psgs_000502262893-CMG_0_05_Deg_Monthly_NDVI.tif")
testveg3 <- rast("VegitationLimaPer_MOD13C2.A2022335.061.2023006093823.psgs_000502262893-CMG_0_05_Deg_Monthly_NDVI.tif")
plot(testveg1)
plot(testveg2)
plot(testveg3)
plot(testveg4)
# All four are missing the santa rosa and ancon data from 
# Replace the NAN with NA
all_veg[all_veg == "NaN"] <- NA

#Bring in the veg data into my master
merged_data <- left_join(merged_data, all_veg, by = c("District", "Year", "Month"))

# Save my dataframe as a master dataset
setwd("~/LSHTM_23/Thesis/Lima_Dengue/02_Cleaned_Data")

st_write(merged_data, "masterdengue.geojson")

#Fix the vegetation NA's

# bring in master data set
master <- st_read("02_Cleaned_Data/masterdengue.geojson")

#Fix December NA's in Master
master <- master %>%
  mutate(Vegetation = case_when(
    District == "ANCON" & Month == "December" & is.na(Vegetation) ~ 0.07676443,
    District == "SANTA ROSA" & Month == "December" & is.na(Vegetation) ~ 0.08596241,
    TRUE ~ Vegetation  # Keep original value if neither condition is met
  ))
# Novmber
master <- master %>%
  mutate(Vegetation = case_when(
    District == "ANCON" & Month == "November" & is.na(Vegetation) ~ 0.08030781,
    District == "SANTA ROSA" & Month == "November" & is.na(Vegetation) ~ 0.08225633,
    TRUE ~ Vegetation  # Keep original value if neither condition is met
  ))
# October
master <- master %>%
  mutate(Vegetation = case_when(
    District == "ANCON" & Month == "October" & is.na(Vegetation) ~ 0.08107214,
    District == "SANTA ROSA" & Month == "October" & is.na(Vegetation) ~ 0.08436890,
    TRUE ~ Vegetation  # Keep original value if neither condition is met
  ))
# September
master <- master %>%
  mutate(Vegetation = case_when(
    District == "ANCON" & Month == "September" & is.na(Vegetation) ~ 0.09459261,
    District == "SANTA ROSA" & Month == "September" & is.na(Vegetation) ~ 0.09305987,
    TRUE ~ Vegetation  # Keep original value if neither condition is met
  ))

master <- rename(master, Pop_1NBI = `X._Pop_1_NBI`, Inadq_House = `X._Inadq_Housing`, Overcrowd = `X._Overcrowding`,
                 Pop_No_Hygeine = `X._Pop_No_Hygeine`, Econ_Depend = `X._High_Econ_Depend`,
                 No_public_H2O = `X._Pop_No_pubwater`, NoLights = `X._No_Light`, DirtFloor = `X._Homes_Dirt_Floor`,
                 UseFirewood = `X._Homes_Firewood`, NoComputer = `X._no_computer`, NoCellPhone = `X._no_cellphone`,
                 NoInternet = `X._no_internet`, OneRoom = `X._Homes_1_room`, Diff_Mov = `X._Difficulty_Moving`,
                 LimitedInter = `X._limited_interact_wothes`)


st_write(master, "masterdenguev2.geojson")
