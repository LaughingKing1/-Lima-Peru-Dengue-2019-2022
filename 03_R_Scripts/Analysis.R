  
pckgs <- c("tidyverse", "readr", "ggplot2", "MODIStsp", "sf", "readxl", 'tmap', 'rmapshaper',
             'janitor', 'terra', 'geodata', "dplyr", "rgdal",
             "PrevMap", "mapview", "automap", "spdep", "spatialreg",
             "geoR", 'gdistance', 'Hmisc', 'raster', "pals",
             "tmap", "readxl", "zoo", "lubridate", "padr", "hms", "purrr", "gstat" ,
           "ggspatial", "sp", "exactextractr", "tidyr", "MASS", "lmtest", "pscl",
           "DHARMa")

source("r_functions/functions.R")
if (!requireNamespace("pacman", quietly = TRUE)) 
options(repos = BiocManager::repositories())
pacman::p_load (pckgs, character.only=TRUE)

setwd("~/LSHTM_23/Thesis/Lima_Dengue")
# bring in master data set
master <- st_read("02_Cleaned_Data/masterdenguev2.geojson")

#Change population density to an integer
master$Pop_Density <- as.integer(master$Pop_Density)

# Add a column for total population for each district from 2017 census
District_pop <- read.csv("00_Raw_Data/PopulationDistricts.csv")
District_pop <- District_pop %>%
  dplyr::select(-c(X)) %>%
  rename(District = 'Messy_Demographics.District', Population = 
           'Messy_Demographics.Pop_Occupied'
)

#Bring in total pop for each district
master <- left_join(master, District_pop, by = "District")
#Divide incidence by population
master$IncidenceRate <- master$IncidenceCount / master$Population
#Create a Date Column
months <- c("January" = "01", "February" = "02", "March" = "03", "April" = "04",
            "May" = "05", "June" = "06", "July" = "07", "August" = "08",
            "September" = "09", "October" = "10", "November" = "11", "December" = "12")

master$month <- months[master$Month]

# Combine year and month columns into a single Date column
master$Date <- as.yearmon(paste(master$Year, master$month, sep = "-"))

# Optionally, convert to Date class if needed for further analysis
master$Date <- as.Date(master$Date)

# Look at yearly incidence for each district
# Aggrigate the incidence
yearly_incidence <- master %>%
  group_by(Year, District) %>%
  summarise(total_incidence = sum(IncidenceRate, na.rm = TRUE))

yearly_incidence$total_incidence <- yearly_incidence$total_incidence*100000
#-----------------------------------------------------------------------------------------
#Create lag columns function dplyr lab function

# Create a 1 month lag column First for Temperature then Precipitation and then Humidity 
# Temperature
#Temp
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag1_Temperature = lag(Temperature, n = 1))
# Rain
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag1_Precipitation = lag(Precipitation, n = 1))
# humidity
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag1_Relative_Humidity = lag(Relative_Humidity, n = 1))

# Create a 2 month lag column First for Temperature then Precipitation and then Humidity 
#Temp
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag2_Temperature = lag(Temperature, n = 2))
# Rain
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag2_Precipitation = lag(Precipitation, n = 2))
# humidity
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag2_Relative_Humidity = lag(Relative_Humidity, n = 2))

# Create a 3 month lag column First for Temperature then Precipitation and then Humidity 
#Temp
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag3_Temperature = lag(Temperature, n = 3))
# Rain
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag3_Precipitation = lag(Precipitation, n = 3))
# humidity
master <- master %>%
  arrange(District, Year, month) %>%
  group_by(District) %>%
  mutate(lag3_Relative_Humidity = lag(Relative_Humidity, n =3))

# Remove the Na rows(first three months of 2019) that have not values from weather in 2018
lag_model_master <- master %>%
  filter(!(year(Date) == 2019 & month(Date) %in% c(1, 2, 3)))

# Create columns in master to have uniform headings even for the base variable
lag_model_master$lag0_Precipitation <- lag_model_master$Precipitation
lag_model_master$lag0_Temperature <- lag_model_master$Temperature
lag_model_master$lag0_Relative_Humidity <- lag_model_master$Relative_Humidity

#############################################
# Might not need the below code so ignore keeping just in case skip till note below
yearly_incidence$incidence_bin <- cut(yearly_incidence$total_incidence,
                                     breaks = c(-Inf, 0, 5, 10, 20, 40, 60, Inf),
                                     labels = c("0", "1-5", "6-10", "11-20", "21-40", "41-60", ">60"),
                                     include.lowest = TRUE)

# Plot
ggplot() +
  geom_sf(data = yearly_incidence, aes(fill = incidence_bin)) + # Use fill based on the new bin variable
  facet_wrap(~Year) +
  scale_fill_manual(values = c("0" = "pink", "1-5" = "white", "6-10" = "grey", "11-20" = "blue", "21-40" = "darkblue", "41-60" = "black", ">60" = "red")) + # Define custom colors for each bin
  labs(title = "Incidence by Year and District", fill = "Incidence Bin") + # Update the legend title
  theme_minimal()

########### Start again here #############################################
#Plot yearly incidence per district
# Loop through each unique year in the dataset
for(year in unique(yearly_incidence$Year)) {
  # Filter the dataset to include only the current year
  current_year_data <- yearly_incidence %>%
    filter(Year == year)
  
  # Create the plot for the current year
  p <- ggplot() +
    geom_sf(data = current_year_data, aes(fill = total_incidence)) + 
    scale_fill_continuous(low = "white", high = "darkred") + 
    labs(title = paste("Incidence by District (", year, ")"), fill = "Incidence per 100,000") + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_nautical())
  # Print the plot
  print(p)
}

# Aggregate by month
month_incidence <- master %>%
  group_by(Month) %>%
  summarise(total_incidence = sum(IncidenceCount, na.rm = TRUE))
  
######################################################################################################## 
# Start visualinzing weather data

# Aggregate for climate factors
#Vegitation
veg <- master %>%
  group_by(Year, Month) %>%
  summarise(veg = mean(Vegetation, na.rm = TRUE))

# Precipitation 
rain <- master %>%
  group_by(Year, Month) %>%
  summarise(rain = mean(Precipitation, na.rm = TRUE))
### validate my assumption *here*
all(rain$Month %in% month.name)
# [1] TRUE
### convert to a factor, order defined by `month.name`
rain$Month <- factor(rain$Month, levels=month.name)

# Rain per District
rain_district <- master %>%
  group_by(Year, Month, District) %>%
  summarise(rain = mean(Precipitation, na.rm = TRUE))
### validate my assumption *here*
all(rain_district$Month %in% month.name)
# [1] TRUE
### convert to a factor, order defined by `month.name`
rain_district$Month <- factor(rain_district$Month, levels=month.name)

# Wind
wind <- master %>%
  group_by(Year, Month) %>%
  summarise(Avg_Wind_Speed = mean(Wind_Speed, na.rm = TRUE))
### validate my assumption *here*
all(wind$Month %in% month.name)
# [1] TRUE
### convert to a factor, order defined by `month.name`
wind$Month <- factor(wind$Month, levels=month.name)

# Wind
wind_district <- master %>%
  group_by(Year, Month, District) %>%
  summarise(Avg_Wind_Speed = mean(Wind_Speed, na.rm = TRUE))
### validate my assumption *here*
all(wind_direction$Month %in% month.name)
# [1] TRUE
### convert to a factor, order defined by `month.name`
wind_direction$Month <- factor(wind_direction$Month, levels=month.name)

# Wind per district
wind_direction <- master %>%
  group_by(Year, Month) %>%
  summarise(Avg_Wind_Dir = mean(Wind_Direction_Degrees, na.rm = TRUE))

# Humidity
humidity <- master %>%
  group_by(Year, Month) %>%
  summarise(Avg_Humidity = mean(Relative_Humidity, na.rm = TRUE))
### validate my assumption *here*
all(humidity$Month %in% month.name)
# [1] TRUE
### convert to a factor, order defined by `month.name`
humidity$Month <- factor(humidity$Month, levels=month.name)

# Temperature
Temperature <- master %>%
  group_by(Year, Month) %>%
  summarise(Avg_temp = mean(Temperature, na.rm = TRUE))
### validate my assumption *here*
all(Temperature$Month %in% month.name)
# [1] TRUE
### convert to a factor, order defined by `month.name`
Temperature$Month <- factor(Temperature$Month, levels=month.name)

# Incidence
month_incidence <- master %>%
  group_by(Year, Month) %>%
  summarise(total_incidence = sum(IncidenceCount, na.rm = TRUE))
### validate my assumption *here*
all(month_incidence$Month %in% month.name)
# [1] TRUE
### convert to a factor, order defined by `month.name`
month_incidence$Month <- factor(month_incidence$Month, levels=month.name)
sum(month_incidence$total_incidence)
# Convert Month to a factor with levels ordered by calendar month
Temperature$Month <- factor(avg_temp_by_month$Month, levels = 1:12, labels = month.abb)

# Graph Rain trends
ggplot(rain, aes(x = Month, y = rain, group = Year)) +
  geom_line(aes(color = factor(Year))) +
  labs(
    title = "Cumulative Precipitation by Month",
    x = "Month",
    y = "Total Precipitation (mm)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Graph Temp Trends
ggplot(Temperature, aes(x = Month, y = Avg_temp, group = Year)) +
  geom_line(aes(color = factor(Year))) +
  labs(
    title = "Average Temperature by Month",
    x = "Month",
    y = "Average Temperature (°C)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Graph Humidity trends
ggplot(humidity, aes(x = Month, y = Avg_Humidity, group = Year)) +
  geom_line(aes(color = factor(Year))) +
  labs(
    title = "Average Humidity by Month",
    x = "Month",
    y = "Average Humidity (%)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Graph Wind Speed trends
ggplot(wind, aes(x = Month, y = Avg_Wind_Speed, group = Year)) +
  geom_line(aes(color = factor(Year))) +
  labs(
    title = "Average Wind Speed by Month",
    x = "Month",
    y = "Average Wind Speed (m/s)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Graph Wind Direction
ggplot(wind_direction, aes(x = Month, y = Avg_Wind_Dir, group = Year)) +
  geom_line(aes(color = factor(Year))) +
  labs(
    title = "Average Wind Direction by Month",
    x = "Month",
    y = "Average Direction (degrees)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# No Hygiene Map

# Create the plot for the current year
ggplot() +
    geom_sf(data = master, aes(fill = Pop_No_Hygeine)) + 
    scale_fill_continuous(low = "white", high = "aquamarine3") + 
    labs(title = paste("Lack of Hygienic Services by District"), fill = "Percent of Households") + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_nautical())

# Illiteracy Rate map
ggplot() +
  geom_sf(data = master, aes(fill = Illiteracy_Rate)) + 
  scale_fill_continuous(low = "white", high = "purple4") + 
  labs(title = paste("Illiteracy Rate by District"), fill = "Percent Illiterate") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_nautical())

# Population
ggplot() +
  geom_sf(data = master, aes(fill = Population)) + 
  scale_fill_continuous(low = "white", high = "orange2") + 
  labs(title = paste("Population by District"), fill = " Number of People") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_nautical())

# Incidnec
ggplot(month_incidence, aes(x = Month, y = total_incidence, group = Year)) +
  geom_line(aes(color = factor(Year))) +
  labs(
    title = "Incidence by Month",
    x = "Month",
    y = "Dengue Case Count",
    color = "Year"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

########################################################################################################

#Start moran's I analysis
#Break up my master dataset by year, district and population
master2019 <- master %>%
  filter(Year == 2019)
master2019 <- master2019 %>%
  group_by(Year, District) %>%
  summarise(total_incidence = sum(IncidenceCount, na.rm = TRUE))
# 2020
master2020 <- master %>%
  filter(Year == 2020)
master2020 <- master2020 %>%
  group_by(Year, District, Population) %>%
  summarise(total_incidence = sum(IncidenceCount, na.rm = TRUE))
hist(master2020$total_incidence)
#2021
master2021 <- master %>%
  filter(Year == 2021)
master2021 <- master2021 %>%
  group_by(Year, District, Population) %>%
  summarise(total_incidence = sum(IncidenceCount, na.rm = TRUE))
hist(master2021$total_incidence)
# 2022
master2022 <- master %>%
  filter(Year == 2022)
master2022 <- master2022 %>%
  group_by(Year, District, Population) %>%
  summarise(total_incidence = sum(IncidenceCount, na.rm = TRUE))
# All of the four years combined
allmaster <- master %>%
  group_by(District, Population) %>%
  summarise(total_incidence = sum(IncidenceCount, na.rm = TRUE))
hist(allmaster$total_incidence)


# Plotting discrete data
# plot the geometry of the shapefile
plot(master$geometry, border = 1, lwd = 0.2)


#-------------------------------------------------------------------------------
# Start Morans in  2020
# plot the data again but shade regions by their incidence rates 
tm_shape(master2020) +
  tm_polygons(col = "total_incidence",
              title = "Dengue Incidence Rate",
              style = "quantile",
              palette = "viridis",
              alpha = 0.7,
              lwd = 0.2,
              n = 4,
              border.col = 1,
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

# Plotting data attributes: dengue incidence histogram
ggplot(master2020, aes(x = total_incidence)) +
  geom_histogram(colour = "gray1",
                 fill = "blue",
                 alpha = 0.7,
                 binwidth = 5) +
  theme_bw() # this step just makes things look nice.

# Changing the data into the logscale because it is not normally distributed
master2020$logitp <- log((master2020$total_incidence + 0.5)/(master2020$Population - master2020$total_incidence + 0.5))
hist(master2020$logitp, xlab = "", main = "Empirical logit",
     border = "black", col = "seagreen3")

# Define which regions are neighbours to one another based on queen contiguity
neighbours <- poly2nb(master2020$geometry)
# Summarise results
neighbours

# Visualize the neighbor matrix that I just made
lima_centroids <- master2020 %>%
  st_centroid()
plot(master2020$geometry, lwd = 0.2)
plot(neighbours, master2020$geometry,
     col = 'red', cex = 0.1, lwd = 1, add = TRUE)

# Update the neighbor pattern to a row standardized weights matrix.
weights_queen <- nb2listw(neighbours, style = "W", zero.policy = TRUE)

# compare neighbor pattern defined by queen contiguity with...
weights_queen$neighbours
# ...the row standardized weights (only the first 5 rows ).
weights_queen$weights[1:5]

# complete a Global Moran's I test
lima_moran2020 <- moran.test(master2020$logitp, listw = weights_queen, zero.policy = TRUE)
lima_moran2020

# Plot a Moran's Scatter plot using scaled data
# scale the dengue incidence parameter
master2020$scaled_incidence <- scale(master2020$logitp)
# crete a lag vector from the neighbor list and teh scaled dengue incidence values
master2020$lag_scaled_incidence <- lag.listw(weights_queen,
                                         master2020$scaled_incidence, zero.policy =  TRUE)
# plot the output
ggplot(data = master2020, aes(x = scaled_incidence, y = lag_scaled_incidence)) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", linetype = "dashed") +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab("Dengue Incidence (scaled)") +
  ylab("Incidence lag vector") +
  ggtitle("2020", ) +
  theme(plot.title = element_text(hjust = 0.5))

# Simulate the 2020 incidence data with a random distribution over the lima regions 10000 times
mc_moran20 <- moran.mc(master2020$logitp, listw = weights_queen,
                       nsim = 10000, zero.policy = TRUE)
# Plot a simple histogram of the data
hist(mc_moran20$res, breaks = 50)

# extract the residuals from the MC simulation output above
mc_moran_df20 <- as.data.frame(mc_moran20$res)
# change the column name to something easier to write
colnames(mc_moran_df20) <- "mcresiduals"
# plot MC residuals and compare against pur Moran
ggplot(mc_moran_df20, aes(x = mcresiduals)) +
  geom_histogram(colour = "gray1", fill = "pink3",
                 alpha = 0.7, binwidth = 0.01) +
  geom_vline(xintercept = lima_moran2020$estimate[1],
             colour = "red", linetype = "dashed") +
  xlab("Monte Carlo Residuals") +
  ylab("Count") +
  ggtitle("2020", ) +
  theme(plot.title = element_text(hjust = 0.5))

# We can extract the pseudo p-value from the simulation object we already made
mc_moran20

# Local Analysis 2020 ---------------------------------------------------------------

# Calculate the local Moran statistic for each region using queen contiguity
local_moran2020 <- localmoran(master2020$logitp,
                          weights_queen, # our weights object
                          zero.policy = TRUE,
                          alternative = "two.sided")
# Replace the column names to make it clearer
colnames(local_moran2020) <- c("local_I", "expected_I",
                           "variance_I", "z_statistic", "p_value")
head(local_moran2020)

# Join Local Moran's I information back on to the spatial data for plotting
local_moran2020 <- cbind(master2020, local_moran2020) %>%
  dplyr::select(scaled_incidence, lag_scaled_incidence, p_value, geometry)
# plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_moran2020) +
  tm_polygons(col = "p_value", title = "Local Moran 2020",
              style = "fixed", breaks = c(1*10^-20, 0.05, 0.1, 1),
              lwd = 0.2, border.col = 1, 
              palette = "-Accent") +
  tm_layout(legend.position = c("right", "top"), legend.outside = TRUE)

# Read in the function for analysis
source("r_functions/map_maker.R")
# Join our local moran outputs to the main data and select columns for analysis
local_map2020 <- master2020 %>%
  cbind(., local_moran2020) %>% 
  dplyr::select(scaled_incidence, lag_scaled_incidence, p_value, geometry)
# Run the map_maker function
local_map2020 <- map_maker(local_map2020)
local_map2020 <- map_maker005(local_map2020)
# Plot the number of each cluster type
table(local_map2020$cluster)
# Plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_map2020) +
  tm_polygons(col = "cluster",
              title = "Clusters",
              style = "fixed",
              palette = "-Set3", # Corrected palette specification
              lwd = 0.2,
              border.col = 1) +
  tm_layout(legend.position = c(".1", ".5"),
            legend.outside = TRUE,# Legend outside the plot
            title = "LISA 2020", # Directly setting the plot title
            title.size = 1, # Adjusting the title size
            title.fontfamily = "fixed") # Adjusting the title font family
# Sensative analysis with p < .005
tm_shape(local_map2020) +
  tm_polygons(col = "cluster",
              title = "Clusters",
              style = "fixed",
              palette = "-Set3", # Corrected palette specification
              lwd = 0.2,
              border.col = 1) +
  tm_layout(legend.position = c(".1", ".5"),
            legend.outside = TRUE,# Legend outside the plot
            title = "LISA 2020", # Directly setting the plot title
            title.size = 1, # Adjusting the title size
            title.fontfamily = "fixed") +
  tm_credits("Sensitivity Analysis",
             position = c("right", "top"),
             just = "bottom",
             size = 0.8, # Adjust the size as needed
             fontfamily = "fixed")

###################################################################################
# Move to 2021
tm_shape(master2021) +
  tm_polygons(col = "total_incidence",
              title = "Dengue Incidence Rate",
              style = "quantile",
              palette = "viridis",
              alpha = 0.7,
              lwd = 0.2,
              n = 4,
              border.col = 1,
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

# Plotting data attributes: dengue 2021 histogram
ggplot(master2021, aes(x = total_incidence)) +
  geom_histogram(colour = "gray1",
                 fill = "blue",
                 alpha = 0.7,
                 binwidth = 5) +
  theme_bw() # this step just makes things look nice.

# Changing the data into the logscale because it was not normally distributed
master2021$logitp <- log((master2021$total_incidence + 0.5)/(master2021$Population - master2021$total_incidence + 0.5))
hist(master2021$logitp, xlab = "", main = "Empirical logit",
     border = "black", col = "pink")

# Define which regions are neighbors to one another based on queen contiguity
neighbours <- poly2nb(master2021$geometry)
# Summarize results
neighbours
# Visualize the neighbor matrix that I just made
lima_centroids21 <- master2021 %>%
  st_centroid()
plot(master2021$geometry, lwd = 0.2)
plot(neighbours, master2021$geometry,
     col = 'red', cex = 0.1, lwd = 1, add = TRUE)

# Update the neighbor pattern to a row standardized weights matrix.
weights_queen <- nb2listw(neighbours, style = "W", zero.policy = TRUE)

# compare neighbor pattern defined by queen contiguity with...
weights_queen$neighbours
# ...the row standardized weights (only the first 5 rows ).
weights_queen$weights[1:5]

# complete a Global Moran's I test
lima_moran21 <- moran.test(master2021$logitp, listw = weights_queen, zero.policy = TRUE)
lima_moran21

# Plot a Moran's Scatter plot using scaled data
# scale the dengue incidence parameter
master2021$scaled_incidence <- scale(master2021$logitp)
# create a lag vector from the neighbor list and the scaled dengue incidence values
master2021$lag_scaled_incidence <- lag.listw(weights_queen,
                                             master2021$scaled_incidence, zero.policy =  TRUE)
# plot the output
ggplot(data = master2021, aes(x = scaled_incidence, y = lag_scaled_incidence)) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", linetype = "dashed") +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab("Dengue Incidence (scaled)") +
  ylab("Incidence lag vector") +
  ggtitle("2021", ) +
  theme(plot.title = element_text(hjust = 0.5))

# Simulate the 2020 incidence data with a random distribution over the Lima regions 10000 times
mc_moran21 <- moran.mc(master2021$logitp, listw = weights_queen,
                       nsim = 10000, zero.policy = TRUE)
# Plot a simple histogram of the data
hist(mc_moran21$res, breaks = 50)

# extract the residuals from the MC simulation output above
mc_moran_df21 <- as.data.frame(mc_moran21$res)
# change the column name to something easier to write
colnames(mc_moran_df21) <- "mcresiduals"
# plot MC residuals and compare against our Moran
ggplot(mc_moran_df21, aes(x = mcresiduals)) +
  geom_histogram(colour = "gray1", fill = "lightblue3",
                 alpha = 0.7, binwidth = 0.01) +
  geom_vline(xintercept = lima_moran21$estimate[1],
             colour = "red", linetype = "dashed") +
  xlab("Monte Carlo Residuals") +
  ylab("Count") +
  ggtitle("2021", ) +
  theme(plot.title = element_text(hjust = 0.5))

# We can extract the pseudo p-value from the simulation object we already made
mc_moran21

# Local Analysis 2021 ---------------------------------------------------------------

# Calculate the local Moran statistic for each region using queen contiguity
local_moran2021 <- localmoran(master2021$logitp,
                              weights_queen, # our weights object
                              zero.policy = TRUE,
                              alternative = "two.sided")
# Replace the column names to make it clearer
colnames(local_moran2021) <- c("local_I", "expected_I",
                               "variance_I", "z_statistic", "p_value")
head(local_moran2021)

# Join Local Moran's I information back on to the spatial data for plotting
local_moran2021 <- cbind(master2021, local_moran2021) %>%
  dplyr::select(scaled_incidence, lag_scaled_incidence, p_value, geometry)


# plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_moran2021) +
  tm_polygons(col = "p_value", title = "Local Moran 2021",
              style = "fixed", breaks = c(1*10^-20, 0.05, 0.1, 1),
              lwd = 0.2, border.col = 1, 
              palette = "-Accent") +
  tm_layout(legend.position = c("right", "top"), legend.outside = TRUE)

# Read in the function for analysis
source("r_functions/map_maker.R")
source("r_functions/map_maker005.R")
# Join our local moran outputs to the main data and select columns for analysis
local_map2021 <- master2021 %>%
  cbind(., local_moran2021) %>% 
  dplyr::select(scaled_incidence, lag_scaled_incidence, p_value, geometry)
# Run the map_maker function
local_map2021 <- map_maker(local_map2021)
local_map2021 <- map_maker005(local_map2021)
# Plot the number of each cluster type
table(local_map2021$cluster)
# Plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_map2021) +
  tm_polygons(col = "cluster",
              style = "fixed",
              palette = "-Set3",
              lwd = 0.2,
              border.col = 1) +
  tm_layout(legend.position = c(".1", ".5"),
            legend.outside = TRUE,# Legend outside the plot
            title = "LISA 2021", # Directly setting the plot title
            title.size = 1, # Adjusting the title size
            title.fontfamily = "fixed") # Adjusting the 
# Sensative analysis with p < .005
tm_shape(local_map2021) +
  tm_polygons(col = "cluster",
              title = "Clusters",
              style = "fixed",
              palette = "-Set3", # Corrected palette specification
              lwd = 0.2,
              border.col = 1) +
  tm_layout(legend.position = c(".1", ".5"),
            legend.outside = TRUE,# Legend outside the plot
            title = "LISA 2021", # Directly setting the plot title
            title.size = 1, # Adjusting the title size
            title.fontfamily = "fixed")  +
  tm_credits("Sensitivity Analysis",
             position = c("right", "top"),
             just = "bottom",
             size = 0.8, # Adjust the size as needed
             fontfamily = "fixed")

##########################################################################################
# Start on 2022
tm_shape(master2022) +
  tm_polygons(col = "total_incidence",
              title = "Dengue Incidence Rate",
              style = "quantile",
              palette = "viridis",
              alpha = 0.7,
              lwd = 0.2,
              n = 4,
              border.col = 1,
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

# Plotting data attributes: ASMR histogram
ggplot(master2022, aes(x = total_incidence)) +
  geom_histogram(colour = "gray1",
                 fill = "blue",
                 alpha = 0.7,
                 binwidth = 5) +
  theme_bw() # this step just makes things look nice.

# Changing the data into the logscale
master2022$logitp <- log((master2022$total_incidence + 0.5)/(master2022$Population - master2022$total_incidence + 0.5))
hist(master2022$logitp, xlab = "", main = "Empirical logit",
     border = "black", col = "darkgreen")

# Define which regions are neighbors to one another based on queen contiguity
neighbours <- poly2nb(master2022$geometry)
# Summarise results
neighbours

# Visualize the neighbor matrix that I just made
lima_centroids22 <- master2022 %>%
  st_centroid()
plot(master2022$geometry, lwd = 0.2)
plot(neighbours, master2022$geometry,
     col = 'red', cex = 0.1, lwd = 1, add = TRUE)

# Update the neighbour pattern to a row standardised weights matrix.
weights_queen <- nb2listw(neighbours, style = "W", zero.policy = TRUE)

# compare neighbour pattern defined by queen contiguity with...
weights_queen$neighbours
# ...the row standardised weights (only the first 5 rows ).
weights_queen$weights[1:5]

# complete a Global Moran's I test
lima_moran22 <- moran.test(master2022$logitp, listw = weights_queen, zero.policy = T)
lima_moran22

# Plot a Moran's Scatter plot using scaled data
# scale the dengue incidence parameter
master2022$scaled_incidence <- scale(master2022$logitp)
# create a lag vector from the neighbor list and the scaled dengue incidence values
master2022$lag_scaled_incidence <- lag.listw(weights_queen,
                                             master2022$scaled_incidence, zero.policy =  T)
# plot the output
ggplot(data = master2022, aes(x = scaled_incidence, y = lag_scaled_incidence)) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", linetype = "dashed") +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab("Dengue Incidence (scaled)") +
  ylab("Incidence lag vector") +
  ggtitle("2022", ) +
  theme(plot.title = element_text(hjust = 0.5))

# Simulate the dengue incidence lima data with a random distribution over the Lima region 10000 times
mc_moran22 <- moran.mc(master2022$logitp, listw = weights_queen,
                     nsim = 10000, zero.policy = T)
# Plot a simple histogram of the data
hist(mc_moran22$res, breaks = 50)

# extract the residuals from the MC simulation output above
mc_moran_df22 <- as.data.frame(mc_moran22$res)
# change the column name to something easier to write
colnames(mc_moran_df22) <- "mcresiduals"
# plot MC residuals and compare against our Moran
ggplot(mc_moran_df22, aes(x = mcresiduals)) +
  geom_histogram(colour = "gray1", fill = "seagreen3",
                 alpha = 0.7, binwidth = 0.01) +
  geom_vline(xintercept = lima_moran22$estimate[1],
             colour = "red", linetype = "dashed") +
  xlab("Monte Carlo Residuals") +
  ylab("Count") +
  ggtitle("2022", ) +
  theme(plot.title = element_text(hjust = 0.5))

# We can extract the pseudo p-value from the simulation object we already made
mc_moran22

# Local Analysis 2022 ---------------------------------------------------------------

# Calculate the local Moran statistic for each region using queen contiguity
local_moran2022 <- localmoran(master2022$logitp,
                              weights_queen, # our weights object
                              zero.policy = TRUE,
                              alternative = "two.sided")
# Replace the column names to make it clearer
colnames(local_moran2022) <- c("local_I", "expected_I",
                               "variance_I", "z_statistic", "p_value")
head(local_moran2022)

# Join Local Moran's I information back on to the spatial data for plotting
local_moran2022 <- cbind(master2022, local_moran2022) %>%
  dplyr::select(scaled_incidence, lag_scaled_incidence, p_value, geometry)

# plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_moran2022) +
  tm_polygons(col = "p_value", title = "Local Moran 2022",
              style = "fixed", breaks = c(1*10^-20, 0.05, 0.1, 1),
              lwd = 0.2, border.col = 1, 
              palette = "-Accent") +
  tm_layout(legend.position = c("right", "top"), legend.outside = TRUE)

# Read in the function for analysis
source("r_functions/map_maker.R")
# Join our local moran outputs to the main data and select columns for analysis
local_map2022 <- master2022 %>%
  cbind(., local_moran2022) %>% 
  dplyr::select(scaled_incidence, lag_scaled_incidence, p_value, geometry)
# Run the map_maker function
local_map2022 <- map_maker(local_map2022)
local_map2022 <- map_maker005(local_map2022)
# Plot the number of each cluster type
table(local_map2022$cluster)
# Plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_map2022) +
  tm_polygons(col = "cluster",
              style = "fixed",
              palette = "-Set3",
              lwd = 0.2,
              border.col = 1) +
  tm_layout(legend.position = c(".1", ".5"),
            legend.outside = TRUE,# Legend outside the plot
            title = "LISA 2022", # Directly setting the plot title
            title.size = 1, # Adjusting the title size
            title.fontfamily = "fixed") # Adjusting the 
# Sensative analysis with p < .005
tm_shape(local_map2022) +
  tm_polygons(col = "cluster",
              title = "Clusters",
              style = "fixed",
              palette = "-Set3", # Corrected palette specification
              lwd = 0.2,
              border.col = 1) +
  tm_layout(legend.position = c(".1", ".5"),
            legend.outside = TRUE,# Legend outside the plot
            title = "LISA 2022", # Directly setting the plot title
            title.size = 1, # Adjusting the title size
            title.fontfamily = "fixed")  +
  tm_credits("Sensitivity Analysis",
             position = c("right", "top"),
             just = "bottom",
             size = 0.8, # Adjust the size as needed
             fontfamily = "fixed")


###################################################################################

##########################################################################################
# 2019-2022 Morans I analysis
tm_shape(allmaster) +
  tm_polygons(col = "total_incidence",
              title = "Dengue Incidence Rate",
              style = "quantile",
              palette = "viridis",
              alpha = 0.7,
              lwd = 0.2,
              n = 4,
              border.col = 1,
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

# Plotting data attributes: total dengue incidense histogram
ggplot(allmaster, aes(x = total_incidence)) +
  geom_histogram(colour = "gray1",
                 fill = "blue",
                 alpha = 0.7,
                 binwidth = 5) +
  theme_bw() # this step just makes things look nice.

# Changing the data into the logscale
allmaster$logitp <- log((allmaster$total_incidence + 0.5)/(allmaster$Population - allmaster$total_incidence + 0.5))
hist(allmaster$logitp, xlab = "", main = "Empirical logit",
     border = "black", col = "darkgreen")

# Define which regions are neighbors to one another based on queen contiguity
neighbours <- poly2nb(allmaster$geometry)
# Summarise results
neighbours

# Visualize the neighbor matrix that I just made
lima_centroids <- allmaster %>%
  st_centroid()
plot(allmaster$geometry, lwd = 0.2)
plot(neighbours, allmaster$geometry,
     col = 'red', cex = 0.1, lwd = 1, add = TRUE)

# Update the neighbour pattern to a row standardised weights matrix.
weights_queen <- nb2listw(neighbours, style = "W", zero.policy = T)

# compare neighbour pattern defined by queen contiguity with...
weights_queen$neighbours
# ...the row standardised weights (only the first 5 rows ).
weights_queen$weights[1:5]

# complete a Global Moran's I test
lima_moranall <- moran.test(allmaster$logitp, listw = weights_queen, zero.policy = T)
lima_moranall

# Plot a Moran's Scatter plot using scaled data
# scale the dengue incidence parameter
allmaster$scaled_incidence <- scale(allmaster$logitp)
# create a lag vector from the neighbor list and the scaled incidence values
allmaster$lag_scaled_incidence <- lag.listw(weights_queen,
                                             allmaster$scaled_incidence, zero.policy =  T)
# plot the output
ggplot(data = allmaster, aes(x = scaled_incidence, y = lag_scaled_incidence)) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", linetype = "dashed") +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab("Raw Incidence (scaled)") +
  ylab("Incidence lag vector") +
  xlab("Dengue Incidence (scaled)") +
  ylab("Incidence lag vector") +
  ggtitle("2019-2022", ) +
  theme(plot.title = element_text(hjust = 0.5))

# Simulate the 4 year incidence data with a random distribution over the lima 10000 times
mc_moran_all <- moran.mc(allmaster$logitp, listw = weights_queen,
                       nsim = 10000, zero.policy = T)
# Plot a simple histogram of the data
hist(mc_moran_all$res, breaks = 50)

# extract the residuals from the MC simulation output above
mc_moran_df_all <- as.data.frame(mc_moran_all$res)
# change the column name to something easier to write
colnames(mc_moran_df_all) <- "mcresiduals"
# plot MC residuals and compare against pur Moran
ggplot(mc_moran_df_all, aes(x = mcresiduals)) +
  geom_histogram(colour = "gray1", fill = "coral",
                 alpha = 0.7, binwidth = 0.01) +
  geom_vline(xintercept = lima_moranall$estimate[1],
             colour = "red", linetype = "dashed") +
  xlab("Monte Carlo Residuals") +
  ylab("Count") +
  ggtitle("2019 - 2022", ) +
  theme(plot.title = element_text(hjust = 0.5))

# We can extract the pseudo p-value from the simulation object we already made
mc_moran_all

# Local Analysis 2022 ---------------------------------------------------------------

# Calculate the local Moran statistic for each region using queen contiguity
local_moran_all <- localmoran(allmaster$logitp,
                              weights_queen, # our weights object
                              zero.policy = TRUE,
                              alternative = "two.sided")
# Replace the column names to make it clearer
colnames(local_moran_all) <- c("local_I", "expected_I",
                               "variance_I", "z_statistic", "p_value")
head(local_moran_all)

# Join Local Moran's I information back on to the spatial data for plotting
local_moran_all <- cbind(allmaster, local_moran_all) %>%
  dplyr::select(scaled_incidence, lag_scaled_incidence, p_value, geometry)

# plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_moran_all) +
  tm_polygons(col = "p_value", title = "Local Moran 2019-2022",
              style = "fixed", breaks = c(1*10^-20, 0.05, 0.1, 1),
              lwd = 0.2, border.col = 1, 
              palette = "-Accent") +
  tm_layout(legend.position = c("right", "top"), legend.outside = TRUE)

# Read in the function for analysis
source("r_functions/map_maker005.R")
# Join our local moran outputs to the main data and select columns for analysis
local_map_all <- allmaster %>%
  cbind(., local_moran_all) %>% 
  dplyr::select(scaled_incidence, lag_scaled_incidence, p_value, geometry)
# Run the map_maker function
local_map_all <- map_maker(local_map_all)
local_map_all <- map_maker005(local_map_all)
# Plot the number of each cluster type
table(local_map_all$cluster)
# Plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_map_all) +
  tm_polygons(col = "cluster",
              title = "Clusters",
              style = "fixed",
              palette = "-Set3", # Corrected palette specification
              lwd = 0.2,
              border.col = 1) +
  tm_layout(legend.position = c(".1", ".5"),
            legend.outside = TRUE,# Legend outside the plot
            title = "LISA 2019-2022", # Directly setting the plot title
            title.size = 1, # Adjusting the title size
            title.fontfamily = "fixed") # Adjusting the title font family

# Sensative analysis with p < .005
tm_shape(local_map_all) +
  tm_polygons(col = "cluster",
              title = "Clusters",
              style = "fixed",
              palette = "-Set3", # Corrected palette specification
              lwd = 0.2,
              border.col = 1) +
  tm_layout(legend.position = c(".1", ".5"),
            legend.outside = TRUE, # Legend outside the plot
            title = "LISA 2019-2022", # Directly setting the plot title
            title.size = 1, # Adjusting the title size
            title.fontfamily = "fixed") +
  tm_credits("Sensitivity Analysis",
             position = c("right", "top"),
             just = "bottom",
             size = 0.8, # Adjust the size as needed
             fontfamily = "fixed")



# Non Spatial Analysis----------------------------------------------------------------------------------------------------------

# Get the mean and variance of the incidence data
mean_count <- mean(master$IncidenceCount)
variance_count <- var(master$IncidenceCount)

# Check what the mean and variane were
cat("Mean:", mean_count, "\n")
cat("Variance:", variance_count, "\n")

# check if its overdispersed or not
if (variance_count > mean_count) {
  print("Overdispersion is present.")
} else {
  print("No overdispersion detected.")
}

# Fit the Poisson model
poisson_model <- glm(IncidenceCount ~ Vegetation, data = master, family = poisson())

# Fit the Negative Binomial model
negbin_model <- glm.nb(IncidenceCount ~ Vegetation, data = master)

lrtest(poisson_model, negbin_model)
#  Fit both Poisson and Negative Binomial models to your data and perform a Likelihood Ratio Test. A significant p-value indicates that the Negative Binomial model provides a better fit

# Start looking at the univariate effects on incidence 
install.packages("aod")
library(aod)

# Build My Model
modelbase <- glm.nb(IncidenceCount ~ Precipitation + Temperature + Relative_Humidity,
                data = master)

summary(modelbase)
#****************************************************************************************************************
#Examples of code used in the loop farther down:
# #Keeping Precipitation Constant
P0T0H1 <- glm.nb(IncidenceCount ~ Precipitation + Temperature + lag1_Relative_Humidity,
                      data = master)
summary(P0T0H1)
# Normal lag 1 temp
P0T1H0 <- glm.nb(IncidenceCount ~ Precipitation + lag1_Temperature + Relative_Humidity,
                 data = master)
summary(p0T1H0)

# All Lag 1
P1L1H1 <- glm.nb(IncidenceCount ~ lag1_Precipitation + lag1_Temperature + lag1_Relative_Humidity,
                    data = master)
summary(P1L1H1)

#____________________________________________
# Create a loop to run all the models at once

# Initialize vectors for P, T, and H
P <- 0:3
T <- 0:3
H <- 0:3

# Create a data frame to store the combinations
combinations <- expand.grid(P = P, T = T, H = H)

# Combine the columns into the desired format
combinations$Combination <- paste0("P", combinations$P, "T", combinations$T, "H", combinations$H)

# Print the combinations
print(combinations$Combination)

combinations <- c("P0T0H0", "P1T0H0", "P2T0H0", "P3T0H0",
                  "P0T1H0", "P1T1H0", "P2T1H0", "P3T1H0",
                  "P0T2H0", "P1T2H0", "P2T2H0", "P3T2H0",
                  "P0T3H0", "P1T3H0", "P2T3H0", "P3T3H0",
                  "P0T0H1", "P1T0H1", "P2T0H1", "P3T0H1",
                  "P0T1H1", "P1T1H1", "P2T1H1", "P3T1H1",
                  "P0T2H1", "P1T2H1", "P2T2H1", "P3T2H1",
                  "P0T3H1", "P1T3H1", "P2T3H1", "P3T3H1",
                  "P0T0H2", "P1T0H2", "P2T0H2", "P3T0H2",
                  "P0T1H2", "P1T1H2", "P2T1H2", "P3T1H2",
                  "P2T2H2", "P3T2H2", "P0T3H2", "P1T3H2", "P2T3H2", "P3T3H2",
                  "P0T0H3", "P1T0H3", "P2T0H3", "P3T0H3",
                  "P0T1H3", "P1T0H3", "P2T1H3", "P3T1H3",
                  "P0T2H3", "P1T2H3", "P2T2H3", "P3T2H3",
                  "P0T3H3", "P1T3H3", "P2T3H3", "P3T3H3")


# Define the lags for each variable
lags <- expand.grid(Precipitation = 0:3, Temperature = 0:3, Relative_Humidity = 0:3)

# Initialize an empty list to hold the models
models <- list()

# Loop through each combination of lags to create all the different models at once
for(i in 1:nrow(lags)) {
  # Generate the model formula based on the current row of lags
  formula <- as.formula(paste0("IncidenceCount ~ lag", lags$Precipitation[i], "_Precipitation + lag", 
                              lags$Temperature[i], "_Temperature + lag", lags$Relative_Humidity[i], 
                              "_Relative_Humidity"))
  
  # Fit the model
  model <- glm.nb(formula, data = lag_model_master)
  
  # Name the model based on the lag combination
  modelName <- paste0("P", lags$Precipitation[i], "T", lags$Temperature[i], "H", lags$Relative_Humidity[i])
  assign(modelName, model)
  
  # Add the model to the list
  models[[modelName]] <- model
}

# Example: Accessing a model printout
summary(models[['P0T0H0']])
# get all the AIC in one list
for(mod in 1:64) { print(c(mod, names(models)[mod], models[[mod]]$aic))}
for(mod in 1:64) { print(c(mod, names(models)[mod], models[[mod]]$deviance))}


#"P0T1H2" has the lowest AIC 1141.66
modelbase <- glm.nb(IncidenceCount ~ lag0_Precipitation + lag1_Temperature + lag2_Relative_Humidity + Population,
                    data = lag_model_master)
summary(modelbase)

modeltest <- glm.nb(IncidenceCount ~ lag0_Precipitation + lag1_Temperature + lag2_Relative_Humidity
                   + Overcrowd + Pop_No_Hygeine + Population, data = lag_model_master)
summary(modeltest)
# Vegetation dropped things down to 1138.1
# Overcrowd dropped to 1136.1
# The two above combined was 1136.5
# Pop_No_Hygeine dropped things to 1136.3
# No Hygeine and Overcrowd brought down to 1135
# NoCellPhone dropped things to 1128.7
# That plus no hygeine and overcrowd dropped to 1125.1
# Difficulty moving dropped things down to 1139.9
# Keep cellphones in as my research online shows that Peru has used cellphones in their healthcare prevention methods towards dengue

#Current Winner

model <- glm.nb(IncidenceCount ~ lag0_Precipitation + lag1_Temperature + lag2_Relative_Humidity +
                  Elevation + NoCellPhone + offset(log(Population))
                , data = lag_model_master)
summary(model)


# Extract the residuals from
# the fitted model object
residuals <- residuals(model, type = "pearson")

# Calculate fitted values
fitted_values <- predict(model, type = "response")

# Create a data frame for plotting
plot_data <- data.frame(Fitted = fitted_values, Residuals = residuals)

# Plot residuals vs. fitted values
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() + # Scatterplot of residuals
  geom_hline(yintercept = 0, linetype = "dashed") + # Horizontal line at y=0
  labs(x = "Fitted Values", y = "Pearson Residuals",
       title = "Residuals vs Fitted Values") +
  theme_minimal()

#############################################################################################################################################
# Zero Inflated model
library(pscl)

#Create and try my model through this changing out and slowly adding
# Environmental Model
env_model <- zeroinfl(IncidenceCount ~ lag0_Precipitation + lag1_Temperature + lag2_Relative_Humidity +
                        Wind_Speed + offset(log(Population)) 
                       | Elevation + lag1_Temperature + Wind_Speed + lag2_Relative_Humidity
                       , data = lag_model_master, dist = "negbin")
summary(env_model)
AIC(env_model)

# Final Model
zinb_model <- zeroinfl(IncidenceCount ~ lag0_Precipitation + lag1_Temperature + lag2_Relative_Humidity
                       + Wind_Speed + Pop_Density + Pop_No_Hygeine + Illiteracy_Rate + offset(log(Population))
                       | Elevation + lag2_Relative_Humidity + Wind_Speed + lag1_Temperature + Pop_Density
                       , data = lag_model_master, dist = "negbin")
summary(zinb_model)
AIC(zinb_model)


logLik(zinb_model)  # This gives you the log-likelihood
length(coef(zinb_model)) + 1  # Number of parameters (including dispersion parameter)
k <- length(coef(zinb_model)) + 1
logL <- logLik(zinb_model)
AIC <- 2 * k - 2 * logL
AIC


lrtest(env_model, zinb_model)
car::vif(zinb_model)

# Scale some of the independent variables to make them fit better within my model if needed
lag_model_master$Pop_No_Hygeine <- scale(lag_model_master$Pop_No_Hygeine)
lag_model_master$No_public_H2O <- scale(lag_model_master$No_public_H2O)
lag_model_master$Illiteracy_Rate <- scale(lag_model_master$Illiteracy_Rate)
lag_model_master$NoInternet <- scale(lag_model_master$NoInternet)
lag_model_master$Overcrowd <- scale(lag_model_master$Overcrowd)
lag_model_master$Inadq_House <- scale(lag_model_master$Inadq_House)
lag_model_master$Econ_Depend <- scale(lag_model_master$Econ_Depend)
lag_model_master$NoComputer <- scale(lag_model_master$NoComputer)
lag_model_master$OneRoom <- scale(lag_model_master$OneRoom)
lag_model_master$Pop_Density <- scale(lag_model_master$Pop_Density)
lag_model_master$Pop_1NBI <- scale(lag_model_master$Pop_1NBI)
lag_model_master$NoCellPhone <- scale(lag_model_master$NoCellPhone)



# Change chart to represent Rate Ratios
options(scipen = 999)

coef_count <- coef(summary(zinb_model))$count
coef_zero <- coef(summary(zinb_model))$zero

# Convert to data frames for easier manipulation
coef_count_df <- as.data.frame(coef_count)
coef_zero_df <- as.data.frame(coef_zero)

# Calculate rate ratios and confidence intervals for the count model
coef_count_df$rate_ratio <- exp(coef_count_df$Estimate)
coef_count_df$conf.low <- exp(coef_count_df$Estimate - 1.96 * coef_count_df$`Std. Error`)
coef_count_df$conf.high <- exp(coef_count_df$Estimate + 1.96 * coef_count_df$`Std. Error`)

# Calculate rate ratios and confidence intervals for the zero-inflation model
coef_zero_df$rate_ratio <- exp(coef_zero_df$Estimate)
coef_zero_df$conf.low <- exp(coef_zero_df$Estimate - 1.96 * coef_zero_df$`Std. Error`)
coef_zero_df$conf.high <- exp(coef_zero_df$Estimate + 1.96 * coef_zero_df$`Std. Error`)

# Print the results
print(coef_count_df)
print(coef_zero_df)


# Extract the residuals from final model
# the fitted model object
residuals <- residuals(zinb_model, type = "pearson")

# Calculate fitted values
fitted_values <- predict(zinb_model, type = "response")

# Create a data frame for plotting
plot_data <- data.frame(Fitted = fitted_values, Residuals = residuals)

# Plot residuals vs. fitted values
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() + # Scatterplot of residuals
  geom_hline(yintercept = 0, linetype = "dashed") + # Horizontal line at y=0
  labs(x = "Fitted Values", y = "Pearson Residuals",
       title = "Residuals vs Fitted Values") +
  theme_minimal()

# Looking for co-linearity by using a correlation matrix
df_numeric <- st_drop_geometry(lag_model_master)
df_numeric <- select_if(df_numeric, is.numeric)

numeric_vars <- df_numeric[, sapply(df_numeric, is.numeric)]
numeric_vars <- numeric_vars[, !names(numeric_vars) %in% "IncidenceCount"]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

# Install and load corrplot package if not already installed
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix, method = "circle")
corrplot(cor_matrix, method = "ellipse")


# Print out my results from my model
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)

setwd("~/LSHTM_23/Thesis/Lima_Dengue/04_Plots")
#Print out my tables
# Create HTML table
html_table <- kable(coef_count_df, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Save to HTML file
write(html_table, file = "zinb_count_df.html")

# Repeat for the second dataframe
html_table2 <- kable(coef_zero_df, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

write(html_table2, file = "zinb_zero_df.html")