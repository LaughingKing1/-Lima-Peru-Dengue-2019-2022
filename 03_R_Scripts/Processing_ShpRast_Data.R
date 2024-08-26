# Assessment
# Load in all the packages I might need
install.packages("pacman")
pckgs <- c("sf", "tidyverse", "readxl", 'tmap', 'rmapshaper',
           'janitor', 'leflet', 'terra', 'geodata', "dplyr", 
           "PrevMap", "mapview", "automap", "spdep", "spatialreg",
           "geoR", "ggplot2", 'gdistance', 'Hmisc', 'raster', "pals",
           "tmap", "readxl")
pacman::p_load (pckgs, character.only=TRUE)


library(raster)
library(sf)

# Set Working Directory
setwd("~/LSHTM_23/Thesis/Lima_Dengue")
# Load in Admin Boundaries
Peru_Bound <- st_read("00_Raw_Data/AminBoundaries/gadm41_PER_0.shp")
Lima_Department <- st_read("05_Maps/Lima_Department.shp")
Lima_Province <- st_read("05_Maps/Lima_Province.shp")
Lima_Districts <- st_read("05_Maps/Lima_Districts.shp")

# Plot Shapefiles for my area of interest for qc check
plot(Peru_Bound)
plot(Lima_Department$geometry)
plot(Lima_Province$geometry)
plot(Lima_Districts$geometry)

st_crs(Lima_Districts)
###############p####################################################################
# Clip river data just for my are of Peru
# Load in river data to see if it's relevant 
Lima_riv <- st_read("00_Raw_Data/HydroRIVERS_v10_sa_shp/HydroRIVERS_v10_sa.shp")

# Using "st_transform" first ensures the files have the same coordinate system (CRS)
Lima_riv <- Lima_riv %>%
  st_transform(st_crs(Lima_Province))

# Clip the shapefile by 'filtering' the dataset by the Lima Province outline
District_riv <- Lima_riv %>%
  st_filter(., Lima_Province)

# plot the clipped rivers data. It should be just Peruvian rivers.
plot(Lima_Districts$geometry,)
plot(District_riv$geometry, col = "blue", add = T)

st_write(District_riv, "01_Messy_Data/Lima_rivers.shp", append=FALSE)
##########################################################################################
# Load back in river data
Lima_riv <-  st_read("01_Messy_Data/Lima_rivers.shp")

# we build the layers on top of each other in tmap using the + sign
tmap_mode("plot")
# we build the layers on top of each other in tmap using the + sign
tm_shape(Lima_Districts$geometry) + tm_polygons() + tm_shape(Lima_riv) + 
  tm_lines(col = "blue")

#################################################################################################3
# Process elevation data to just the area I'm using
setwd("~/LSHTM_23/Thesis/Lima_Dengue/")
# Import my elevation data
South_peru_elevation <- rast("00_Raw_Data/SouthMedian30S090W_20101117_gmted_med075.tif")
plot(South_peru_elevation)

# create a shapefile with the extent of Lima districts as our template
Lima_proj <- Lima_Districts %>% st_transform(3857)
# note that 3857 is EPSG code for the projected coordinate system
# WGS84 Web Mercator (Auxiliary Sphere) a global projected coordinate system
# make the template a raster layer (called rasterisation)
# create raster layer with no values using the "rast" function
# we use our template shapefile "area_proj" to define the extent
#Create a raster out of lima districts shapefile
raster_area <- rast(Lima_proj) 
# specify 1000m resolution for the raster layer
res(raster_area) <- 1000
# make all values of the raster = 1
values(raster_area) <- 1
# Turn the polygon into a raster based upon the raster template
raster_area <- terra::rasterize(Lima_proj, raster_area)
# visualise our template raster with only 1s
tm_shape(raster_area) +
  tm_raster() +
  tm_layout(legend.outside = TRUE)

# change CRS of target raster (elevation) to match template extent (the "ext" object)

Elevation_districts <- project(South_peru_elevation, raster_area)

# crop matches extent of target raster "elevation_districs" to the "raster_area" template
Peru_Elevation_Crop <- crop(Elevation_districts, raster_area)
plot(Peru_Elevation_Crop)

# mask replaces cell values in our target "Peru_elevation_crop" with NAs (missing)
# this only occurs if they fall outside the boundary of our template "raster_area"
ElevPeru_Mask <- terra::mask(Peru_Elevation_Crop, raster_area)
plot(ElevPeru_Mask)

# resample gives the target raster the same resolution as the template (1km x 1km)
# when re-sampling you must choose a method to assign values to re-sampled cells.
# the method I used is "bilinear":
# new cells are the weighted distance average of the four nearest input cells.
# this method is useful for continuous data and smooth the data

LimaProvince_Elevation_res <- terra::resample(ElevPeru_Mask, raster_area, method= 'bilinear')
# display cropped, masked and resampled target raster
tm_shape(LimaProvince_Elevation_res) +
  tm_raster( title = "Elevation (m)", palette = "seq")  +
  tm_shape(Lima_Districts) +  # Add this line to include your shapefile
  tm_borders() +
  tm_scale_bar(position = c("left", "bottom")) +  # Add a scale bar
  tm_compass(position = c("right", "top")) +
  tm_layout(legend.position = c("right", "top"), legend.outside = TRUE)

tmaptools::palette_explorer()

# Check the metadata
LimaProvince_Elevation_res
setwd("01_Messy_Data/")
# save processed elevation data
writeRaster(LimaProvince_Elevation_res, "Lima_District_Elevation.tif", overwrite =TRUE)

#####################################################################################################################
#Example process I've pretty much used to  process the vegetation data for all 48 months but below ive streamlined it to do all files at once
setwd("~/LSHTM_23/Thesis/Lima_Dengue")

# import the vegetation files
veg2019_01 <- rast("00_Raw_Data/VegitationLima/MOD13C2.A2019001.061.2020286183227.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI.tif")
plot(veg2019_01)


# change CRS of target raster (vegetation) to match template extent (the "ext" object)
veg2019_01_districts <- project(veg2019_01, raster_area)

# crop matches extent of target raster "veg_districs" to the "raster_area" template
veg2019_01_Crop <- crop(veg2019_01_districts, raster_area)
plot(veg2019_01_Crop)

# mask replaces cell values in our target "Peru_elevation_crop" with NAs (missing)
# this only occurs if they fall outside the boundary of our template "raster_area"
veg2019_01_Mask <- terra::mask(veg2019_01_Crop, raster_area)
plot(veg2019_01_Mask)
# resample gives the target raster the same resolution as the template (1km x 1km)
# when re-sampling you must choose a method to assign values to re-sampled cells.
# the method I used is "bilinear":
# new cells are the weighted distance average of the four nearest input cells.
# this method is useful for continuous data and smooth the data

veg2019_01_res <- terra::resample(veg2019_01_Mask, raster_area, method= 'bilinear')
# display cropped, masked and resampled target raster
tm_shape(veg2019_01_res) +
  tm_raster() +
  tm_layout(legend.outside = TRUE)
# Check the metadata
veg2019_01_res
#####################################################################################################################
# Streamline this for all 48 raster files

file_directory <- "~/LSHTM_23/Thesis/Lima_Dengue/00_Raw_Data/VegitationLima"
Vegitation_files <- list.files(path = file_directory, pattern = "MOD13C2.*\\.tif$", full.names = TRUE)
Vegitation_files
getwd()

setwd("~/LSHTM_23/Thesis/Lima_Dengue")

library(terra) # Ensure terra package is loaded

for (j in 1:length(Vegitation_files)) {
  print(paste0("Start:", Vegitation_files[j])) # Print the current file being processed
  
  # Correctly read in the raster using the full path from Vegitation_files[j]
  i <- rast(Vegitation_files[j])

    # Project, crop, mask, and resample the raster as per your original logic
  i <- project(x=i, y=raster_area)
  i <- crop(i, raster_area)
  i <- terra::mask(i, raster_area)
  i <- terra::resample(i, raster_area, method = 'bilinear')
  
  # Construct the output filename correctly
  output_filename <- paste0("~/LSHTM_23/Thesis/Lima_Dengue/01_Messy_Data", "Per_", basename(Vegitation_files[j]))
  
  # Write the raster to the output file
  writeRaster(i, output_filename, overwrite=TRUE)
  
  print(paste0("Finish:", basename(Vegitation_files[j]))) # Print completion message
}
########################################################################################################################
# Check to make sure the data come out correct
test1 <- rast("01_Messy_Data/VegitationLimaPer_MOD13C2.A2019001.061.2020286183227.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI.tif")
tm_shape(test1) +
  tm_raster() +
  tm_layout(legend.outside = TRUE)
test1
test2 <- rast("01_Messy_Data/VegitationLimaPer_MOD13C2.A2019032.061.2020289005752.psgs_000502262911-CMG_0_05_Deg_Monthly_NDVI.tif")
tm_shape(test2) +
  tm_raster() +
  tm_layout(legend.outside = TRUE)
########################################################################################################################
#Process the population density Data
setwd("~/LSHTM_23/Thesis/Lima_Dengue/")
# Import my elevation data
Pop_Den19 <- rast("00_Raw_Data/per_populationdensity_2019_1km.tif")
plot(Pop_Den19)
Pop_Den20 <- rast("00_Raw_Data/per_populationdensity_2020_1km.tif")
plot(Pop_Den20)

# change CRS of target raster (elevation) to match template extent (the "ext" object)

Pop19_districts <- project(Pop_Den19, raster_area)
Pop20_districts <- project(Pop_Den20, raster_area)


# crop matches extent of target raster "elevation_districs" to the "raster_area" template
Pop19_Crop <- crop(Pop19_districts, raster_area)
plot(Pop19_Crop)
Pop20_Crop <- crop(Pop20_districts, raster_area)
plot(Pop20_Crop)

# mask replaces cell values in our target "Peru_elevation_crop" with NAs (missing)
# this only occurs if they fall outside the boundary of our template "raster_area"
Pop19_Mask <- terra::mask(Pop19_Crop, raster_area)
plot(Pop19_Mask)
Pop20_Mask <- terra::mask(Pop20_Crop, raster_area)
plot(Pop20_Mask)

# resample gives the target raster the same resolution as the template (1km x 1km)
# when re-sampling you must choose a method to assign values to re-sampled cells.
# the method I used is "bilinear":
# new cells are the weighted distance average of the four nearest input cells.
# this method is useful for continuous data and smooth the data

Pop19_res <- terra::resample(Pop19_Mask, raster_area, method= 'bilinear')
Pop20_res <- terra::resample(Pop20_Mask, raster_area, method= 'bilinear')

# display cropped, masked and resampled target raster
tm_shape(Pop19_res) +
  tm_raster() +
  tm_layout(legend.outside = TRUE)
tm_shape(Pop20_res) +
  tm_raster() +
  tm_layout(legend.outside = TRUE)
# Check the metadata
Pop19_res
Pop20_res
setwd("~/LSHTM_23/Thesis/Lima_Dengue/02_Cleaned_Data")
# save processed elevation data
writeRaster(Pop19_res, "Lima_PopDense19.tif", overwrite =TRUE)
writeRaster(Pop20_res, "Lima_PopDense20.tif", overwrite =TRUE)



