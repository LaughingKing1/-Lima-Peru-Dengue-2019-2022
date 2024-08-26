# Junk Code I used but evetually didnt need. 
# Fixing Demographic Data Types
head(Messy_Demographics)

#Get rid of all the commas
# Define the columns to be excluded
exclude_columns <- c("Province", "Department", "District")

# Select all columns except the ones to be excluded
selected_columns <- setdiff(names(Messy_Demographics), exclude_columns)

# Apply the operation to the selected columns

for(col in selected_columns) {
  Messy_Demographics[[col]] <- as.numeric(gsub("[^0-9]", "", Messy_Demographics[[col]]))
}

# Set all the columns as integers except for the admin values
Messy_Demographics <- Messy_Demographics %>% mutate_at(c("%_Pop_1_NBI", "%_Inadq_Housing",  "%_Overcrowding",
                                                         "%_Pop_No_Hygeine", "%_High_Econ_Depend", "%_Pop_No_pubwater",
                                                         "%_Homes_Dirt_Floor", "%_Homes_Firewood", "%_No_Light",
                                                         "%_no_internet", "%_Homes_1_room", "%_no_computer",
                                                         "%_Difficulty_Moving", "%_limited_interact_wothes",
                                                         "Illiteracy_Rate"), as.numeric)