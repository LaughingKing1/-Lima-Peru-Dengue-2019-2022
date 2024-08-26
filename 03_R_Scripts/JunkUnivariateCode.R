# Unused single covariate code 

# Precipitation ***
rain_nbn <- glm.nb(IncidenceCount ~ Precipitation,
                   data = master)

summary(rain_nbn)
exp(coef(rain_nbn))
exp(confint(rain_nbn))
wald.test(b = coef(rain_nbn), Sigma = vcov(rain_nbn), Terms = 2)

IncidenceCount <- master$IncidenceCount
Precipitation <- master$Precipitation

# Calculate Spearman correlation
spearman_rain <- cor(IncidenceCount, Precipitation, method = "spearman")

# Print the result
print(spearman_rain)

# Humidity
humid_nbn <- glm.nb(IncidenceCount ~ Relative_Humidity,
                    data = master)

summary(humid_nbn)
exp(coef(humid_nbn))
exp(confint(humid_nbn))

Humidity <- master$Relative_Humidity

# Calculate Spearman correlation
spearman_humid <- cor(IncidenceCount, Humidity, method = "spearman")

# Print the result
print(spearman_humid)


# Temperature **
temp_nbn <- glm.nb(IncidenceCount ~ Temperature,
                   data = master)
summary(temp_nbn)
exp(coef(temp_nbn))
exp(confint(temp_nbn))

temp <- master$Temperature

# Calculate Spearman correlation
spearman_temp <- cor(IncidenceCount, temp, method = "spearman")

# Print the result
print(spearman_temp)

#Wind Speed
windsp_nbn <- glm.nb(IncidenceCount ~ Wind_Speed,
                     data = master)
summary(windsp_nbn)
exp(coef(windsp_nbn))
exp(confint(windsp_nbn))



#Wind Direction *
winddr_nbn <- glm.nb(IncidenceCount ~ Wind_Direction_Degrees,
                     data = master)
summary(winddr_nbn)
exp(coef(winddr_nbn))
exp(confint(winddr_nbn))


#Vegitation 
veg_nbn <- glm.nb(IncidenceCount ~ Vegetation,
                  data = master)
summary(veg_nbn)
exp(coef(veg_nbn))
exp(confint(veg_nbn))

# Population Density
popden_nbn <- glm.nb(IncidenceCount ~ Pop_Density,
                     data = master)
summary(popden_nbn)
exp(coef(popden_nbn))
exp(confint(popden_nbn))


# Elevation 
elevation_nbn <- glm.nb(IncidenceCount ~ Elevation,
                        data = master)
summary(elevation_nbn)
exp(coef(elevation_nbn))
exp(confint(elevation_nbn))

# Pop 1 NBI
NBI_nbn <- glm.nb(IncidenceCount ~ Pop_1NBI,
                  data = master)
summary(NBI_nbn)
exp(coef(NBI_nbn))
exp(confint(NBI_nbn))

# Inadiquate Housing
inad_house_nbn <- glm.nb(IncidenceCount ~ Inadq_House,
                         data = master)
summary(inad_house_nbn)
exp(coef(inad_house_nbn))
exp(confint(inad_house_nbn))


# Overcrowd
overcrowd_nbn <- glm.nb(IncidenceCount ~ Overcrowd,
                        data = master)
summary(overcrowd_nbn)
exp(coef(overcrowd_nbn))
exp(confint(overcrowd_nbn))


# No Hygeine 
nohyg_nbn <- glm.nb(IncidenceCount ~ Pop_No_Hygeine,
                    data = master)
summary(nohyg_nbn)
exp(coef(nohyg_nbn))
exp(confint(nohyg_nbn))

# Econ Dependency
econdep_nbn <- glm.nb(IncidenceCount ~ Econ_Depend,
                      data = master)
summary(econdep_nbn)
exp(coef(econdep_nbn))
exp(confint(econdep_nbn))


# NO Public Water *
nowater_nbn <- glm.nb(IncidenceCount ~ No_public_H2O,
                      data = master)
summary(nowater_nbn)
exp(coef(nowater_nbn))
exp(confint(nowater_nbn))


#No Lights
nolight_nbn <- glm.nb(IncidenceCount ~ NoLights,
                      data = master)
summary(nolight_nbn)
exp(coef(nolight_nbn))
exp(confint(nolight_nbn))


#Dirt Floors
dirt_nbn <- glm.nb(IncidenceCount ~ DirtFloor,
                   data = master)
summary(dirt_nbn)
exp(coef(dirt_nbn))
exp(confint(dirt_nbn))


# firewood
fire_nbn <- glm.nb(IncidenceCount ~ UseFirewood,
                   data = master)
summary(fire_nbn)
exp(coef(fire_nbn))
exp(confint(fire_nbn))


#No Computer
computer_nbn <- glm.nb(IncidenceCount ~ NoComputer,
                       data = master)
summary(computer_nbn)
exp(coef(computer_nbn))
exp(confint(computer_nbn))

#No Cellphone
nocell_nbn <- glm.nb(IncidenceCount ~ NoCellPhone,
                     data = master)
summary(nocell_nbn)
exp(coef(nocell_nbn))
exp(confint(nocell_nbn))

#No Internet
internet_nbn <- glm.nb(IncidenceCount ~ NoInternet,
                       data = master)
summary(internet_nbn)
exp(coef(internet_nbn))
exp(confint(internet_nbn))

# One Room
oneroom_nbn <- glm.nb(IncidenceCount ~ OneRoom,
                      data = master)
summary(oneroom_nbn)
exp(coef(oneroom_nbn))
exp(confint(oneroom_nbn))


# Dificulty Moving
diffmov_nbn <- glm.nb(IncidenceCount ~ Diff_Mov,
                      data = master)
summary(diffmov_nbn)
exp(coef(diffmov_nbn))
exp(confint(diffmov_nbn))


#  Limited Interactions with Others
limit_nbn <- glm.nb(IncidenceCount ~ LimitedInter,
                    data = master)
summary(limit_nbn)
exp(coef(limit_nbn))

# Illiterate
illiterate_nbn <- glm.nb(IncidenceCount ~ Illiteracy_Rate,
                         data = master)
summary(illiterate_nbn)
exp(coef(illiterate_nbn))
exp(confint(illiterate_nbn))
