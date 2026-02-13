###Take the predicted ALD for each day of year for each tsf year and convert that to volume of carbon available for decomposition

##library
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(raster)
library(dplyr)
library(whitebox)
library(tmap)
library(terra)


##Upload the carbon storage estimates 

##Carbon storage 0-50
carbon_100 <- terra::rast("/Users/kmathes/Desktop/DATA/palmtag-2022-spatial-1/upscaled-SOC-storage-0-100-cm.tif")

##Transform the CRS of the raster file 
ext <- ext(carbon_100)
max_x <- xmax(ext)
carbon_100 <- terra::project(carbon_100, "epsg:4326")
plot(carbon_100)

##Write the new raster file
writeRaster(carbon_100, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Output/palmtag_SOC_1_100.tif", overwrite = TRUE) 

##Carbon storage 0-100
carbon_100 <- terra::rast("/Users/kmathes/Desktop/DATA/palmtag-2022-spatial-1/upscaled-SOC-storage-0-100-cm.tif")

##Transform the CRS of the raster file 
ext <- ext(carbon_100)
max_x <- xmax(ext)
carbon_100 <- terra::project(carbon_100, "epsg:4326")
plot(carbon_100)

##Write the new raster file
writeRaster(carbon_100, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Output/palmtag_SOC_1_100.tif", overwrite = TRUE) 



##Upload the carbon storage estimates 
##Carbon 100 - 200
carbon_200 <- terra::rast("/Users/kmathes/Desktop/DATA/palmtag-2022-spatial-1/upscaled-SOC-storage-100-200-cm.tif")

##Transform the CRS of the raster file 
ext <- ext(carbon_200)
max_x <- xmax(ext)
carbon_200 <- terra::project(carbon_200, "epsg:4326")
plot(carbon_200)

##Write the new raster file
writeRaster(carbon_200, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Output/palmtag_SOC_100_200.tif", overwrite = TRUE) 



##Upload the carbon storage estimates 
##Carbon 200 - 300
carbon_300 <- terra::rast("/Users/kmathes/Desktop/DATA/palmtag-2022-spatial-1/upscaled-SOC-storage-200-300-cm.tif")

##Transform the CRS of the raster file 
ext <- ext(carbon_300)
max_x <- xmax(ext)
carbon_300 <- terra::project(carbon_300, "epsg:4326")
plot(carbon_300)

##Write the new raster file
writeRaster(carbon_300, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Output/palmtag_SOC_200_300.tif", overwrite = TRUE) 


burn_points <- read_csv("/Users/kmathes/Desktop/PermafrostThaw/Output/Step06_CarbonStorage_LittleMosquito.csv")





###Old Script ###

##Upload the fire perimeter 
fire_perimeter <- st_read("/Users/kmathes/Desktop/Combustion Model/Counterfactual/Output/2006_ChloyaLakes/Perimeter_ChloyaLakes.shp","Perimeter_ChloyaLakes")%>%
  st_transform(crs = 4326)


carbon_100_crop <- crop(carbon_100, fire_perimeter)

plot(carbon_100_crop)


##Measure for the real fire perimeters fires 

#Data

YF_realPerimeters <- st_read("/Users/kmathes/Desktop/Combustion Model/YF_RealPerimeters/OutputYFFires/YFFiresPerimeters.shp","YFFiresPerimeters")%>%
  st_transform(crs = 3338)%>%
  
YF_realPerimeters  <- YF_realPerimeters%>%
  filter(NAME != "Chahalie") %>%
  filter(NAME != "Medicine Lake")


Alaska_soil_carbon_v2 <- st_read("/Users/kmathes/Desktop/DATA/NCSCDv2_regional_polygons_WGS84/NCSCDv2_Alaska_WGS84.shp")%>%
  st_transform(crs = 3338)

predicted_ALD <- read.csv("/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Output/predicted_ALD_allDoy.csv")

##Select only the variables of interest 
socc <- Alaska_soil_carbon_v2%>%
  dplyr::select(SOCC_100cm, SOCC_200,SOCC_300)

##Find the intersection of the real fire perimeters and the SOCC data layers at 100, 200, 300 

Fire_perimeters_socc <- st_intersection(YF_realPerimeters, socc)

##Add the three burned depth increments (1,2,3m) for the total kgC/m2 up to 3m 

Fire_perimeters_socc <- Fire_perimeters_socc%>%
  mutate(SOCC_total3m_kgC_m2 = SOCC_100cm + SOCC_200 + SOCC_300)
  
##Convert from KgC/m2 to Kg/m3 by multiplying by 1/3m 
Fire_perimeters_socc <- Fire_perimeters_socc%>%
  mutate(SOCC_total3m_kgC_m3 = SOCC_total3m_kgC_m2/3)

##Find the total amount of carbon in the burned area up to 3m 
Fire_perimeters_socc <- Fire_perimeters_socc%>%
  mutate(Burned_area_to_3m_m3 = brnr_m2*3)%>%
  mutate(Total_Carbon_pool_kgC = Burned_area_to_3m_m3*SOCC_total3m_kgC_m3)

###Add the daily change in ALD depth, convert to m, create a volume of thawed permafrost (m3) by multiplying the change in active layer depth to the burned area, 
##Calculate the amount of carbon in the thawed permafrost pool available for respiration 
Fire_perimeters_socc_predicted_ALD_allDoy <- predicted_ALD%>%
  group_by(tsf, Predicted_Doy)%>%
  merge(., Fire_perimeters_socc)%>%
  mutate(predicted_ALD_change_m = predicted_ALD_change/100)%>%
  mutate(Volume_thawed_permafrost_m3 = predicted_ALD_change_m*brnr_m2)%>%
  mutate(fire_thawed_carbon_kgC = Volume_thawed_permafrost_m3*SOCC_total3m_kgC_m3)%>%
  ungroup()


###Annual fire thaw carbon pool 

Fire_perimeters_socc_predicted_ALD_annual <- Fire_perimeters_socc_predicted_ALD_allDoy%>%
  group_by(tsf, NAME)%>%
  summarize(fire_thawed_carbon_kgC_annual = sum(fire_thawed_carbon_kgC))%>%
  mutate(fire_thawed_carbon_MgC_annual = fire_thawed_carbon_kgC_annual/1000)


Fire_perimeters_socc_predicted_ALD_annual_MartenCreek <- Fire_perimeters_socc_predicted_ALD_annual%>%
  filter(NAME == "Marten Creek")

Fire_perimeters_socc_predicted_ALD_annual_Mosquito <- Fire_perimeters_socc_predicted_ALD_annual%>%
  filter(NAME == "Little Mosquito")

ggplot(Fire_perimeters_socc_predicted_ALD_annual_MartenCreek, aes(x = tsf, y = fire_thawed_carbon_MgC_annual)) +
  geom_point() + xlab("Years Since Fire") + ylab("Annual Fire Thawed Carbon Pool (MgC)") + ggtitle("Marten Creek Real Fire Perimeter")

ggsave("TransientThaw/Output/MartenCreekThawedCarbonPoolbyyear.png")
