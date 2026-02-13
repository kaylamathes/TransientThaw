### Separating the counterfactual burn points for each fire by landform type (upland vs lowland)

####library
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(raster)
library(dplyr)
library(whitebox)
library(tmap)
library(terra)

##Upload Data

landform =  list.files(path="/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Data/VunleLake/",pattern='*.csv', full.names = TRUE) %>%
  purrr::map(., read_csv)%>%
  map_dfr(., ~ .x %>%
            mutate(across(ID, as.character))) %>%
  bind_rows()%>%
  dplyr::select(!"system:index")%>%
  dplyr::select(!".geo")


landform <- landform%>%
  mutate(duplicate = duplicated(.$RID))

landform <- landform%>%
  filter(duplicate != TRUE)%>%
  dplyr::select(!duplicate)%>%
  dplyr::select(!RID)

landform_upland <- landform%>%
  filter(landform == "0")

landform_lowland <- landform%>%
  filter(landform == "1")

write_csv(landform_lowland, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Data/Counterfactual_landform_classification/VunleLake_Lowland.csv")
write_csv(landform_upland, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Data/Counterfactual_landform_classification/VunleLake_Upland.csv")


Upland_landform <- read.csv("/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Data/Counterfactual_landform_classification/DiscoveryCreek_Upland.csv")
Lowland_landform <- read.csv("/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Data/Counterfactual_landform_classification/DiscoveryCreek_Lowland.csv")

landform <- rbind(Upland_landform,Lowland_landform)

landform_sf <- st_as_sf(landform,
                      coords = c("lon", "lat"),
                      crs = 3338)











# Create an empty raster with desired extent, resolution, and CRS
# Adjust extent and resolution as needed for your data
template_raster <- rast(
  ext = ext(0, 6, 0, 6), # minx, maxx, miny, maxy
  res = 1, # resolution in x and y
  crs = "epsg:3338"
)

plot(template_raster)
