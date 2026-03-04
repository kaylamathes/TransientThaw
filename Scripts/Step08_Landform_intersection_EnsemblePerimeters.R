library(sf)
library(dplyr)
library(ggplot2)
library(zip)
library(data.table)
library(parallel)

options(scipen = 999)

##Load Data 
#Perimeter Data
polygons = st_read("/Users/kmathes/Desktop/FSPro_Runs/2010_TreatIsland_valid.shp")%>%
  st_transform(crs = 3338)

# ##Find the NA valid fires 
# polygons_notvalid <- polygons_notvalid%>%
#   mutate(Valid = as.character(st_is_valid(.)))
# 
# ##Delete the NA valid fires 
# polygons <- polygons_notvalid%>%
#   filter(Valid == "TRUE")

  

# Load the Landform point csvs 
Upland  <- read.csv("Data/Counterfactual_landform_classification/TreatIsland_Upland.csv")
  
# Convert points to an sf object with the same CRS
Upland_sf <- st_as_sf(Upland, coords = c("lon", "lat"), crs = 4326)



# --- 2. Create Clipping Polygon (Buffer) ---
# Define a Projected CRS (e.g., UTM zone 15N, EPSG:32615) for accurate metric buffer
# We must project before buffering!
target_crs <- 32615

# Project both datasets to the metric CRS
Upland_sf_proj <- st_transform(Upland_sf, target_crs)

# Load the Landform point csv 
Lowland  <- read.csv("Data/Counterfactual_landform_classification/TreatIsland_Lowland.csv")

# Convert points to an sf object with the same CRS
Lowland_sf <- st_as_sf(Lowland, coords = c("lon", "lat"), crs = 4326)

# --- 2. Create Clipping Polygon (Buffer) ---
# Define a Projected CRS (e.g., UTM zone 15N, EPSG:32615) for accurate metric buffer
# We must project before buffering!
target_crs <- 32615

# Project both datasets to the metric CRS
Lowland_sf_proj <- st_transform(Lowland_sf, target_crs)

# Create a 30-meter buffer around the points and dissolve/union them.
# The 'dist = 30' is now correctly interpreted as 30 meters.
Lowland_buffer <- st_buffer(Lowland_sf_proj, dist = 30) %>%
  st_union() %>%
  st_as_sf()

Upland_buffer <- st_buffer(Upland_sf_proj, dist = 30)%>%
st_union() %>%
st_as_sf()

#Upland_buffer_wgs84 <- st_transform(Upland_buffer, 4326)

#st_write(Upland_buffer_wgs84, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Data/Counterfactual_landform/DiscoveryCreek_Upland_Buffer_union.shp",  driver  = "ESRI Shapefile", append = FALSE)


polygons_proj <- st_transform(polygons, target_crs)

###### 1000
polygons_proj_sub <- polygons_proj%>%
  filter(FIRENUMBER >= 0 & FIRENUMBER < 1000)

# --- 3. Clip the Target Polygon ---
# Perform the intersection using the projected layers

###Upland
clipped_Upland_proj_1000 <- st_intersection(polygons_proj_sub, Upland_buffer)

# (Optional) Re-project back to WGS84 for mapping or storage
clipped_Upland_wgs84_1000 <- st_transform(clipped_Upland_proj_1000, 4326)

clipped_Upland_wgs84_1000<- clipped_Upland_wgs84_1000%>%
  mutate(Upland_area_m2 = st_area(.))%>%
  mutate(Upland_area_acres = Upland_area_m2*0.000247105)


st_write(clipped_Upland_wgs84_1000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_1000.gpkg", driver = "GPKG") 


##Lowland

clipped_Lowland_proj_1000 <- st_intersection(polygons_proj_sub, Lowland_buffer)

clipped_Lowland_wgs84_1000 <- st_transform(clipped_Lowland_proj_1000, 4326)

clipped_Lowland_wgs84_1000 <- clipped_Lowland_wgs84_1000%>%
  mutate(Lowland_area_m2 = st_area(.))%>%
  mutate(Lowland_area_acres = Lowland_area_m2*0.000247105)

st_write(clipped_Lowland_wgs84_1000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_1000.gpkg", driver = "GPKG") 


####2000
polygons_proj_sub_2000 <- polygons_proj%>%
  filter(FIRENUMBER >= 1000 & FIRENUMBER < 2000)

# --- 3. Clip the Target Polygon ---
# Perform the intersection using the projected layers

###Upland
clipped_Upland_proj_2000 <- st_intersection(polygons_proj_sub_2000, Upland_buffer)

# (Optional) Re-project back to WGS84 for mapping or storage
clipped_Upland_wgs84_2000 <- st_transform(clipped_Upland_proj_2000, 4326)

clipped_Upland_wgs84_2000<- clipped_Upland_wgs84_2000%>%
  mutate(Upland_area_m2 = st_area(.))%>%
  mutate(Upland_area_acres = Upland_area_m2*0.000247105)


st_write(clipped_Upland_wgs84_2000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_2000.gpkg", driver = "GPKG") 


##Lowland
clipped_Lowland_proj_2000 <- st_intersection(polygons_proj_sub_2000, Lowland_buffer)

clipped_Lowland_wgs84_2000 <- st_transform(clipped_Lowland_proj_2000, 4326)

clipped_Lowland_wgs84_2000 <- clipped_Lowland_wgs84_2000%>%
  mutate(Lowland_area_m2 = st_area(.))%>%
  mutate(Lowland_area_acres = Lowland_area_m2*0.000247105)

st_write(clipped_Lowland_wgs84_2000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_2000.gpkg", driver = "GPKG") 



####3000
polygons_proj_sub_3000 <- polygons_proj%>%
  filter(FIRENUMBER >= 2000 & FIRENUMBER < 3000)

# --- 3. Clip the Target Polygon ---
# Perform the intersection using the projected layers

###Upland
clipped_Upland_proj_3000 <- st_intersection(polygons_proj_sub_3000, Upland_buffer)

# (Optional) Re-project back to WGS84 for mapping or storage
clipped_Upland_wgs84_3000 <- st_transform(clipped_Upland_proj_3000, 4326)

clipped_Upland_wgs84_3000<- clipped_Upland_wgs84_3000%>%
  mutate(Upland_area_m2 = st_area(.))%>%
  mutate(Upland_area_acres = Upland_area_m2*0.000247105)


st_write(clipped_Upland_wgs84_3000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_3000.gpkg", driver = "GPKG") 


##Lowland

clipped_Lowland_proj_3000 <- st_intersection(polygons_proj_sub_3000, Lowland_buffer)

clipped_Lowland_wgs84_3000 <- st_transform(clipped_Lowland_proj_3000, 4326)

clipped_Lowland_wgs84_3000 <- clipped_Lowland_wgs84_3000%>%
  mutate(Lowland_area_m2 = st_area(.))%>%
  mutate(Lowland_area_acres = Lowland_area_m2*0.000247105)

st_write(clipped_Lowland_wgs84_3000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_3000.gpkg", driver = "GPKG") 


####4000
polygons_proj_sub_4000 <- polygons_proj%>%
  filter(FIRENUMBER >= 3000 & FIRENUMBER < 4000)

# --- 3. Clip the Target Polygon ---
# Perform the intersection using the projected layers

###Upland
clipped_Upland_proj_4000 <- st_intersection(polygons_proj_sub_4000, Upland_buffer)

# (Optional) Re-project back to WGS84 for mapping or storage
clipped_Upland_wgs84_4000 <- st_transform(clipped_Upland_proj_4000, 4326)

clipped_Upland_wgs84_4000 <- clipped_Upland_wgs84_4000%>%
  mutate(Upland_area_m2 = st_area(.))%>%
  mutate(Upland_area_acres = Upland_area_m2*0.000247105)


st_write(clipped_Upland_wgs84_4000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_4000.gpkg", driver = "GPKG") 


##Lowland

clipped_Lowland_proj_4000 <- st_intersection(polygons_proj_sub_4000, Lowland_buffer)

clipped_Lowland_wgs84_4000 <- st_transform(clipped_Lowland_proj_4000, 4326)

clipped_Lowland_wgs84_4000 <- clipped_Lowland_wgs84_4000%>%
  mutate(Lowland_area_m2 = st_area(.))%>%
  mutate(Lowland_area_acres = Lowland_area_m2*0.000247105)

st_write(clipped_Lowland_wgs84_4000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_4000.gpkg", driver = "GPKG") 


####5000
polygons_proj_sub_5000 <- polygons_proj%>%
  filter(FIRENUMBER >= 4000 & FIRENUMBER < 5000)

# --- 3. Clip the Target Polygon ---
# Perform the intersection using the projected layers

###Upland
clipped_Upland_proj_5000 <- st_intersection(polygons_proj_sub_5000, Upland_buffer)

# (Optional) Re-project back to WGS84 for mapping or storage
clipped_Upland_wgs84_5000 <- st_transform(clipped_Upland_proj_5000, 4326)

clipped_Upland_wgs84_5000<- clipped_Upland_wgs84_5000%>%
  mutate(Upland_area_m2 = st_area(.))%>%
  mutate(Upland_area_acres = Upland_area_m2*0.000247105)


st_write(clipped_Upland_wgs84_5000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_5000.gpkg", driver = "GPKG") 


##Lowland

clipped_Lowland_proj_5000 <- st_intersection(polygons_proj_sub_5000, Lowland_buffer)

clipped_Lowland_wgs84_5000 <- st_transform(clipped_Lowland_proj_5000, 4326)

clipped_Lowland_wgs84_5000 <- clipped_Lowland_wgs84_5000%>%
  mutate(Lowland_area_m2 = st_area(.))%>%
  mutate(Lowland_area_acres = Lowland_area_m2*0.000247105)

st_write(clipped_Lowland_wgs84_5000, "Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_5000.gpkg", driver = "GPKG") 






library(sf)
library(dplyr)
options(scipen=999)



clipped_Upland_wgs84_1000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_1000.gpkg")
clipped_Upland_wgs84_2000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_2000.gpkg")
clipped_Upland_wgs84_3000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_3000.gpkg")
clipped_Upland_wgs84_4000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_4000.gpkg")
clipped_Upland_wgs84_5000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Upland_5000.gpkg")

clipped_Lowland_wgs84_1000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_1000.gpkg")
clipped_Lowland_wgs84_2000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_2000.gpkg")
clipped_Lowland_wgs84_3000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_3000.gpkg")
clipped_Lowland_wgs84_4000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_4000.gpkg")
clipped_Lowland_wgs84_5000 <- st_read("Data/Counterfactual_landform/Partial_shapefiles/TreatIsland_Lowland_5000.gpkg")





clipped_Upland_wgs84 <- rbind(clipped_Upland_wgs84_1000,clipped_Upland_wgs84_2000, clipped_Upland_wgs84_3000, clipped_Upland_wgs84_4000,clipped_Upland_wgs84_5000)
clipped_Lowland_wgs84 <- rbind(clipped_Lowland_wgs84_1000,clipped_Lowland_wgs84_2000, clipped_Lowland_wgs84_3000, clipped_Lowland_wgs84_4000,clipped_Lowland_wgs84_5000)


###### Creating an option with areas totals for upland vs lowland as columns for each ensemble perimeter


clipped_Lowland_wgs84$Lowland_area_m2 <- as.numeric(clipped_Lowland_wgs84$Lowland_area_m2)
clipped_Lowland_wgs84$Lowland_area_acres <- as.numeric(clipped_Lowland_wgs84$Lowland_area_acres)


clipped_Upland_wgs84$Upland_area_m2 <- as.numeric(clipped_Upland_wgs84$Upland_area_m2)
clipped_Upland_wgs84$Upland_area_acres <- as.numeric(clipped_Upland_wgs84$Upland_area_acres)


###Write the shapefile as a geopackage 
#st_write(clipped_Lowland_wgs84, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Data/Counterfactual_landform/clipped_Lowland_TreatIsland.gpkg", driver = "GPKG") 

#st_write(clipped_Upland_wgs84, "/Users/kmathes/Desktop/PermafrostThaw/TransientThaw/Data/Counterfactual_landform/clipped_Upland_TwomileLake.gpkg", driver = "GPKG") 



############ Check with mapping the clipped perimeters 
### Use a subset of polygons to map 

# polygons_sub <- polygons%>%
#   filter(FIRENUMBER < 50)
# 
# clipped_Upland_wgs84_sub <- clipped_Upland_wgs84%>%
#   filter(FIRENUMBER < 50)
# 
# clipped_Lowland_wgs84_sub <- clipped_Lowland_wgs84%>%
#   filter(FIRENUMBER < 50)
# 
# ggplot() +
#  #geom_sf(data = polygons_sub, color = "black")+
#  geom_sf(data = clipped_Upland_wgs84_sub, fill = "#FFEE8C",color = "#FFEE8C")+
#  geom_sf(data = clipped_Lowland_wgs84_sub, fill = "#4fb9af", color = "#4fb9af")
# 


##Create a combined CSV file with both the areas of upland and lowland burn areas for each perimeter. Remove the geometry variable 
clipped_Upland_wgs84_csv <- clipped_Upland_wgs84%>%
  st_drop_geometry()

clipped_Lowland_wgs84_csv <- clipped_Lowland_wgs84%>%
  st_drop_geometry()


clipped_landform_wgs84_csv <- full_join(clipped_Upland_wgs84_csv, clipped_Lowland_wgs84_csv , by = c("FIRENUMBER", "SIZE_ACRES", "Valid"))

clipped_landform_wgs84_csv[is.na(clipped_landform_wgs84_csv)] <- 0

clipped_landform_wgs84_csv <- clipped_landform_wgs84_csv%>%
 mutate(total = Lowland_area_acres + Upland_area_acres)


###Adjusting the upland and lowland acres to reflect the correct total acres 
clipped_landform_wgs84_csv <- clipped_landform_wgs84_csv%>%
  mutate(Upland_ratio = Upland_area_acres/total)%>%
  mutate(Upland_area_acres_adjust  = SIZE_ACRES*Upland_ratio)%>%
 mutate(Lowland_area_acres_adjust  = SIZE_ACRES*(1-Upland_ratio))
  
  

###Write the CSV file 

write.csv(clipped_landform_wgs84_csv,"Data/Counterfactual_landform/Landform_TreatIsland.csv")













###Old Scripts 
####################### Clip the final files so they are not too big 
clipped_Upland_wgs84_1 <- clipped_Upland_wgs84%>%
  filter(FIRENUMBER < 2500)

clipped_Upland_wgs84_2 <- clipped_Upland_wgs84%>%
  filter(FIRENUMBER >= 2500)

clipped_Lowland_wgs84_1 <- clipped_Lowland_wgs84%>%
  filter(FIRENUMBER < 2500)

clipped_Lowland_wgs84_2 <- clipped_Lowland_wgs84%>%
  filter(FIRENUMBER >= 2500)



##########################Save the shapefiles as zip files to upload to the Cloud bucket 
 # 1. Define the output directory and file name
output_dir <- "/Users/kmathes/Desktop/FSPro_Runs/2012_BrooksCreek/"
shapefile_name <- "BrooksCreek_Lowland_EnsemblePerimeters_2"
full_path_no_ext <- file.path(output_dir, shapefile_name)
zip_path <- file.path(output_dir, paste0(shapefile_name, ".zip"))

# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 2. Write the shapefile using st_write
# The 'driver = "ESRI Shapefile"' is often automatically guessed from the file extension
st_write(clipped_Lowland_wgs84_2, dsn = paste0(full_path_no_ext, ".shp"), driver = "ESRI Shapefile", delete_layer = TRUE)

# 3. Identify all files associated with the shapefile (e.g., .shp, .shx, .dbf, .prj)
shapefile_files <- list.files(output_dir, pattern = paste0("^", shapefile_name, "\\."), full.names = TRUE)

# 4. Compress the files into a single ZIP archive
# Use the 'zip' package for straightforward zipping
zip::zip(
  zipfile = zip_path,
  files = shapefile_files,
  mode = "cherry-pick"
)

#Remove the individual shapefile components after zipping
if (file.exists(zip_path)) {
  file.remove(shapefile_files)
  cat(sprintf("Shapefile successfully written and compressed to %s\n", zip_path))
}



############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################






############# Trying out the Intersection with burn depth to make sure the upland vs lowland ensemble perimeters are functional
landform <- st_read("/Users/kmathes/Desktop/FSPro_Runs/2009_Discovery Creek/DiscoveryCreek_Upland_EnsemblePerimeters.shp")%>%
  st_transform(crs = 3338)

# Load libraries
library(sf)
library(parallel)
library(tidyverse)
library(data.table)
library(ggplot2)

Burn_depth  <- read.csv("/Users/kmathes/Desktop/Combustion Model/Counterfactual/Output/2009_DiscoveryCreek/DiscoveryCreek_BurnDepthRetrained.csv")%>%
  dplyr::select(lat, lon, prediction, ID)%>%
  rename(depth_prediction=prediction)

##convert to sf 
Burn_depth_sf <- st_as_sf(Burn_depth, coords = c("lon", "lat"), crs = 4326)
points <- st_transform(Burn_depth_sf, crs = 3338)


batch_size <- 100000
batches <- split(points, ceiling(seq_len(nrow(points)) / batch_size))

landform_clip <- landform%>%
  filter(FIRENUMBER < 100)


results <- lapply(batches, function(batch) {
  st_join(batch, landform_clip, join = st_intersects)
})

final_result <- rbindlist(results)

summary <- final_result %>%
  st_drop_geometry() %>%  # Drop geometry for faster summary
  group_by(SIZE_ACRES, FIRENUMBER) %>%
  summarize(
    mean_depth_prediction = mean(depth_prediction, na.rm = TRUE),
    min_depth_prediction = min(depth_prediction, na.rm = TRUE),
    max_depth_prediction = max(depth_prediction, na.rm = TRUE),
   # .groups = 'drop'
  )%>%
  drop_na()


summary_geometry <- merge(summary, landform_clip, by = c("FIRENUMBER", "SIZE_ACRES"))

