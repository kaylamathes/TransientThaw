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

options(scipen = 999)

################# This whole process is separated by landform ###################

##################### UPLAND ###########################
####Step 1: Upload the burn points by landform (Upland vs lowland)

Upland_burnpoints <- read.csv("TransientThaw/Data/Counterfactual_landform_classification/BrooksCreek_Upland.csv")

###Step 2: Upload the carbon density burn points 

carbon_burnpoints <- read.csv("TransientThaw/Data/CarbonDensityBurnPoints/Palmtag_CarbonStorage_FullCounterfactualPerimeter_Step06_CarbonStorage_BrooksCreek.csv")%>%
  dplyr::select(!"system.index")

### Step 3: Upload the active layer thickness burn points 

### Step 4: Combine datasets (steps 1,2,3) into one (separated by landform: upland with carbon and ALT and lowland with carbon and ALT)
Upland_carbon_ALT <- left_join(Upland_burnpoints, carbon_burnpoints, by = c("lat", "lon", "ID","ID1"))%>%
  ungroup()

### Step 5: Find the AVERAGE carbon densities (all depths) and active layer thickness for upland and lowland burn points: Also include the max and min 
Upland_carbon_ALT_average <- Upland_carbon_ALT%>%
  group_by(ID)%>%
  summarise(Average_carbon_100 = mean(carbon_100, na.rm = TRUE),
         Average_carbon_200 = mean(carbon_200, na.rm = TRUE), 
         Average_carbon_300 = mean(carbon_300, na.rm = TRUE),
         Max_carbon_100 = max(carbon_100, na.rm = TRUE),
         Max_carbon_200 = max(carbon_200, na.rm = TRUE),
         Max_carbon_300 = max(carbon_300, na.rm = TRUE),
         Min_carbon_100 = min(carbon_100, na.rm = TRUE),
         Min_carbon_200 = min(carbon_200, na.rm = TRUE),
         Min_carbon_300 = min(carbon_300, na.rm = TRUE))
#### Step 6: Calculate the daily change in thaw depth using the average active layer thickness (separated by landform)

#### Step 7: Upload the daily fire-induced change in active layer percent for 50 years post-fire 
ALD_daily_change_upland <- read.csv("TransientThaw/Output/predicted_ALD_allDoy_upland_percentchange.csv")

ALD_daily_change_upland_sub <- ALD_daily_change_upland%>%
  dplyr::select(tsf,Predicted_Doy,predicted_ALD_change)

### Step 8: Merge the data layers from step 5, 6 and step 7 (the resulting dataframe should have the same number of columns as step 7)
ALD_daily_change_upland_sub_carbon_ALT <- ALD_daily_change_upland_sub%>%
  mutate(ID = Upland_carbon_ALT_average$ID,
         Average_carbon_100 = Upland_carbon_ALT_average$Average_carbon_100, 
         Average_carbon_200 = Upland_carbon_ALT_average$Average_carbon_200, 
         Average_carbon_300 = Upland_carbon_ALT_average$Average_carbon_300, 
         Max_carbon_100 = Upland_carbon_ALT_average$Max_carbon_100, 
         Max_carbon_200 = Upland_carbon_ALT_average$Max_carbon_200,
         Max_carbon_300 = Upland_carbon_ALT_average$Max_carbon_300)
         

### Step 9: Calculate the daily fire-induced change in thaw depth by multiplying the daily change in thaw depth with the percent change in daily thaw depth from fire 



##################### LOWLAND ###########################
####Step 1: Upload the burn points by landform (Upland vs lowland)

Lowland_burnpoints <- read.csv("TransientThaw/Data/Counterfactual_landform_classification/BrooksCreek_Lowland.csv")

###Step 2: Upload the carbon density burn points 

        ###Already Done 

### Step 3: Upload the active layer thickness burn points 

### Step 4: Combine datasets (steps 1,2,3) into one (separated by landform: upland with carbon and ALT and lowland with carbon and ALT)
Upland_carbon_ALT <- left_join(Lowland_burnpoints, carbon_burnpoints, by = c("lat", "lon", "ID","ID1"))%>%
  ungroup()

### Step 5: Find the AVERAGE carbon densities (all depths) and active layer thickness for upland and lowland burn points: Also include the max and min 
Lowland_carbon_ALT_average <- Lowland_carbon_ALT%>%
  group_by(ID)%>%
  summarise(Average_carbon_100 = mean(carbon_100, na.rm = TRUE),
            Average_carbon_200 = mean(carbon_200, na.rm = TRUE), 
            Average_carbon_300 = mean(carbon_300, na.rm = TRUE),
            Max_carbon_100 = max(carbon_100, na.rm = TRUE),
            Max_carbon_200 = max(carbon_200, na.rm = TRUE),
            Max_carbon_300 = max(carbon_300, na.rm = TRUE),
            Min_carbon_100 = min(carbon_100, na.rm = TRUE),
            Min_carbon_200 = min(carbon_200, na.rm = TRUE),
            Min_carbon_300 = min(carbon_300, na.rm = TRUE))
#### Step 6: Calculate the daily change in thaw depth using the average active layer thickness (separated by landform)

#### Step 7: Upload the daily fire-induced change in active layer percent for 50 years post-fire 
ALD_daily_change_Lowland <- read.csv("TransientThaw/Output/predicted_ALD_allDoy_Lowland_percentchange.csv")

ALD_daily_change_Lowland_sub <- ALD_daily_change_Lowland%>%
  dplyr::select(tsf,Predicted_Doy,predicted_ALD_change)

### Step 8: Merge the data layers from step 5, 6 and step 7 (the resulting dataframe should have the same number of columns as step 7)
ALD_daily_change_Lowland_sub_carbon_ALT <- ALD_daily_change_Lowland_sub%>%
  mutate(ID = Lowland_carbon_ALT_average$ID,
         Average_carbon_100 = Lowland_carbon_ALT_average$Average_carbon_100, 
         Average_carbon_200 = Lowland_carbon_ALT_average$Average_carbon_200, 
         Average_carbon_300 = Lowland_carbon_ALT_average$Average_carbon_300, 
         Max_carbon_100 = Lowland_carbon_ALT_average$Max_carbon_100, 
         Max_carbon_200 = Lowland_carbon_ALT_average$Max_carbon_200,
         Max_carbon_300 = Lowland_carbon_ALT_average$Max_carbon_300)


### Step 9: Calculate the daily fire-induced change in thaw depth by multiplying the daily change in thaw depth with the percent change in daily thaw depth from fire 

