###updating Step 09 
###Upland Only 
library(tidyverse)
library(sf)
library(ggplot2)

options(scipen = 999)


################# This whole process is separated by landform ###################

#####For Upland we need to subtract the talik Area 

####Volume of daily thawed permafrost 
####Step 1: fire perimeter (m2) Upload the counterfactual upland parts of the ensemble perimeters (Upland vs lowland) and 

###########DATA Upload (THIS DOES CHANGE)
Upland_perimeter <- read.csv("Data/Counterfactual_landform/Landform_BrooksCreek.csv")%>%
  dplyr::select(FIRENUMBER,SIZE_ACRES,Upland_area_acres_adjust)%>%
  rename(Upland_perimeter_acres = Upland_area_acres_adjust)%>%
  mutate(Upland_perimeter_m2 = Upland_perimeter_acres*4046.86)

###########DATA Upload (THIS DOES CHANGE)
Talik_perimeter <- read.csv("/Users/kmathes/Desktop/ProgressiveThaw/Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BrooksCreek_medium.csv")
######################

Upland_perimeter <- merge(Upland_perimeter, Talik_perimeter, by = c("FIRENUMBER", "SIZE_ACRES"))%>%
  mutate(Upland_perimeter_m2_noTalik = Upland_perimeter_m2 - talik_area_m2)%>%
  dplyr::select(FIRENUMBER,Upland_perimeter_m2_noTalik )
######################

#### Step 2: Upload the percent change daily fire-induced change in active layer percent for 50 years post-fire 

########Data upload (THIS DOESN"T CHANGE): Removing year 0 since start of fire because it does not make sense 
ALD_daily_change_upland <- read.csv("Output/predicted_ALD_allDoy_upland_percentchange.csv")
###############


ALD_daily_change_upland_sub <- ALD_daily_change_upland%>%
  dplyr::select(tsf,Predicted_Doy,predicted_ALD_change)%>%
  filter(tsf !=0)

#### Step 3: Find the daily change in baseline ALT for the ensemble perimters
#### Add the growing season days: 100 - 271 and the non-linear relationship
### Add the first day of ALD which is 0 

################DATA UPLOAD (THIS DOES CHANGE)
ALT_baseline <- read.csv("Data/Counterfactual_ALT_Baseline/ALT_counterfactual_Intersections/BrooksCreek_ALT_Baseline_Intersection.csv")%>%
  dplyr::select(FIRENUMBER, SIZE_ACRES, mean)%>%
  rename(ALT_baseline_last = mean)%>%
  mutate(ALT_baseline_first = 0)%>%
  mutate(Predicted_DOY_max = 271)%>%
  mutate(Predicted_DOY_min = 100)%>%
  mutate(ALT_baseline_last_cm = ALT_baseline_last*100)
##################

##Add time since fire columns (This will be the same for all years as the baseline) 
##add a list of the tsf 
tsf <- c(1:50)

ALT_baseline_tsf <- ALT_baseline%>%
  expand(FIRENUMBER, full_seq(tsf,1))%>%
  rename(tsf = "full_seq(tsf, 1)")

##Join that list of tsf across the counterfactual perimeter 

ALT_baseline_tsf <- left_join(ALT_baseline_tsf, ALT_baseline, by = c("FIRENUMBER"))
  

#### Step 4: Calculate the daily tragetory of ALD for each counterfactual perimeter using:
#### a (quadratic term that is separated by part in the time series: I got these values from R script Step 05)
 a_0_10_mean = -0.00247
 a_13_49_mean = -0.000846
 a_mean = -0.001658


##Find "b", "c", parameters (the quadratic parameter: use the average across the tsf that we do have: cannot predict this shape of the parabula for each other year without more Doy data)
 ALT_baseline_tsf <- ALT_baseline_tsf%>%
  mutate(a = a_mean)%>%
  group_by(tsf)%>%
  mutate(b = ((ALT_baseline_last_cm - ALT_baseline_first - a*((Predicted_DOY_max^2) - (Predicted_DOY_min^2)))/(Predicted_DOY_max - Predicted_DOY_min)))%>%
  mutate(c = ALT_baseline_last_cm-(b*Predicted_DOY_max)-(a*Predicted_DOY_max^2))%>%
  ungroup()

##Create a list of all the DOY we want to predict the change in ALD 
Predicted_DOY_list <- c(100:271)

ALT_baseline_tsf_allDOY <- ALT_baseline_tsf%>%
  expand(tsf,FIRENUMBER, full_seq(Predicted_DOY_list,1))

##Join that list of DOY across all tsf 
ALT_baseline_tsf_allDOY <- left_join(ALT_baseline_tsf_allDOY,  ALT_baseline_tsf, by = c("tsf", "FIRENUMBER"))%>%
  rename(Predicted_Doy = "full_seq(Predicted_DOY_list, 1)")

##Now predict the ALD for every doy for each tsf 
ALT_baseline_tsf_allDOY <- ALT_baseline_tsf_allDOY%>%
  mutate(daily_ALD_change = (a*(Predicted_Doy^2)) + (b*Predicted_Doy) + c)

ALT_baseline_tsf_allDOY_1 <- ALT_baseline_tsf_allDOY%>%
  filter(FIRENUMBER == 500)

ggplot(ALT_baseline_tsf_allDOY_1, aes(x = Predicted_Doy, y = daily_ALD_change))+
  geom_point() +
  facet_wrap(~tsf)


#### Step 04: Calculate the fire-thawed daily ALT thickness 

##Change the negative percent change to zero 
ALD_daily_change_upland_sub <- ALD_daily_change_upland_sub%>%
  mutate(predicted_ALD_percentchange_noneg = case_when(predicted_ALD_change < 0 ~ 0, 
                                                       predicted_ALD_change >= 0 ~ predicted_ALD_change))

###Combined the percent fire-induced change with the ALT baseline for each fire 
ALT_daily_fire_change <- ALT_baseline_tsf_allDOY%>%
  group_by(FIRENUMBER)%>%
  left_join(., ALD_daily_change_upland_sub, by = c("tsf", "Predicted_Doy"))

##Calculate the daily fire-induced change in ALT 
ALT_daily_fire_change <- ALT_daily_fire_change%>%
  mutate(Daily_fire_change_cm = (predicted_ALD_percentchange_noneg*daily_ALD_change)/100)%>%
  mutate(Daily_fire_change_m = Daily_fire_change_cm/100)


##Step 05: Calculate the Daily ALT volume (m3)
ALT_daily_fire_change_volume <- left_join(ALT_daily_fire_change, Upland_perimeter, by = c("FIRENUMBER"))


ALT_daily_fire_change_volume <-  ALT_daily_fire_change_volume%>%
  mutate(Daily_ALT_change_volume_m3 = Upland_perimeter_m2_noTalik*Daily_fire_change_m)


##### Daily Carbon pool vulnerable to emissions 

##Step 1: Upload the Carbon Density data (Start with the top 100 cm)

###########DATA Upload (THIS DOES CHANGE)
CarbonDensity <- read.csv("/Users/kmathes/Desktop/ProgressiveThaw/Output/Counterfactual_CarbonDensity/BrooksCreek_CarbonDensityIntersectionTotal.csv")%>%
  dplyr::select(FIRENUMBER, carbon_100)
  
######################

Daily_carbon_pool_kg = left_join(ALT_daily_fire_change_volume,CarbonDensity, by = ("FIRENUMBER"))

Daily_carbon_pool_kg <- Daily_carbon_pool_kg%>%
  mutate(Daily_carbon_pool_kg = Daily_ALT_change_volume_m3*carbon_100)

Daily_carbon_pool_kg_test <- Daily_carbon_pool_kg%>%
  filter(FIRENUMBER == 200)%>%
  filter(tsf == 1)%>%
  mutate(DailyCarbonLossPercent = 0.011627907)%>%
  mutate(DailyCarbonLoss_kg = (DailyCarbonLossPercent*Daily_carbon_pool_kg)/100)

#######Step 2: Upload the Gerrevink data 
Gerrevink <- read.csv("/Users/kmathes/Desktop/ProgressiveThaw/Data/CarbonDensity/Gerrevink_percentcarbonloss.csv")

###We are going to start by split up the percent annual loss into a daily percent based on the number of frost-free days (or the growing season), which is 172 to start (This might need to change)

Gerrevink <- Gerrevink%>%
  mutate(DailyPercent = percentCarbonLoss_Mineral/172)%>%
  filter(YearsSinceFire_or_ThawInitation != 50)%>%
  mutate(tsf = 1:50 )


Daily_carbon_pool_kg_withloss <- left_join(Daily_carbon_pool_kg, Gerrevink, by = c("tsf"))

##Calculate the daily carbon Loss 

Daily_carbon_pool_kg_withloss <- Daily_carbon_pool_kg_withloss%>%
  mutate(DailyCarbonLoss_kg = (DailyPercent*Daily_carbon_pool_kg)/100)

## Summarize to annual carbon loss 
Annual_carbon_pool_kg_withloss_summary <- Daily_carbon_pool_kg_withloss%>%
  group_by(FIRENUMBER, tsf)%>%
  summarize(AnnualCarbonLoss_kg = sum(DailyCarbonLoss_kg))

Annualcarbon_pool_kg_withloss_summary_test <- Daily_carbon_pool_kg_withloss_summary%>%
  filter(FIRENUMBER == 10)

## Summarize to total carbon loss over 50 years 

Total50yr_carbon_pool_kg_withloss_summary <- Daily_carbon_pool_kg_withloss%>%
  group_by(FIRENUMBER)%>%
  summarize(Total50yrCarbonLoss_kg = sum(DailyCarbonLoss_kg))

