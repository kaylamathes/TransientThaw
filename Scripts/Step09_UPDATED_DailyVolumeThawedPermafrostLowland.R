###updating Step 09 
###Upland Only 
library(tidyverse)
library(sf)
library(ggplot2)

options(scipen = 999)


################# This whole process is separated by landform ###################



####Volume of daily thawed permafrost 
####Step 1: fire perimeter (m2) Upload the counterfactual upland parts of the ensemble perimeters (Upland vs lowland) and 

###########DATA Upload (THIS DOES CHANGE)
Lowland_perimeter <- read.csv("Data/Counterfactual_landform/Landform_BrooksCreek.csv")%>%
  dplyr::select(FIRENUMBER,SIZE_ACRES,Lowland_area_acres_adjust)%>%
  rename(Lowland_perimeter_acres = Lowland_area_acres_adjust)%>%
  mutate(Lowland_perimeter_m2 = Lowland_perimeter_acres*4046.86)
######################


#### Step 2: Upload the percent change daily fire-induced change in active layer percent for 50 years post-fire 

########Data upload (THIS DOESN"T CHANGE)
ALD_daily_change_lowland <- read.csv("Output/predicted_ALD_allDoy_lowland_percentchange.csv")
###############


ALD_daily_change_lowland_sub <- ALD_daily_change_lowland%>%
  dplyr::select(tsf,Predicted_Doy,predicted_ALD_change)

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
tsf <- c(0:50)

ALT_baseline_tsf <- ALT_baseline%>%
  expand(FIRENUMBER, full_seq(tsf,1))%>%
  rename(tsf = "full_seq(tsf, 1)")

##Join that list of tsf across the counterfactual perimeter 

ALT_baseline_tsf <- left_join(ALT_baseline_tsf, ALT_baseline, by = c("FIRENUMBER"))
  

#### Step 4: Calculate the daily tragetory of ALD for each counterfactual perimeter using:
#### a (quadratic term that is separated by part in the time series: I got these values from R script Step 05)
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
ALD_daily_change_lowland_sub <- ALD_daily_change_lowland_sub%>%
  mutate(predicted_ALD_percentchange_noneg = case_when(predicted_ALD_change < 0 ~ 0, 
                                                       predicted_ALD_change >= 0 ~ predicted_ALD_change))

###Combined the percent fire-induced change with the ALT baseline for each fire 
ALT_daily_fire_change <- ALT_baseline_tsf_allDOY%>%
  group_by(FIRENUMBER)%>%
  left_join(., ALD_daily_change_lowland_sub, by = c("tsf", "Predicted_Doy"))

##Calculate the daily fire-induced change in ALT 
ALT_daily_fire_change <- ALT_daily_fire_change%>%
  mutate(Daily_fire_change_cm = (predicted_ALD_percentchange_noneg*daily_ALD_change)/100)%>%
  mutate(Daily_fire_change_m = Daily_fire_change_cm/100)


##Step 05: Calculate the Daily ALT volume (m3)
ALT_daily_fire_change_volume <- left_join(ALT_daily_fire_change, Lowland_perimeter, by = c("FIRENUMBER"))


ALT_daily_fire_change_volume <-  ALT_daily_fire_change_volume%>%
  mutate(Daily_ALT_change_volume_m3 = Lowland_perimeter_m2*Daily_fire_change_m)




