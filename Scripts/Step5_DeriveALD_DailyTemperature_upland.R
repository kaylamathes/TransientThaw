############# Derive daily thaw depths for all measurements from Talucci's dataset (AK Boreal interior measurements only)#####
######## Some code adapted from Talucci's supplementary materials 
##### Equation to derived thaw depths taken from the pre-print (Talucci et al. 2024): https://essd.copernicus.org/preprints/essd-2024-526/
############################################################################################################################################

##Library
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(raster)
library(dplyr)
library(AICcmodavg)

##Import Daily Temperature dataset (From Step 3 Rmd script)

data = read.csv("Output/ERA5_dailyTemperature_EE_expanded/ERA_dailyTemperature_expanded_summary.csv")%>%
  mutate(Doy = TempDoy)

##Import predicted change in thaw depth (Assuming that reverse engineering this equation will work for differences in thaw rate)

predicted_thaw_rate_difference_upland <- read.csv("Output/WeibullModelOutput_upland_percentchange.csv")

predicted_thaw_rate_difference_lowland <- read.csv("Output/WeibullModelOutput_lowland.csv")

##### Checking to see if I get the same predicted ALT depth using my summarized data and Anna's equations. Yes: They are the same! #### 
# calc_A = function(x) {
#   x %>% 
#     group_by(year, id, lastNm, msrDoy) %>%
#     filter(TempDoy < msrDoy) %>%
#     filter(TempC > 0) %>%
#     arrange(TempDoy) %>%
#     summarize( 
#       A_sum = sum(TempC),
#       A = sqrt(A_sum)) -> x
#   return(x)
# }
# 
# calc_B = function(x) {
#   x %>% 
#     group_by(year, id, lastNm, msrDoy) %>%
#     filter(TempDoy < predDoy) %>%
#     filter(TempC > 0) %>%
#     arrange(TempDoy) %>%
#     summarize( 
#       B_sum = sum(TempC),
#       B = sqrt(B_sum)) -> x
#   return(x)
# }
# ( calc_A_data = calc_A(data) )
# ( calc_B_data = calc_B(data) )
# 
# ### Filter original data 
# ( orgData = data %>% 
#     group_by(year, id, lastNm) %>%
#     filter(TempDoy == predDoy)
# )
# 
# 
# ### Join Calculations with Orginial Data
# 
# 
# ( dataCombine = orgData %>% 
#     full_join(y=calc_A_data, by=c("year", "id", 'lastNm', 'msrDoy')) %>%
#     full_join(y=calc_B_data, by=c("year", "id", 'lastNm', 'msrDoy')) %>%
#     mutate(A_sum = ifelse(is.na(A_sum), 0, A_sum),
#            A =ifelse(is.na(A), 0, A)) %>%
#     mutate(C = B/A) %>%
#     mutate(predDepth_2 = round((msrDpth*C), 0)) 
# )
# 
# dataCombine_sub <- dataCombine%>%
#   dplyr::select(predDepth_2, prdDpth)




##### Figure out how to reverse the equation now #### 

#######Calculating B for the entire dataset 
calc_B = function(x) {
  x %>% 
    group_by(year, id, lastNm, msrDoy) %>%
    filter(TempDoy < predDoy) %>%
    filter(TempC > 0) %>%
    arrange(TempDoy) %>%
    summarize( 
      B_sum = sum(TempC),
      B = sqrt(B_sum)) -> x
  return(x)
}

(calc_B_data = calc_B(data))



#######Calculating A 

## filtering for only temperatures above freezing and only need days prior to predicted ALT DOY. 
data_a <- data%>%
  filter(Doy <= predDoy)%>%
  filter(TempC > 0)


##Create a unique ID for all 7861 rows (so the number of actual observations)
data_a_tempsum <- data_a%>%
  group_by(lastNm, year, msrDoy, id)%>%
  mutate(ID = cur_group_id())

##Create the cumulative sum of all tempC above the current DOY for each observation ( the - TempC is to subtract the temperature from the current row)
data_a_tempsum <- transform(data_a_tempsum,TempSum = ave(TempC,ID, FUN = cumsum) - TempC)

#Take the sqrt of the temperature sum 
data_a_tempsum <- data_a_tempsum%>%
  mutate(A = sqrt(TempSum))





###Join the calc_B_data and the data_a_tempsum dataset together 
##Take the B/A Ratio 
##Calculate the thaw depth from the inverse of Talucci's equation: ALT = ThawDepth(B/A) 
##create a negative thaw depth column for graphing purposes

ThawDepth <- left_join(data_a_tempsum, calc_B_data, by = c("lastNm", "year", "msrDoy", "id"))%>%
  mutate(AB_ratio = B/A)%>%
  mutate(ThawDepthCurrent = prdDpth/AB_ratio)%>%
  mutate(ThawDepthCurrentNeg = ThawDepthCurrent*-1)


##Import predicted change in thaw depth (Assuming that reverse engineering this equation will work for differences in thaw rate)


##filtering out the paired measurements with missing years in their pair (usually the undisturbed datasets in a pair had more years than the disturbed datasets)
ThawDepth_sub_upland <- ThawDepth%>%
  filter(paired != "m5")%>%
  filter((paired == "f1" & year == "2011")|(paired == "f1" & year == "2013")|(paired == "f1" & year == "2014")
         |(paired == "f1" & year == "2012") |(paired == "m3" & year == "2007") |(paired == "m2" & year == "2005")|
           (paired == "l2" & year == "2015") |(paired == "l3" & year == "2017")|(paired == "l4" & year == "2017") |
           (paired == "e3" & year == "2018") |(paired == "e4" & year == "2018")|(paired == "l3" & year == "2018") |
           (paired == "l5" & year == "2018")|(paired == "c1" & year == "2019"))

ThawDepth_sub_upland$ID <- as.factor(ThawDepth_sub_upland$ID)

##Recreate a timesincefire variable 

ThawDepth_sub_upland <- ThawDepth_sub_upland%>%
  mutate(tsf = case_when(distur == "burned" ~ as.factor(year - fireYr), 
                         distur == "unburned" ~ as.factor("unburned")))

# ###Filtering by pair and disturbance and then plotting
# #k1
# k1 <- ThawDepth_sub%>%
#   filter(paired == "k1")
# 
# #m2
# m2 <- ThawDepth_sub%>%
#   filter(paired == "m2")
# 
# #m3
# m3 <- ThawDepth_sub%>%
#   filter(paired == "m3")
# 
# 
# #l2 
# l2 <- ThawDepth_sub%>%
#   filter(paired == "l2")
# 
# #f1
# f1<- ThawDepth_sub%>%
#   filter(paired == "f1")
# 
# #l3
# 
# l3 <- ThawDepth_sub%>%
#   filter(paired == "l3")
# 
# #c1
# 
# c1 <- ThawDepth_sub%>%
#   filter(paired == "c1")
# 
# #######Example Plot with temperature curve (pick a random observation ID and then plot it!) #####
# ##Make sure to adjust the measurement DOY/Depth and predicted DOY/depth!! (In the ggplot code) 
# 
# graph_example <- ThawDepth%>%
#   filter(ID == 7700)
# 
# DailyTemp <- data%>%
#   group_by(lastNm, year, msrDoy, id)%>%
#   mutate(ID = cur_group_id())%>%
#   filter(ID == 7700)
# 
# ggplot()+
#   geom_point(data = graph_example, aes(x = Doy, y = ThawDepthCurrentNeg), color = "#48AAAD", size = 1) +
#   geom_point(data = DailyTemp, aes(x = Doy, y = TempC), color = "#1520A6") +
#   geom_point(aes(x = 172, y = -55), color = "darkred", size = 4) +
#   geom_point( aes(x = 260, y = -93), color = "darkred", size = 4, shape = 4) +
#   geom_hline(yintercept = 0) +
#   xlab("Day of Year") + ylab("ThawDepth (cm)") +
#   theme_classic() +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Air Temperature (C)"))
# 
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/Example1_O'Donnell_2005_unburned.png", height = 5, width= 8)

### SUMMARIZING: Creating averages across paired, disturbance, year of measure and tsf

ThawDepth_sub_summary_upland <- ThawDepth_sub_upland%>%
  group_by( paired, distur, year, Doy, tsf)%>%
  summarize(ThawDepthCurrentNeg_ave = mean(ThawDepthCurrentNeg), TempC_ave = mean(TempC), ThawDepthCurrent_ave = mean(ThawDepthCurrent), 
            ThawDepthCurrent_max = max(ThawDepthCurrent), ThawDepthCurrent_min = min(ThawDepthCurrent))

# #k1
# k1_summary <- ThawDepth_sub_summary%>%
#   filter(paired == "k1")
# 
# #m2
# m2_summary <- ThawDepth_sub_summary%>%
#   filter(paired == "m2")
# 
# #m3
# m3_summary <- ThawDepth_sub_summary%>%
#   filter(paired == "m3")
# 
# #l2 
# l2_summary <- ThawDepth_sub_summary%>%
#   filter(paired == "l2")
# 
# #f1
# f1_summary<- ThawDepth_sub_summary%>%
#   filter(paired == "f1")
# 
# #l3
# 
# l3_summary <- ThawDepth_sub_summary%>%
#   filter(paired == "l3")
# 
# #c1
# 
# c1_summary <- ThawDepth_sub_summary%>%
#   filter(paired == "c1")
# 
# 
# ggplot()+
#   geom_point(data = k1, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
#   facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("K1 Summary")+
#   theme(legend.position = "none") + 
#   geom_smooth(data = k1_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/k1.png", height = 5, width= 8)
# 
# ggplot()+
#   geom_point(data = m2, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
#   facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("m2 Summary")+
#   theme(legend.position = "none") + 
#   geom_smooth(data = m2_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/m2.png", height = 5, width= 8)
# 
# ggplot()+
#   geom_point(data = m3, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
#   facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("m3 Summary")+
#   theme(legend.position = "none") + 
#   geom_smooth(data = m3_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/m3.png", height = 5, width= 8)
# 
# ggplot()+
#   geom_point(data = l2, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
#   facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("l2 Summary")+
#   theme(legend.position = "none") + 
#   geom_smooth(data = l2_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/l2.png", height = 5, width= 8)
# 
# ggplot()+
#   geom_point(data = f1, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
#   facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("f1 Summary")+
#   theme(legend.position = "none") + 
#   geom_smooth(data = f1_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/f1.png", height = 8, width= 10)
# 
# ggplot()+
#   geom_point(data = l3, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
#   facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("l3 Summary")+
#   theme(legend.position = "none") + 
#   geom_smooth(data = l3_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/l3.png", height = 5, width= 8)
# 
# ggplot()+
#   geom_point(data = c1, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
#   facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("c1 Summary")+
#   theme(legend.position = "none") + 
#   geom_smooth(data = c1_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/c1.png", height = 5, width= 8)
# 
# 
# ##Summary (average faceted by year and paired (representing the different time since disturbance pairings)
# supp.labs <- c("14", "1")
# ggplot() +
#   geom_point(data = ThawDepth_sub_summary, aes(Doy, y = ThawDepthCurrentNeg_ave, color = distur)) +
#   facet_wrap(~paired + year) +
#   xlab("Day of year") + ylab("Thaw Depth (cm)")
# ggsave("TransientThaw/Output/ThawDepthCurveExamples/Summary.png", height = 5, width= 8)

######Calculating the change in thaw depth for each day for each time since fire

##Separating the disturbance and then rebinding as separate columns 

ThawDepth_sub_summary_disturb_upland <- ThawDepth_sub_summary_upland%>%
  filter(distur == "burned")%>%
  rename(ThawDepthCurrent_ave_burned = ThawDepthCurrent_ave)%>%
  rename(ThawDepthCurrent_max_burned = ThawDepthCurrent_max)%>%
  rename(ThawDepthCurrent_min_burned = ThawDepthCurrent_min)

ThawDepth_sub_summary_undisturb_upland <- ThawDepth_sub_summary_upland%>%
  filter(distur == "unburned")%>%
  dplyr::select(!tsf)%>%
  rename(ThawDepthCurrent_ave_unburned = ThawDepthCurrent_ave)%>%
  rename(ThawDepthCurrent_max_unburned = ThawDepthCurrent_max)%>%
  rename(ThawDepthCurrent_min_unburned = ThawDepthCurrent_min)

ThawDepth_sub_summary_merge_upland <- merge(ThawDepth_sub_summary_undisturb_upland, ThawDepth_sub_summary_disturb_upland , by = c("year", "paired", "Doy"))%>%
  mutate(change_ThawDepthCurrent_ave_cm = ThawDepthCurrent_ave_burned - ThawDepthCurrent_ave_unburned)%>%
  mutate(change_ThawDepthCurrent_max_cm = ThawDepthCurrent_max_burned - ThawDepthCurrent_max_unburned)%>%
  mutate(change_ThawDepthCurrent_min_cm = ThawDepthCurrent_min_burned - ThawDepthCurrent_min_unburned)


###Create a precent change column, but I need to make sure the undisturbed thaw depth is not zero or else it will come out as NA
ThawDepth_sub_summary_merge_upland <- ThawDepth_sub_summary_merge_upland%>%
  mutate(ThawDepthCurrent_ave_unburned_adjust = case_when(ThawDepthCurrent_ave_unburned > 0 ~ ThawDepthCurrent_ave_unburned, 
                                                          ThawDepthCurrent_ave_unburned == 0 ~ 0.0000001))%>%
  mutate(ThawDepthCurrent_max_unburned_adjust = case_when(ThawDepthCurrent_max_unburned > 0 ~ ThawDepthCurrent_max_unburned, 
                                                          ThawDepthCurrent_max_unburned == 0 ~ 0.0000001))%>%
  mutate(ThawDepthCurrent_min_unburned_adjust = case_when(ThawDepthCurrent_min_unburned > 0 ~ ThawDepthCurrent_min_unburned, 
                                                          ThawDepthCurrent_min_unburned == 0 ~ 0.0000001))%>%
  mutate(percentchange_ThawDepthCurrent_ave = case_when((ThawDepthCurrent_ave_burned > 0.0000001 & ThawDepthCurrent_ave_unburned_adjust > 0.0000001) ~  ((ThawDepthCurrent_ave_burned - ThawDepthCurrent_ave_unburned_adjust)/ThawDepthCurrent_ave_unburned_adjust)*100, 
                                                        (ThawDepthCurrent_ave_burned == 0.0000001 & ThawDepthCurrent_ave_unburned_adjust == 0.0000001) ~ 0.0000001 ))%>%
  mutate(percentchange_ThawDepthCurrent_max = case_when((ThawDepthCurrent_max_burned > 0.0000001 & ThawDepthCurrent_max_unburned_adjust > 0.0000001) ~  ((ThawDepthCurrent_max_burned - ThawDepthCurrent_max_unburned_adjust)/ThawDepthCurrent_max_unburned_adjust)*100, 
                                                        (ThawDepthCurrent_max_burned == 0.0000001 & ThawDepthCurrent_max_unburned_adjust == 0.0000001) ~ 0.0000001 ))%>%
  mutate(percentchange_ThawDepthCurrent_min = case_when((ThawDepthCurrent_min_burned > 0.0000001 & ThawDepthCurrent_min_unburned_adjust > 0.0000001) ~  ((ThawDepthCurrent_min_burned - ThawDepthCurrent_min_unburned_adjust)/ThawDepthCurrent_min_unburned_adjust)*100, 
                                                       (ThawDepthCurrent_min_burned == 0.0000001 & ThawDepthCurrent_min_unburned_adjust == 0.0000001) ~ 0.0000001 ))%>%
  mutate(Season = case_when(Doy < 100 ~ "DOY < 100", 
                            Doy >= 100 & Doy < 150 ~ "DOY = 100 - 150", 
                            Doy >= 150 & Doy < 200 ~ "DOY = 150 - 200",
                            Doy >= 200 ~ "DOY > 200"))
##Create a day of the growing season column, which just counts the doy of the year as the first, second, third, etc. day of the growing season. 



##graphing daily change in thaw depth for different day grouping as a function of time since fire 
ThawDepth_sub_summary_merge_upland$Doy <- as.numeric(ThawDepth_sub_summary_merge_upland$Doy)

##Average the years with mulitple pairs 
ThawDepth_sub_summary_merge_tsfaverage_upland <- ThawDepth_sub_summary_merge_upland%>%
  group_by(paired, Doy, tsf)%>%
  mutate(change_ThawDepthCurrent_tsf_ave_cm = mean(change_ThawDepthCurrent_ave_cm), 
         percent_change_ThawDepthCurrent_tsf_ave = mean(percentchange_ThawDepthCurrent_ave))


ggplot(data = ThawDepth_sub_summary_merge_tsfaverage_upland, aes(x = Doy, y =change_ThawDepthCurrent_tsf_ave_cm, group = paired)) +
  geom_point()+ 
  facet_wrap(~tsf) +xlab("DOY") + ylab("Relative Change in Thaw Depth (percent increase)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) 

ggsave("TransientThaw/Output/ThawDepthCurveExamples/changethawdepth_DOY_relationship.png", height = 5, width= 8)

################## Find the relationship between day of year and change in thaw depth for each time since fire in the observational dataset
## Test linear vs non relationship between day of year and change in thaw depth for all tsf 
#Find the average first thawing degree day of the year across the dataset 
###RESULT: non linear relationship is better!! As currently plotted in the above ggplot

ThawDepth_sub_summary_merge_tsfaverage_relationship_upland <- ThawDepth_sub_summary_merge_upland%>%
  group_by(paired, Doy, tsf)%>%
  mutate(change_ThawDepthCurrent_tsf_ave_cm = mean(change_ThawDepthCurrent_ave_cm))

options(scipen = 999)

##Tsf = 0 
ThawDepth_sub_summary_merge0_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 0)

#linear model
lm_0_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge0_upland )
summary(lm_0_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip0_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge0_upland)

# Summarize the model
summary(quadrecip0_upland)


ggplot(data = ThawDepth_sub_summary_merge0_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

##Tsf = 1
ThawDepth_sub_summary_merge1_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 1)

#linear model
lm_1_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge1_upland )
summary(lm_1_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip1_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge1_upland)

# Summarize the model
summary(quadrecip1_upland)


ggplot(data = ThawDepth_sub_summary_merge1_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

##Tsf = 2

##For the tsf with multiple paired trends, we need to build separate regression lines and then find the average by averaging the predicted values
ThawDepth_sub_summary_merge2_upland_f1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==2)%>%
  filter(paired == "f1")

ThawDepth_sub_summary_merge2_upland_l3 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==2)%>%
  filter(paired == "l3")

ThawDepth_sub_summary_merge2_upland_l4 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==2)%>%
  filter(paired == "l4")

ThawDepth_sub_summary_merge2_upland_m2 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==2)%>%
  filter(paired == "m2")



#linear models
lm_2_upland_f1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_upland_f1)
lm_2_upland_l3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_upland_l3)
lm_2_upland_l4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_upland_l4)
lm_2_upland_m2 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_upland_m2)

#summarize models
summary(lm_2_upland_f1)
summary(lm_2_upland_l3)
summary(lm_2_upland_l4)
summary(lm_2_upland_m2)


#non linear model

# Fit the quadratic reciprocal regression model
quadrecip2_upland_f1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_upland_f1)
quadrecip2_upland_l3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_upland_l3)
quadrecip2_upland_l4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_upland_l4)
quadrecip2_upland_m2 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_upland_m2)

# Summarize the model
summary(quadrecip2_upland_f1)
summary(quadrecip2_upland_l3)
summary(quadrecip2_upland_l4)
summary(quadrecip2_upland_m2)


ggplot(data = ThawDepth_sub_summary_merge2_upland_f1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge2_upland_l3, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge2_upland_l4, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge2_upland_m2, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

# New data to predict on
average_prediction_2 <- data.frame(Doy = seq(97, 271, by = 1))

# Get predictions from each model
upland_2_f1 <- predict(quadrecip2_upland_f1, newdata = average_prediction_2)
upland_2_l3 <- predict(quadrecip2_upland_l3, newdata = average_prediction_2)
upland_2_l4 <- predict(quadrecip2_upland_l4, newdata = average_prediction_2)
upland_2_m2 <- predict(quadrecip2_upland_m2, newdata = average_prediction_2)

# Calculate the average prediction
average_predictions_2_upland <- (upland_2_f1 + upland_2_l3 + upland_2_l4 + upland_2_m2) / 4

average_predictions_2_upland <- data.frame(Doy = average_prediction_2, 
                                            change_ThawDepthCurrent_tsf_ave_cm = average_predictions_2_upland)


ggplot(data = average_predictions_2_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


## TSF = 3
##For the tsf with multiple paired trends, we need to build separate regression lines and then find the average by averaging the predicted values
ThawDepth_sub_summary_merge3_upland_f1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==3)%>%
  filter(paired == "f1")

ThawDepth_sub_summary_merge3_upland_l3 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==3)%>%
  filter(paired == "l3")

ThawDepth_sub_summary_merge3_upland_l5 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==3)%>%
  filter(paired == "l5")


#linear models
lm_3_upland_f1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3_upland_f1)
lm_3_upland_l3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3_upland_l3)
lm_3_upland_l5 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3_upland_l5)

#summarize models
summary(lm_3_upland_f1)
summary(lm_3_upland_l3)
summary(lm_3_upland_l5)



#non linear model
# Fit the quadratic reciprocal regression model
quadrecip3_upland_f1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3_upland_f1)
quadrecip3_upland_l3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3_upland_l3)
quadrecip3_upland_l5 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3_upland_l5)

# Summarize the model
summary(quadrecip3_upland_f1)
summary(quadrecip3_upland_l3)
summary(quadrecip3_upland_l5)



ggplot(data = ThawDepth_sub_summary_merge3_upland_f1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge3_upland_l3, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge3_upland_l5, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


# New data to predict on
average_prediction_3 <- data.frame(Doy = seq(114, 251, by = 1))

# Get predictions from each model
upland_3_f1 <- predict(quadrecip3_upland_f1, newdata = average_prediction_3)
upland_3_l3 <- predict(quadrecip3_upland_l3, newdata = average_prediction_3)
upland_3_l5 <- predict(quadrecip3_upland_l5, newdata = average_prediction_3)

# Calculate the average prediction
average_predictions_3_upland <- (upland_3_f1 + upland_3_l3 + upland_3_l5) / 3

average_predictions_3_upland <- data.frame(Doy = average_prediction_3, 
                                           change_ThawDepthCurrent_tsf_ave_cm = average_predictions_3_upland)


ggplot(data = average_predictions_3_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")



###########TSf = 4
##For the tsf with multiple paired trends, we need to build separate regression lines and then find the average by averaging the predicted values
ThawDepth_sub_summary_merge4_upland_f1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==4)%>%
  filter(paired == "f1")

ThawDepth_sub_summary_merge4_upland_m3 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==4)%>%
  filter(paired == "m3")

ThawDepth_sub_summary_merge4_upland_e4 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==4)%>%
  filter(paired == "e4")


#linear models
lm_4_upland_f1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge4_upland_f1)
lm_4_upland_m3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge4_upland_m3)
lm_4_upland_e4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge4_upland_e4)

#summarize models
summary(lm_4_upland_f1)
summary(lm_4_upland_m3)
summary(lm_4_upland_e4)



#non linear model
# Fit the quadratic reciprocal regression model
quadrecip4_upland_f1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge4_upland_f1)
quadrecip4_upland_m3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge4_upland_m3)
quadrecip4_upland_e4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge4_upland_e4)

# Summarize the model
summary(quadrecip4_upland_f1)
summary(quadrecip4_upland_m3)
summary(quadrecip4_upland_e4)



ggplot(data = ThawDepth_sub_summary_merge4_upland_f1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge4_upland_m3, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge4_upland_e4, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


# New data to predict on
average_prediction_4 <- data.frame(Doy = seq(115, 245, by = 1))

# Get predictions from each model
upland_4_f1 <- predict(quadrecip4_upland_f1, newdata = average_prediction_4)
upland_4_m3 <- predict(quadrecip4_upland_m3, newdata = average_prediction_4)
upland_4_e4 <- predict(quadrecip4_upland_e4, newdata = average_prediction_4)

# Calculate the average prediction
average_predictions_4_upland <- (upland_4_f1 + upland_4_m3 + upland_4_e4) / 3

average_predictions_4_upland <- data.frame(Doy = average_prediction_4, 
                                           change_ThawDepthCurrent_tsf_ave_cm = average_predictions_4_upland)


ggplot(data = average_predictions_4_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

############Tsf = 13

ThawDepth_sub_summary_merge13_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 13)

#linear model
lm_13_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge13_upland )
summary(lm_13_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip13_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge13_upland)

# Summarize the model
summary(quadrecip13_upland)


ggplot(data = ThawDepth_sub_summary_merge13_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

############Tsf = 14
##For the tsf with multiple paired trends, we need to build separate regression lines and then find the average by averaging the predicted values
ThawDepth_sub_summary_merge14_upland_c1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==14)%>%
  filter(paired == "c1")

ThawDepth_sub_summary_merge14_upland_l3 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==14)%>%
  filter(paired == "l3")

ThawDepth_sub_summary_merge14_upland_l5 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf ==14)%>%
  filter(paired == "l5")


#linear models
lm_14_upland_c1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge14_upland_c1)
lm_14_upland_l3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge14_upland_l3)
lm_14_upland_l5 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge14_upland_l5)

#summarize models
summary(lm_14_upland_c1)
summary(lm_14_upland_l3)
summary(lm_14_upland_l5)



#non linear model
# Fit the quadratic reciprocal regression model
quadrecip14_upland_c1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge14_upland_c1)
quadrecip14_upland_l3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge14_upland_l3)
quadrecip14_upland_l5 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge14_upland_l5)

# Summarize the model
summary(quadrecip14_upland_c1)
summary(quadrecip14_upland_l3)
summary(quadrecip14_upland_l5)



ggplot(data = ThawDepth_sub_summary_merge14_upland_c1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge14_upland_l3, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(data = ThawDepth_sub_summary_merge14_upland_l5, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


# New data to predict on
average_prediction_14 <- data.frame(Doy = seq(102, 266, by = 1))

# Get predictions from each model
upland_14_c1 <- predict(quadrecip14_upland_c1, newdata = average_prediction_14)
upland_14_l3 <- predict(quadrecip14_upland_l3, newdata = average_prediction_14)
upland_14_l5 <- predict(quadrecip14_upland_l5, newdata = average_prediction_14)

# Calculate the average prediction
average_predictions_14_upland <- (upland_14_c1 + upland_14_l3 + upland_14_l5) / 3

average_predictions_14_upland <- data.frame(Doy = average_prediction_14, 
                                           change_ThawDepthCurrent_tsf_ave_cm = average_predictions_14_upland)


ggplot(data = average_predictions_14_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

############Tsf = 15

ThawDepth_sub_summary_merge15_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 15)

#linear model
lm_15_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge15_upland )
summary(lm_15_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip15_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge15_upland)

# Summarize the model
summary(quadrecip15_upland)


ggplot(data = ThawDepth_sub_summary_merge15_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

############Tsf = 17
ThawDepth_sub_summary_merge17_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 17)

#linear model
lm_17_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge17_upland )
summary(lm_17_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip17_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge17_upland)

# Summarize the model
summary(quadrecip17_upland)


ggplot(data = ThawDepth_sub_summary_merge17_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 32
ThawDepth_sub_summary_merge32_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 32)

#linear model
lm_32_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge32_upland )
summary(lm_32_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip32_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge32_upland)

# Summarize the model
summary(quadrecip32_upland)


ggplot(data = ThawDepth_sub_summary_merge32_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 40
ThawDepth_sub_summary_merge40_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 40)

#linear model
lm_40_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge40_upland )
summary(lm_40_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip40_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge40_upland)

# Summarize the model
summary(quadrecip40_upland)


ggplot(data = ThawDepth_sub_summary_merge40_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 45
ThawDepth_sub_summary_merge45_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 45)

#linear model
lm_45_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge45_upland )
summary(lm_45_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip45_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge45_upland)

# Summarize the model
summary(quadrecip45_upland)


ggplot(data = ThawDepth_sub_summary_merge45_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 49
ThawDepth_sub_summary_merge49_upland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_upland%>%
  filter(tsf == 49)

#linear model
lm_49_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge49_upland )
summary(lm_49_upland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip49_upland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge49_upland)

# Summarize the model
summary(quadrecip49_upland)


ggplot(data = ThawDepth_sub_summary_merge49_upland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) 
  #geom_smooth(method = "lm", se = FALSE, color = "red")


####Find the average of the quadratic coefficient across tsf
coef0 <- coefficients(quadrecip0_upland)
a0 <- coef0[3]
b0 <- coef0[2]

coef1 <- coefficients(quadrecip1_upland)
a1 <- coef1[3]
b1 <- coef1[2]

coef2_l3 <- coefficients(quadrecip2_upland_l3)
a2_l3 <- coef2_l3[3]
b2_l3 <- coef2_l3[2]

coef2_l4 <- coefficients(quadrecip2_upland_l4)
a2_l4 <- coef2_l4[3]
b2_l4 <- coef2_l4[2]

coef2_m2 <- coefficients(quadrecip2_upland_m2)
a2_m2 <- coef2_m2[3]
b2_m2 <- coef2_m2[2]


coef2_f1 <- coefficients(quadrecip2_upland_f1)
a2_f1 <- coef2_f1[3]
b2_f1 <- coef2_f1[2]


coef3_f1 <- coefficients(quadrecip3_upland_f1)
a3_f1 <- coef3_f1[3]
b3_f1 <- coef3_f1[2]

coef3_l3 <- coefficients(quadrecip3_upland_l3)
a3_l3 <- coef3_l3[3]
b3_l3 <- coef3_l3[2]

coef3_l5 <- coefficients(quadrecip3_upland_l5)
a3_l5 <- coef3_l5[3]
b3_l5 <- coef3_l5[2]

coef4_f1 <- coefficients(quadrecip4_upland_f1)
a4_f1 <- coef4_f1[3]
b4_f1 <- coef4_f1[2]

coef4_e4 <- coefficients(quadrecip4_upland_e4)
a4_e4 <- coef4_e4[3]
b4_e4 <- coef4_e4[2]

coef4_m3 <- coefficients(quadrecip4_upland_m3)
a4_m3 <- coef4_m3[3]
b4_m3 <- coef4_m3[2]

coef13 <- coefficients(quadrecip13_upland)
a13 <- coef13[3]
b13 <- coef13[2]

coef14_c1 <- coefficients(quadrecip14_upland_c1)
a14_c1 <- coef14_c1[3]
b14_c1 <- coef14_c1[2]

coef14_l3 <- coefficients(quadrecip14_upland_l3)
a14_l3 <- coef14_l3[3]
b14_l3 <- coef14_l3[2]

coef14_l5 <- coefficients(quadrecip14_upland_l5)
a14_l5 <- coef14_l5[3]
b14_l5 <- coef14_l5[2]

coef15 <- coefficients(quadrecip15_upland)
a15 <- coef15[3]
b15 <- coef15[2]

coef17 <- coefficients(quadrecip17_upland)
a17 <- coef17[3]
b17 <- coef17[2]

coef32 <- coefficients(quadrecip32_upland)
a32 <- coef32[3]
b32 <- coef32[2]

coef40 <- coefficients(quadrecip40_upland)
a40 <- coef40[3]
b40 <- coef40[2]

coef45 <- coefficients(quadrecip45_upland)
a45 <- coef45[3]
b45 <- coef45[2]

coef49 <- coefficients(quadrecip49_upland)
a49 <- coef49[3]
b49 <- coef49[2]

a <- data.frame(a0,a1,
                a2_l3,a2_l4,a2_m2,a2_f1,
                a3_f1,a3_l3,a3_l5,
                a4_e4,a4_f1,a4_m3,
                a13,
                a14_c1,a14_l3,a14_l5,
                a15,a17,a32,a40,a45,a49)
a_mean <- rowMeans(a)


a_0_4 <- data.frame(a0,a1,
                    a2_l3,a2_l4,a2_m2,a2_f1,
                    a3_f1,a3_l3,a3_l5,
                    a4_e4,a4_f1,a4_m3)
a_0_4_mean <- rowMeans(a_0_4)

a_13_49 <- data.frame(a13,
                      a14_c1,a14_l3,a14_l5,
                      a15,a17,a32,a40,a45,a49)
a_13_49_mean <- rowMeans(a_13_49)

########Now Predict across all time since fire ####################
##Find the average first and last thawing degree day of the year across the dataset 
firstthawingdegreeday <- ThawDepth_sub_upland%>%
  group_by(year, paired, plotId)%>%
  summarize(Doy_min = min(Doy))%>%
  ungroup()%>%
  summarize(Doy_min_average = mean(Doy_min))

firstthawingdegreeday$Doy_min_average <- format(firstthawingdegreeday$Doy_min_average, digits = 3)

lastthawingdegreeday <- ThawDepth_sub_upland%>%
  group_by(year, paired, plotId)%>%
  summarize(Doy_max = max(Doy))%>%
  ungroup()%>%
  summarize(Doy_max_average = mean(Doy_max))

lastthawingdegreeday$Doy_max_average <-   format(lastthawingdegreeday$Doy_max_average, digits = 3)

##Now take the predicted end of season ALT depths from the modelled data and add the average end of year DOY 
options(scipen = 999)
predicted_ALT <- predicted_thaw_rate_difference_upland%>%
  rename(predicted_ALT_change_last_percentchange = predicted_mean_change_ALD_percent_change)%>%
  mutate(Predicted_DOY_max = lastthawingdegreeday$Doy_max_average)

##Create a new dataframe with the beginning of season at the average DOY min with the change in thaw depth set to zero 

predicted_ALT_zero <- data.frame(predicted_ALT$tsf)%>%
  mutate(predicted_ALT_change_first = 0)%>%
  mutate(Predicted_DOY_min = firstthawingdegreeday$Doy_min_average)%>%
  rename(tsf = predicted_ALT.tsf)


##merge these dataframes together 

predicted_ALT <- merge(predicted_ALT, predicted_ALT_zero, by = "tsf")

##now build slope and intercept from the predicted x and y points 
predicted_ALT$Predicted_DOY_max <- as.numeric(predicted_ALT$Predicted_DOY_max)
predicted_ALT$Predicted_DOY_min <- as.numeric(predicted_ALT$Predicted_DOY_min)

### reserve engineer the linear equation (plus quadratic term) y = ax^2 +bx + c

##Find a parameter (the quadratic parameter: use the average across the tsf that we do have: cannot predict this shape of the parabula for each other year without more Doy data)
predicted_ALT_2 <- predicted_ALT%>%
  mutate(a = case_when(tsf >= 0 & tsf <= 4 ~ a_0_4_mean, 
                       tsf > 4 & tsf <= 49 ~ a_13_49_mean,
                       tsf > 49 ~ 0))%>%
  group_by(tsf)%>%
  mutate(b = ((predicted_ALT_change_last_percentchange - predicted_ALT_change_first - a*((Predicted_DOY_max^2) - (Predicted_DOY_min^2)))/(Predicted_DOY_max - Predicted_DOY_min)))%>%
  mutate(c = predicted_ALT_change_last_percentchange-(b*Predicted_DOY_max)-(a*Predicted_DOY_max^2))%>%
  ungroup()

##Create a list of all the DOY we want to predict the change in ALD 
Predicted_DOY_list <- c(100:271)

predicted_ALD_allDOY <- predicted_ALT_2%>%
  expand(tsf, full_seq(Predicted_DOY_list,1))


##Join that list of DOY across all tsf 
predicted_ALD_allDOY <- left_join(predicted_ALD_allDOY, predicted_ALT_2, by = c("tsf"))%>%
  rename(Predicted_Doy = "full_seq(Predicted_DOY_list, 1)")

##Now predict the ALD for every doy for each tsf 

predicted_ALD_allDOY_2 <- predicted_ALD_allDOY%>%
  mutate(predicted_ALD_change = (a*(Predicted_Doy^2)) + (b*Predicted_Doy) + c)

write.csv(predicted_ALD_allDOY_2, "TransientThaw/Output/predicted_ALD_allDoy_upland_percentchange.csv")

##Graph the predicted change in thaw depth 

ggplot(data = predicted_ALD_allDOY_2, aes(x = Predicted_Doy, y = predicted_ALD_change)) +
  geom_point()+ 
  facet_wrap(~tsf) +xlab("DOY") + ylab("Predicted Change in Thaw Depth (% Change)")

ggsave("TransientThaw/Output/ThawDepthCurveExamples/Predicted_ALD_change_upland_percentchange.png", height = 10, width= 10)
















##############. Old Script ######################

##Library
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(raster)
library(dplyr)
library(AICcmodavg)

##Import Daily Temperature dataset (From Step 3 Rmd script)

data = read.csv("Output/ERA_dailyTemperature_summary.csv")%>%
  mutate(Doy = TempDoy)

##Import predicted change in thaw depth (Assuming that reverse engineering this equation will work for differences in thaw rate)

predicted_thaw_rate_difference <- read.csv("Output/WeibullModelOutput_mean_ALT.csv")


##### Checking to see if I get the same predicted ALT depth using my summarized data and Anna's equations. Yes: They are the same! #### 
# calc_A = function(x) {
#   x %>% 
#     group_by(year, id, lastNm, msrDoy) %>%
#     filter(TempDoy < msrDoy) %>%
#     filter(TempC > 0) %>%
#     arrange(TempDoy) %>%
#     summarize( 
#       A_sum = sum(TempC),
#       A = sqrt(A_sum)) -> x
#   return(x)
# }
# 
# calc_B = function(x) {
#   x %>% 
#     group_by(year, id, lastNm, msrDoy) %>%
#     filter(TempDoy < predDoy) %>%
#     filter(TempC > 0) %>%
#     arrange(TempDoy) %>%
#     summarize( 
#       B_sum = sum(TempC),
#       B = sqrt(B_sum)) -> x
#   return(x)
# }
# ( calc_A_data = calc_A(data) )
# ( calc_B_data = calc_B(data) )
# 
# ### Filter original data 
# ( orgData = data %>% 
#     group_by(year, id, lastNm) %>%
#     filter(TempDoy == predDoy)
# )
# 
# 
# ### Join Calculations with Orginial Data
# 
# 
# ( dataCombine = orgData %>% 
#     full_join(y=calc_A_data, by=c("year", "id", 'lastNm', 'msrDoy')) %>%
#     full_join(y=calc_B_data, by=c("year", "id", 'lastNm', 'msrDoy')) %>%
#     mutate(A_sum = ifelse(is.na(A_sum), 0, A_sum),
#            A =ifelse(is.na(A), 0, A)) %>%
#     mutate(C = B/A) %>%
#     mutate(predDepth_2 = round((msrDpth*C), 0)) 
# )
# 
# dataCombine_sub <- dataCombine%>%
#   dplyr::select(predDepth_2, prdDpth)




##### Figure out how to reverse the equation now #### 

#######Calculating B for the entire dataset 
calc_B = function(x) {
     x %>% 
      group_by(year, id, lastNm, msrDoy) %>%
      filter(TempDoy < predDoy) %>%
      filter(TempC > 0) %>%
      arrange(TempDoy) %>%
      summarize( 
       B_sum = sum(TempC),
       B = sqrt(B_sum)) -> x
     return(x)
   }

(calc_B_data = calc_B(data))



#######Calculating A 

## filtering for only temperatures above freezing and only need days prior to predicted ALT DOY. 
data_a <- data%>%
  filter(Doy <= predDoy)%>%
  filter(TempC > 0)


##Create a unique ID for all 7861 rows (so the number of actual observations)
data_a_tempsum <- data_a%>%
  group_by(lastNm, year, msrDoy, id)%>%
  mutate(ID = cur_group_id())

##Create the cumulative sum of all tempC above the current DOY for each observation ( the - TempC is to subtract the temperature from the current row)
data_a_tempsum <- transform(data_a_tempsum,TempSum = ave(TempC,ID, FUN = cumsum) - TempC)

#Take the sqrt of the temperature sum 
data_a_tempsum <- data_a_tempsum%>%
  mutate(A = sqrt(TempSum))


  


###Join the calc_B_data and the data_a_tempsum dataset together 
##Take the B/A Ratio 
##Calculate the thaw depth from the inverse of Talucci's equation: ALT = ThawDepth(B/A) 
##create a negative thaw depth column for graphing purposes

ThawDepth <- left_join(data_a_tempsum, calc_B_data, by = c("lastNm", "year", "msrDoy", "id"))%>%
  mutate(AB_ratio = B/A)%>%
  mutate(ThawDepthCurrent = prdDpth/AB_ratio)%>%
  mutate(ThawDepthCurrentNeg = ThawDepthCurrent*-1)


##Import predicted change in thaw depth (Assuming that reverse engineering this equation will work for differences in thaw rate)
  
##predicted_thaw_rate_difference <- read.csv("ProgressiveThaw/Output/WeibullModelOutput_mean_ALT.csv")


##filtering out the paired measurements with missing years in their pair (usually the undisturbed datasets in a pair had more years than the disturbed datasets)
ThawDepth_sub <- ThawDepth%>%
  filter(paired != "m5")%>%
  filter((paired == "c1" & year == "2019") | (paired == "l3" & year == "2017") | (paired == "l3" & year == "2018") |
           (paired == "l2" & year == "2015") |(paired == "f1" & year == "2011")|(paired == "f1" & year == "2013")|(paired == "f1" & year == "2014")
         |(paired == "f1" & year == "2012") |(paired == "f1" & year == "2018")|(paired == "f1" & year == "2020") |(paired == "m3" & year == "2007") |(paired == "m2" & year == "2005")|(paired == "k1" & year == "2001"))

ThawDepth_sub$ID <- as.factor(ThawDepth_sub$ID)

##Recreate a timesincefire variable 

ThawDepth_sub <- ThawDepth_sub%>%
  mutate(tsf = case_when(distur == "burned" ~ as.factor(year - fireYr), 
                         distur == "unburned" ~ as.factor("unburned")))
  
###Filtering by pair and disturbance and then plotting
#k1
k1 <- ThawDepth_sub%>%
  filter(paired == "k1")

#m2
m2 <- ThawDepth_sub%>%
  filter(paired == "m2")

#m3
m3 <- ThawDepth_sub%>%
  filter(paired == "m3")


#l2 
l2 <- ThawDepth_sub%>%
  filter(paired == "l2")

#f1
f1<- ThawDepth_sub%>%
  filter(paired == "f1")

#l3

l3 <- ThawDepth_sub%>%
  filter(paired == "l3")

#c1

c1 <- ThawDepth_sub%>%
  filter(paired == "c1")

#######Example Plot with temperature curve (pick a random observation ID and then plot it!) #####
##Make sure to adjust the measurement DOY/Depth and predicted DOY/depth!! (In the ggplot code) 

graph_example <- ThawDepth%>%
filter(ID == 7700)

DailyTemp <- data%>%
  group_by(lastNm, year, msrDoy, id)%>%
  mutate(ID = cur_group_id())%>%
  filter(ID == 7700)

ggplot()+
  geom_point(data = graph_example, aes(x = Doy, y = ThawDepthCurrentNeg), color = "#48AAAD", size = 1) +
  geom_point(data = DailyTemp, aes(x = Doy, y = TempC), color = "#1520A6") +
  geom_point(aes(x = 172, y = -55), color = "darkred", size = 4) +
  geom_point( aes(x = 260, y = -93), color = "darkred", size = 4, shape = 4) +
  geom_hline(yintercept = 0) +
  xlab("Day of Year") + ylab("ThawDepth (cm)") +
  theme_classic() +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Air Temperature (C)"))

ggsave("TransientThaw/Output/ThawDepthCurveExamples/Example1_O'Donnell_2005_unburned.png", height = 5, width= 8)

### SUMMARIZING: Creating averages across paired, disturbance, year of measure and tsf

ThawDepth_sub_summary <- ThawDepth_sub%>%
  group_by( paired, distur, year, Doy, tsf)%>%
  summarize(ThawDepthCurrentNeg_ave = mean(ThawDepthCurrentNeg), TempC_ave = mean(TempC), ThawDepthCurrent_ave = mean(ThawDepthCurrent), 
            ThawDepthCurrent_max = max(ThawDepthCurrent), ThawDepthCurrent_min = min(ThawDepthCurrent))

#k1
k1_summary <- ThawDepth_sub_summary%>%
  filter(paired == "k1")

#m2
m2_summary <- ThawDepth_sub_summary%>%
  filter(paired == "m2")

#m3
m3_summary <- ThawDepth_sub_summary%>%
  filter(paired == "m3")

#l2 
l2_summary <- ThawDepth_sub_summary%>%
  filter(paired == "l2")

#f1
f1_summary<- ThawDepth_sub_summary%>%
  filter(paired == "f1")

#l3

l3_summary <- ThawDepth_sub_summary%>%
  filter(paired == "l3")

#c1

c1_summary <- ThawDepth_sub_summary%>%
  filter(paired == "c1")


ggplot()+
  geom_point(data = k1, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
  facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("K1 Summary")+
  theme(legend.position = "none") + 
  geom_smooth(data = k1_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
ggsave("TransientThaw/Output/ThawDepthCurveExamples/k1.png", height = 5, width= 8)

ggplot()+
  geom_point(data = m2, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
  facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("m2 Summary")+
  theme(legend.position = "none") + 
  geom_smooth(data = m2_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
ggsave("TransientThaw/Output/ThawDepthCurveExamples/m2.png", height = 5, width= 8)

ggplot()+
  geom_point(data = m3, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
  facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("m3 Summary")+
  theme(legend.position = "none") + 
  geom_smooth(data = m3_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
ggsave("TransientThaw/Output/ThawDepthCurveExamples/m3.png", height = 5, width= 8)

ggplot()+
  geom_point(data = l2, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
  facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("l2 Summary")+
  theme(legend.position = "none") + 
  geom_smooth(data = l2_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
ggsave("TransientThaw/Output/ThawDepthCurveExamples/l2.png", height = 5, width= 8)

ggplot()+
  geom_point(data = f1, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
  facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("f1 Summary")+
  theme(legend.position = "none") + 
  geom_smooth(data = f1_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
ggsave("TransientThaw/Output/ThawDepthCurveExamples/f1.png", height = 8, width= 10)

ggplot()+
  geom_point(data = l3, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
  facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("l3 Summary")+
  theme(legend.position = "none") + 
  geom_smooth(data = l3_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
ggsave("TransientThaw/Output/ThawDepthCurveExamples/l3.png", height = 5, width= 8)

ggplot()+
  geom_point(data = c1, aes(x = Doy, y = ThawDepthCurrentNeg), size = 0.5) +
  facet_wrap(~year +distur + tsf)+ xlab("Day of Year") + ylab("Thaw Depth (cm)") +ggtitle("c1 Summary")+
  theme(legend.position = "none") + 
  geom_smooth(data = c1_summary, aes(x = Doy, y = ThawDepthCurrentNeg_ave))
ggsave("TransientThaw/Output/ThawDepthCurveExamples/c1.png", height = 5, width= 8)


##Summary (average faceted by year and paired (representing the different time since disturbance pairings)
supp.labs <- c("14", "1")
ggplot() +
  geom_point(data = ThawDepth_sub_summary, aes(Doy, y = ThawDepthCurrentNeg_ave, color = distur)) +
  facet_wrap(~paired + year) +
  xlab("Day of year") + ylab("Thaw Depth (cm)")
ggsave("TransientThaw/Output/ThawDepthCurveExamples/Summary.png", height = 5, width= 8)

######Calculating the change in thaw depth for each day for each time since fire

##Separating the disturbance and then rebinding as separate columns 

ThawDepth_sub_summary_disturb <- ThawDepth_sub_summary%>%
  filter(distur == "burned")%>%
  rename(ThawDepthCurrent_ave_burned = ThawDepthCurrent_ave)%>%
  rename(ThawDepthCurrent_max_burned = ThawDepthCurrent_max)%>%
  rename(ThawDepthCurrent_min_burned = ThawDepthCurrent_min)

ThawDepth_sub_summary_undisturb <- ThawDepth_sub_summary%>%
  filter(distur == "unburned")%>%
  dplyr::select(!tsf)%>%
  rename(ThawDepthCurrent_ave_unburned = ThawDepthCurrent_ave)%>%
  rename(ThawDepthCurrent_max_unburned = ThawDepthCurrent_max)%>%
  rename(ThawDepthCurrent_min_unburned = ThawDepthCurrent_min)

ThawDepth_sub_summary_merge <- merge(ThawDepth_sub_summary_undisturb, ThawDepth_sub_summary_disturb , by = c("year", "paired", "Doy"))%>%
  mutate(change_ThawDepthCurrent_ave_cm = ThawDepthCurrent_ave_burned - ThawDepthCurrent_ave_unburned)%>%
  mutate(change_ThawDepthCurrent_max_cm = ThawDepthCurrent_max_burned - ThawDepthCurrent_max_unburned)%>%
  mutate(change_ThawDepthCurrent_min_cm = ThawDepthCurrent_min_burned - ThawDepthCurrent_min_unburned)%>%
  mutate(Season = case_when(Doy < 100 ~ "DOY < 100", 
                            Doy >= 100 & Doy < 150 ~ "DOY = 100 - 150", 
                            Doy >= 150 & Doy < 200 ~ "DOY = 150 - 200",
                            Doy >= 200 ~ "DOY > 200"))
##Create a day of the growing season column, which just counts the doy of the year as the first, second, third, etc. day of the growing season. 



##graphing daily change in thaw depth for different day grouping as a function of time since fire 
ThawDepth_sub_summary_merge$Doy <- as.numeric(ThawDepth_sub_summary_merge$Doy)

##Average the years with mulitple pairs 
ThawDepth_sub_summary_merge_tsfaverage <- ThawDepth_sub_summary_merge%>%
  group_by(paired, Doy, tsf)%>%
  mutate(change_ThawDepthCurrent_tsf_ave_cm = mean(change_ThawDepthCurrent_ave_cm))


ggplot(data = ThawDepth_sub_summary_merge_tsfaverage, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm, group = paired)) +
  geom_point()+ 
   facet_wrap(~tsf) +xlab("DOY") + ylab(" Change in Thaw Depth (cm)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) 
  
ggsave("TransientThaw/Output/ThawDepthCurveExamples/changethawdepth_DOY_relationship.png", height = 5, width= 8)

################## Find the relationship between day of year and change in thaw depth for each time since fire in the observational dataset
## Test linear vs non relationship between day of year and change in thaw depth for all tsf 
#Find the average first thawing degree day of the year across the dataset 
###RESULT: non linear relationship is better!! As currently plotted in the above ggplot

ThawDepth_sub_summary_merge_tsfaverage_relationship <- ThawDepth_sub_summary_merge%>%
  group_by( Doy, tsf)%>%
  mutate(change_ThawDepthCurrent_tsf_ave_cm = mean(change_ThawDepthCurrent_ave_cm))

options(scipen = 999)

##Tsf = 0 
ThawDepth_sub_summary_merge0 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 0)

#linear model
lm_0 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge0 )
summary(lm_0)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip0 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge0)

# Summarize the model
summary(quadrecip0)


ggplot(data = ThawDepth_sub_summary_merge0, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")
  

##TSf = 2
ThawDepth_sub_summary_merge2 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 2)

#linear model
lm_2 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2 )
summary(lm_2)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip2 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2)

# Summarize the model
summary(quadrecip2)


ggplot(data = ThawDepth_sub_summary_merge2, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")



###########TSf = 3
ThawDepth_sub_summary_merge3 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 3)

#linear model
lm_3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3 )
summary(lm_3)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3)

# Summarize the model
summary(quadrecip3)


ggplot(data = ThawDepth_sub_summary_merge2, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

##Test to see which model is better 
#define list of models



ThawDepth_sub_summary_merge3 <- ThawDepth_sub_summary_merge_tsfaverage%>%
  filter(tsf == 3)

lm_3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3 )

summary(lm_3)

# Fit the quadratic reciprocal regression model
quadrecip3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3)

# Summarize the model
summary(quadrecip3)


ggplot(data = ThawDepth_sub_summary_merge3, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")




############Tsf = 4
ThawDepth_sub_summary_merge4 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 4)
#non linear model

# Fit the quadratic reciprocal regression model
quadrecip4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge4)

# Summarize the model
summary(quadrecip4)


ggplot(data = ThawDepth_sub_summary_merge4, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

############Tsf = 1
ThawDepth_sub_summary_merge1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 1)
#non linear model

# Fit the quadratic reciprocal regression model
quadrecip1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge1)

# Summarize the model
summary(quadrecip1)


ggplot(data = ThawDepth_sub_summary_merge1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 1
ThawDepth_sub_summary_merge1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 1)
#non linear model

# Fit the quadratic reciprocal regression model
quadrecip1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge1)

# Summarize the model
summary(quadrecip1)


ggplot(data = ThawDepth_sub_summary_merge1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

############Tsf = 8
ThawDepth_sub_summary_merge8 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 8)
#non linear model

# Fit the quadratic reciprocal regression model
quadrecip8 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge8)

# Summarize the model
summary(quadrecip8)


ggplot(data = ThawDepth_sub_summary_merge8, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 10
ThawDepth_sub_summary_merge10 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 10)
#non linear model

# Fit the quadratic reciprocal regression model
quadrecip10 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge10)

# Summarize the model
summary(quadrecip10)


ggplot(data = ThawDepth_sub_summary_merge10, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 14
ThawDepth_sub_summary_merge14 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 14)
#non linear model

# Fit the quadratic reciprocal regression model
quadrecip14 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge14)

# Summarize the model
summary(quadrecip14)


ggplot(data = ThawDepth_sub_summary_merge14, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 15
ThawDepth_sub_summary_merge15 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 15)
#non linear model

# Fit the quadratic reciprocal regression model
quadrecip15 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge15)

# Summarize the model
summary(quadrecip15)


ggplot(data = ThawDepth_sub_summary_merge15, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


############Tsf = 32
ThawDepth_sub_summary_merge32 <- ThawDepth_sub_summary_merge_tsfaverage_relationship%>%
  filter(tsf == 32)
#non linear model

# Fit the quadratic reciprocal regression model
quadrecip32 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge32)

# Summarize the model
summary(quadrecip32)


ggplot(data = ThawDepth_sub_summary_merge32, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

####Find the average of the quadratic coefficient across tsf
coef0 <- coefficients(quadrecip0)
a0 <- coef0[3]
b0 <- coef0[2]

coef1 <- coefficients(quadrecip1)
a1 <- coef1[3]
b1 <- coef1[2]

coef2 <- coefficients(quadrecip2)
a2 <- coef2[3]
b2 <- coef2[2]

coef3 <- coefficients(quadrecip3)
a3 <- coef3[3]
b3 <- coef3[2]

coef4 <- coefficients(quadrecip4)
a4 <- coef4[3]
b4 <- coef4[2]

coef8 <- coefficients(quadrecip8)
a8 <- coef8[3]
b8 <- coef8[2]

coef14 <- coefficients(quadrecip14)
a14 <- coef14[3]
b14 <- coef14[2]

coef10 <- coefficients(quadrecip10)
a10 <- coef10[3]
b10 <- coef10[2]

coef15 <- coefficients(quadrecip15)
a15 <- coef15[3]
b15 <- coef15[2]

coef32 <- coefficients(quadrecip32)
a32 <- coef32[3]
b32 <- coef32[2]

a <- data.frame(a0,a1,a2,a3,a4,a8,a10,a14,a15,a32)
a_mean <- rowMeans(a)


a_0_10 <- data.frame(a0,a1,a2,a3,a4,a8,a10)
a_0_10_mean <- rowMeans(a_0_10)

a_14_32 <- data.frame(a14,a15,a32)
a_14_32_mean <- rowMeans(a_14_32)

########Now Predict across all time since fire ####################
##Find the average first and last thawing degree day of the year across the dataset 
firstthawingdegreeday <- ThawDepth_sub%>%
  group_by(year, paired, plotId)%>%
  summarize(Doy_min = min(Doy))%>%
  ungroup()%>%
  summarize(Doy_min_average = mean(Doy_min))

firstthawingdegreeday$Doy_min_average <- format(firstthawingdegreeday$Doy_min_average, digits = 3)
  
lastthawingdegreeday <- ThawDepth_sub%>%
  group_by(year, paired, plotId)%>%
  summarize(Doy_max = max(Doy))%>%
  ungroup()%>%
  summarize(Doy_max_average = mean(Doy_max))

lastthawingdegreeday$Doy_max_average <-   format(lastthawingdegreeday$Doy_max_average, digits = 3)

##Now take the predicted end of season ALT depths from the modelled data and add the average end of year DOY 
options(scipen = 999)
predicted_ALT <- predicted_thaw_rate_difference%>%
  rename(predicted_ALT_change_last = predicted_mean_change_ALD_cm)%>%
  mutate(Predicted_DOY_max = lastthawingdegreeday$Doy_max_average)

##Create a new dataframe with the beginning of season at the average DOY min with the change in thaw depth set to zero 

predicted_ALT_zero <- data.frame(predicted_ALT$tsf)%>%
  mutate(predicted_ALT_change_first = 0)%>%
  mutate(Predicted_DOY_min = firstthawingdegreeday$Doy_min_average)%>%
  rename(tsf = predicted_ALT.tsf)
  

##merge these dataframes together 

predicted_ALT <- merge(predicted_ALT, predicted_ALT_zero, by = "tsf")

##now build slope and intercept from the predicted x and y points 
predicted_ALT$Predicted_DOY_max <- as.numeric(predicted_ALT$Predicted_DOY_max)
predicted_ALT$Predicted_DOY_min <- as.numeric(predicted_ALT$Predicted_DOY_min)

### reserve engineer the linear equation (plus quadratic term) y = ax^2 +bx + c

##Find a parameter (the quadratic parameter: use the average across the tsf that we do have: cannot predict this shape of the parabula for each other year without more Doy data)
predicted_ALT_2 <- predicted_ALT%>%
  mutate(a = case_when(tsf >= 0 & tsf <= 10 ~ a_0_10_mean, 
                       tsf > 10 & tsf <= 32 ~ a_14_32_mean,
                       tsf > 32 ~ 0))%>%
  group_by(tsf)%>%
  mutate(b = ((predicted_ALT_change_last - predicted_ALT_change_first - a*((Predicted_DOY_max^2) - (Predicted_DOY_min^2)))/(Predicted_DOY_max - Predicted_DOY_min)))%>%
  mutate(c = predicted_ALT_change_last-(b*Predicted_DOY_max)-(a*Predicted_DOY_max^2))%>%
  ungroup()

##Create a list of all the DOY we want to predict the change in ALD 
Predicted_DOY_list <- c(100:271)

predicted_ALD_allDOY <- predicted_ALT_2%>%
 expand(tsf, full_seq(Predicted_DOY_list,1))


##Join that list of DOY across all tsf 
predicted_ALD_allDOY <- left_join(predicted_ALD_allDOY, predicted_ALT_2, by = c("tsf"))%>%
  rename(Predicted_Doy = "full_seq(Predicted_DOY_list, 1)")

##Now predict the ALD for every doy for each tsf 

predicted_ALD_allDOY_2 <- predicted_ALD_allDOY%>%
  mutate(predicted_ALD_change = (a*(Predicted_Doy^2)) + (b*Predicted_Doy) + c)

write.csv(predicted_ALD_allDOY_2, "TransientThaw/Output/predicted_ALD_allDoy.csv")

##Graph the predicted change in thaw depth 

##Create a dataframe with 30 years and earlier 

predicted_ALD_allDOY_30 <- predicted_ALD_allDOY_2%>%
  filter(tsf <= 30)

ggplot(data = predicted_ALD_allDOY_2, aes(x = Predicted_Doy, y = predicted_ALD_change)) +
  geom_point()+ 
  facet_wrap(~tsf) +xlab("DOY") + ylab("Predicted Change in Thaw Depth (cm)")

ggsave("TransientThaw/Output/ThawDepthCurveExamples/Predicted_ALD_change.png", height = 10, width= 10)
