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



predicted_thaw_rate_difference_lowland <- read.csv("Output/UpdateWeibullPercentChange_Range/WeibullModelOutput_Lowland_percentChange_Range.csv")

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
ThawDepth_sub_lowland <- ThawDepth%>%
  filter((paired == "k1")|(paired == "m1")|(paired == "i1")
         |(paired == "n1") |(paired == "n2") |(paired == "n3")|
           (paired == "n4") |(paired == "n5")|(paired == "n6") |
           (paired == "n7") |(paired == "i2"))

ThawDepth_sub_lowland$ID <- as.factor(ThawDepth_sub_lowland$ID)

##Recreate a timesincefire variable 

ThawDepth_sub_lowland <- ThawDepth_sub_lowland%>%
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

ThawDepth_sub_summary_lowland <- ThawDepth_sub_lowland%>%
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

ThawDepth_sub_summary_disturb_lowland <- ThawDepth_sub_summary_lowland%>%
  filter(distur == "burned")%>%
  rename(ThawDepthCurrent_ave_burned = ThawDepthCurrent_ave)%>%
  rename(ThawDepthCurrent_max_burned = ThawDepthCurrent_max)%>%
  rename(ThawDepthCurrent_min_burned = ThawDepthCurrent_min)

ThawDepth_sub_summary_undisturb_lowland <- ThawDepth_sub_summary_lowland%>%
  filter(distur == "unburned")%>%
  dplyr::select(!tsf)%>%
  rename(ThawDepthCurrent_ave_unburned = ThawDepthCurrent_ave)%>%
  rename(ThawDepthCurrent_max_unburned = ThawDepthCurrent_max)%>%
  rename(ThawDepthCurrent_min_unburned = ThawDepthCurrent_min)

ThawDepth_sub_summary_merge_lowland <- merge(ThawDepth_sub_summary_undisturb_lowland, ThawDepth_sub_summary_disturb_lowland , by = c("year", "paired", "Doy"))%>%
  mutate(change_ThawDepthCurrent_ave_cm = ThawDepthCurrent_ave_burned - ThawDepthCurrent_ave_unburned)%>%
  mutate(change_ThawDepthCurrent_max_cm = ThawDepthCurrent_max_burned - ThawDepthCurrent_max_unburned)%>%
  mutate(change_ThawDepthCurrent_min_cm = ThawDepthCurrent_min_burned - ThawDepthCurrent_min_unburned)%>%
  mutate(Season = case_when(Doy < 100 ~ "DOY < 100", 
                            Doy >= 100 & Doy < 150 ~ "DOY = 100 - 150", 
                            Doy >= 150 & Doy < 200 ~ "DOY = 150 - 200",
                            Doy >= 200 ~ "DOY > 200"))
##Create a day of the growing season column, which just counts the doy of the year as the first, second, third, etc. day of the growing season. 



##graphing daily change in thaw depth for different day grouping as a function of time since fire 
ThawDepth_sub_summary_merge_lowland$Doy <- as.numeric(ThawDepth_sub_summary_merge_lowland$Doy)

##Average the years with mulitple pairs 
ThawDepth_sub_summary_merge_tsfaverage_lowland <- ThawDepth_sub_summary_merge_lowland%>%
  group_by(paired, Doy, tsf)%>%
  mutate(change_ThawDepthCurrent_tsf_ave_cm = mean(change_ThawDepthCurrent_ave_cm))


ggplot(data = ThawDepth_sub_summary_merge_tsfaverage_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm, group = paired)) +
  geom_point()+ 
  facet_wrap(~tsf) +xlab("DOY") + ylab(" Change in Thaw Depth (cm)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) 

#ggsave("TransientThaw/Output/ThawDepthCurveExamples/changethawdepth_DOY_relationship.png", height = 5, width= 8)

################## Find the relationship between day of year and change in thaw depth for each time since fire in the observational dataset
## Test linear vs non relationship between day of year and change in thaw depth for all tsf 
#Find the average first thawing degree day of the year across the dataset 
###RESULT: non linear relationship is better!! As currently plotted in the above ggplot

ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland <- ThawDepth_sub_summary_merge_lowland%>%
  group_by(paired, Doy, tsf)%>%
  mutate(change_ThawDepthCurrent_tsf_ave_cm = mean(change_ThawDepthCurrent_ave_cm))

options(scipen = 999)

##Tsf = 0 
ThawDepth_sub_summary_merge0_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 0)

#linear model
lm_0_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge0_lowland)
summary(lm_0_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip0_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge0_lowland)

# Summarize the model
summary(quadrecip0_lowland)


ggplot(data = ThawDepth_sub_summary_merge0_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

##Tsf = 1
ThawDepth_sub_summary_merge1_lowland_n7 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 1)%>%
  filter(paired == "n7")

ThawDepth_sub_summary_merge1_lowland_n6 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 1)%>%
  filter(paired == "n6")

ThawDepth_sub_summary_merge1_lowland_n3 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 1)%>%
  filter(paired == "n3")

ThawDepth_sub_summary_merge1_lowland_i1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 1)%>%
  filter(paired == "i1")

#linear model
lm_1_lowland_i1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge1_lowland_i1 )
lm_1_lowland_n3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge1_lowland_n3 )
lm_1_lowland_n6 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge1_lowland_n6 )
lm_1_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge1_lowland_n7 )
summary(lm_1_lowland_i1)
summary(lm_1_lowland_n3)
summary(lm_1_lowland_n6)
summary(lm_1_lowland_n7)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip1_lowland_i1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge1_lowland_i1)
quadrecip1_lowland_n3 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge1_lowland_n3)
quadrecip1_lowland_n6 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge1_lowland_n6)
quadrecip1_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge1_lowland_n7)

# Summarize the model
summary(quadrecip1_lowland_i1)
summary(quadrecip1_lowland_n3)
summary(quadrecip1_lowland_n6)
summary(quadrecip1_lowland_n7)


ggplot(data = ThawDepth_sub_summary_merge1_lowland_i1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge1_lowland_n3, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge1_lowland_n6, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge1_lowland_n7, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

# New data to predict on
average_prediction_1 <- data.frame(Doy = seq(105, 271, by = 1))

# Get predictions from each model
lowland_1_i1 <- predict(quadrecip1_lowland_i1, newdata = average_prediction_1)
lowland_1_n3 <- predict(quadrecip1_lowland_n3, newdata = average_prediction_1)
lowland_1_n6 <- predict(quadrecip1_lowland_n6, newdata = average_prediction_1)
lowland_1_n7 <- predict(quadrecip1_lowland_n7, newdata = average_prediction_1)

# Calculate the average prediction
average_predictions_1_lowland <- (lowland_1_i1 + lowland_1_n3 + lowland_1_n6 + lowland_1_n7) / 4

average_predictions_1_lowland <- data.frame(Doy = average_prediction_1, 
                                           change_ThawDepthCurrent_tsf_ave_cm = average_predictions_1_lowland)


ggplot(data = average_predictions_1_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


##### Tsf = 2


ThawDepth_sub_summary_merge2_lowland_k1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 2)%>%
  filter(paired == "k1")

ThawDepth_sub_summary_merge2_lowland_m1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 2)%>%
  filter(paired == "m1")

ThawDepth_sub_summary_merge2_lowland_n5 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 2)%>%
  filter(paired == "n5")

ThawDepth_sub_summary_merge2_lowland_i1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 2)%>%
  filter(paired == "i1")

ThawDepth_sub_summary_merge2_lowland_i2 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 2)%>%
  filter(paired == "i2")

ThawDepth_sub_summary_merge2_lowland_n7 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 2)%>%
  filter(paired == "n7")


#linear model
lm_2_lowland_k1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_lowland_k1 )
lm_2_lowland_m1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_lowland_m1 )
lm_2_lowland_n5 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_lowland_n5 )
lm_2_lowland_i1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_lowland_i1 )
lm_2_lowland_i2 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_lowland_i2 )
lm_2_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge2_lowland_n7 )


summary(lm_2_lowland_k1)
summary(lm_2_lowland_m1)
summary(lm_2_lowland_n5)
summary(lm_2_lowland_i1)
summary(lm_2_lowland_i2)
summary(lm_2_lowland_n7)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip2_lowland_k1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_lowland_k1)
quadrecip2_lowland_m1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_lowland_m1)
quadrecip2_lowland_n5 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_lowland_n5)
quadrecip2_lowland_i1<- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_lowland_i1)
quadrecip2_lowland_i2<- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_lowland_i2)
quadrecip2_lowland_n7<- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge2_lowland_n7)


# Summarize the model
summary(quadrecip2_lowland_k1)
summary(quadrecip2_lowland_m1)
summary(quadrecip2_lowland_n5)
summary(quadrecip2_lowland_i1)
summary(quadrecip2_lowland_i2)
summary(quadrecip2_lowland_n7)


ggplot(data = ThawDepth_sub_summary_merge2_lowland_k1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge2_lowland_m1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge2_lowland_n5, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge2_lowland_i1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge2_lowland_i2, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge2_lowland_n7, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

# New data to predict on
average_prediction_2 <- data.frame(Doy = seq(101, 260, by = 1))

# Get predictions from each model
lowland_2_i1 <- predict(quadrecip2_lowland_i1, newdata = average_prediction_2)
lowland_2_n5 <- predict(quadrecip2_lowland_n5, newdata = average_prediction_2)
lowland_2_k1 <- predict(quadrecip2_lowland_k1, newdata = average_prediction_2)
lowland_2_n7 <- predict(quadrecip2_lowland_n7, newdata = average_prediction_2)
lowland_2_i2 <- predict(quadrecip2_lowland_i2, newdata = average_prediction_2)
lowland_2_m1 <- predict(quadrecip2_lowland_m1, newdata = average_prediction_2)

# Calculate the average prediction
average_predictions_2_lowland <- (lowland_2_i1 + lowland_2_n5 + lowland_2_k1 + lowland_2_n7 + lowland_2_i2 + lowland_2_m1) / 6

average_predictions_2_lowland <- data.frame(Doy = average_prediction_2, 
                                            change_ThawDepthCurrent_tsf_ave_cm = average_predictions_2_lowland)


ggplot(data = average_predictions_2_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

######## TSF = 3
##For the tsf with multiple paired trends, we need to build separate regression lines and then find the average by averaging the predicted values

ThawDepth_sub_summary_merge3_lowland_n4 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 3)%>%
  filter(paired == "n4")

ThawDepth_sub_summary_merge3_lowland_n5 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 3)%>%
  filter(paired == "n5")

ThawDepth_sub_summary_merge3_lowland_n7 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 3)%>%
  filter(paired == "n7")

ThawDepth_sub_summary_merge3_lowland_i1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 3)%>%
  filter(paired == "i1")

ThawDepth_sub_summary_merge3_lowland_i2 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 3)%>%
  filter(paired == "i2")


#linear model
lm_3_lowland_n4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3_lowland_n4 )
lm_3_lowland_n5 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3_lowland_n5 )
lm_3_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3_lowland_n7 )
lm_3_lowland_i1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3_lowland_i1 )
lm_3_lowland_i2 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge3_lowland_i2 )


summary(lm_3_lowland_n4)
summary(lm_3_lowland_n5)
summary(lm_3_lowland_n7)
summary(lm_3_lowland_i1)
summary(lm_3_lowland_i2)


#non linear model

# Fit the quadratic reciprocal regression model
quadrecip3_lowland_n4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3_lowland_n4)
quadrecip3_lowland_n5 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3_lowland_n5)
quadrecip3_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3_lowland_n7)
quadrecip3_lowland_i1<- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3_lowland_i1)
quadrecip3_lowland_i2<- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge3_lowland_i2)


# Summarize the model
summary(quadrecip3_lowland_n4)
summary(quadrecip3_lowland_n5)
summary(quadrecip3_lowland_n7)
summary(quadrecip3_lowland_i1)
summary(quadrecip3_lowland_i2)



ggplot(data = ThawDepth_sub_summary_merge3_lowland_n4, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge3_lowland_n5, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge3_lowland_n7, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge3_lowland_i1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge3_lowland_i2, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


# New data to predict on
average_prediction_3 <- data.frame(Doy = seq(90, 269, by = 1))

# Get predictions from each model
lowland_3_i1 <- predict(quadrecip3_lowland_i1, newdata = average_prediction_3)
lowland_3_n5 <- predict(quadrecip3_lowland_n5, newdata = average_prediction_3)
lowland_3_n4 <- predict(quadrecip3_lowland_n4, newdata = average_prediction_3)
lowland_3_n7 <- predict(quadrecip3_lowland_n7, newdata = average_prediction_3)
lowland_3_i2 <- predict(quadrecip3_lowland_i2, newdata = average_prediction_3)

# Calculate the average prediction
average_predictions_3_lowland <- (lowland_3_i1 + lowland_3_n5 + lowland_3_n4 + lowland_3_n7 + lowland_3_i2) / 5

average_predictions_3_lowland <- data.frame(Doy = average_prediction_3, 
                                            change_ThawDepthCurrent_tsf_ave_cm = average_predictions_3_lowland)


ggplot(data = average_predictions_3_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")



###########TSf = 4
##For the tsf with multiple paired trends, we need to build separate regression lines and then find the average by averaging the predicted values
ThawDepth_sub_summary_merge4_lowland_n4 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 4)%>%
  filter(paired == "n4")

ThawDepth_sub_summary_merge4_lowland_i1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 4)%>%
  filter(paired == "i1")

ThawDepth_sub_summary_merge4_lowland_i2 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 4)%>%
  filter(paired == "i2")



#linear model
lm_4_lowland_n4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge4_lowland_n4 )
lm_4_lowland_i1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge4_lowland_i1 )
lm_4_lowland_i2 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge4_lowland_i2 )


summary(lm_4_lowland_n4)
summary(lm_4_lowland_i1)
summary(lm_4_lowland_i2)



#non linear model

# Fit the quadratic reciprocal regression model
quadrecip4_lowland_n4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge4_lowland_n4)
quadrecip4_lowland_i1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge4_lowland_i1)
quadrecip4_lowland_i2 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge4_lowland_i2)


# Summarize the model
summary(quadrecip4_lowland_n4)
summary(quadrecip4_lowland_i1)
summary(quadrecip4_lowland_i2)




ggplot(data = ThawDepth_sub_summary_merge4_lowland_n4, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge4_lowland_i1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge4_lowland_i2, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


# New data to predict on
average_prediction_4 <- data.frame(Doy = seq(104, 259, by = 1))

# Get predictions from each model
lowland_4_i1 <- predict(quadrecip4_lowland_i1, newdata = average_prediction_4)
lowland_4_n4 <- predict(quadrecip4_lowland_n4, newdata = average_prediction_4)
lowland_4_i2 <- predict(quadrecip4_lowland_i2, newdata = average_prediction_4)

# Calculate the average prediction
average_predictions_4_lowland <- (lowland_4_i1 + lowland_4_n4 + lowland_4_i2) / 3

average_predictions_4_lowland <- data.frame(Doy = average_prediction_4, 
                                            change_ThawDepthCurrent_tsf_ave_cm = average_predictions_4_lowland)


ggplot(data = average_predictions_4_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 5
ThawDepth_sub_summary_merge5_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 5)

#linear model
lm_5_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge5_lowland)
summary(lm_5_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip5_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge5_lowland)

# Summarize the model
summary(quadrecip5_lowland)


ggplot(data = ThawDepth_sub_summary_merge5_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

######### TSF = 8
ThawDepth_sub_summary_merge8_lowland_n7 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 8)%>%
  filter(paired == "n7")

ThawDepth_sub_summary_merge8_lowland_n4 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 8)%>%
  filter(paired == "n4")


#linear model
lm_8_lowland_n4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge8_lowland_n4 )
lm_8_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge8_lowland_n7 )


summary(lm_8_lowland_n4)
summary(lm_8_lowland_n7)


#non linear model

# Fit the quadratic reciprocal regression model
quadrecip8_lowland_n4 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge8_lowland_n4)
quadrecip8_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge8_lowland_n7)


# Summarize the model
summary(quadrecip8_lowland_n4)
summary(quadrecip8_lowland_n7)


ggplot(data = ThawDepth_sub_summary_merge8_lowland_n4, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge8_lowland_n7, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


# New data to predict on
average_prediction_8 <- data.frame(Doy = seq(88, 265, by = 1))

# Get predictions from each model
lowland_8_n4 <- predict(quadrecip8_lowland_n4, newdata = average_prediction_8)
lowland_8_n7 <- predict(quadrecip8_lowland_n7, newdata = average_prediction_8)

# Calculate the average prediction
average_predictions_8_lowland <- (lowland_8_n4 + lowland_8_n7) / 2

average_predictions_8_lowland <- data.frame(Doy = average_prediction_8, 
                                            change_ThawDepthCurrent_tsf_ave_cm = average_predictions_8_lowland)


ggplot(data = average_predictions_8_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

######### TSF = 9
ThawDepth_sub_summary_merge9_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 9)

#linear model
lm_9_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge9_lowland)
summary(lm_9_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip9_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge9_lowland)

# Summarize the model
summary(quadrecip9_lowland)


ggplot(data = ThawDepth_sub_summary_merge9_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 10
ThawDepth_sub_summary_merge10_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 10)

#linear model
lm_10_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge10_lowland)
summary(lm_10_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip10_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge10_lowland)

# Summarize the model
summary(quadrecip10_lowland)


ggplot(data = ThawDepth_sub_summary_merge10_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

######### TSF = 12
ThawDepth_sub_summary_merge12_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 12)

#linear model
lm_12_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge12_lowland)
summary(lm_12_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip12_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge12_lowland)

# Summarize the model
summary(quadrecip12_lowland)


ggplot(data = ThawDepth_sub_summary_merge12_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

######### TSF = 13
ThawDepth_sub_summary_merge13_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 13)

#linear model
lm_13_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge13_lowland)
summary(lm_13_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip13_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge13_lowland)

# Summarize the model
summary(quadrecip13_lowland)


ggplot(data = ThawDepth_sub_summary_merge13_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 15
ThawDepth_sub_summary_merge15_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 15)

#linear model
lm_15_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge15_lowland)
summary(lm_15_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip15_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge15_lowland)

# Summarize the model
summary(quadrecip15_lowland)


ggplot(data = ThawDepth_sub_summary_merge15_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

######### TSF = 16
ThawDepth_sub_summary_merge16_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 16)

#linear model
lm_16_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge16_lowland)
summary(lm_16_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip16_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge16_lowland)

# Summarize the model
summary(quadrecip16_lowland)


ggplot(data = ThawDepth_sub_summary_merge16_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 20
ThawDepth_sub_summary_merge20_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 20)

#linear model
lm_20_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge20_lowland)
summary(lm_20_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip20_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge20_lowland)

# Summarize the model
summary(quadrecip20_lowland)


ggplot(data = ThawDepth_sub_summary_merge20_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 21
ThawDepth_sub_summary_merge21_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 21)

#linear model
lm_21_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge21_lowland)
summary(lm_21_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip21_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge21_lowland)

# Summarize the model
summary(quadrecip21_lowland)


ggplot(data = ThawDepth_sub_summary_merge21_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 31
ThawDepth_sub_summary_merge31_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 31)

#linear model
lm_31_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge31_lowland)
summary(lm_31_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip31_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge31_lowland)

# Summarize the model
summary(quadrecip31_lowland)


ggplot(data = ThawDepth_sub_summary_merge31_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

######### TSF = 32
ThawDepth_sub_summary_merge32_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 32)

#linear model
lm_32_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge32_lowland)
summary(lm_32_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip32_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge32_lowland)

# Summarize the model
summary(quadrecip32_lowland)


ggplot(data = ThawDepth_sub_summary_merge32_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 33
ThawDepth_sub_summary_merge33_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 33)

#linear model
lm_33_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge33_lowland)
summary(lm_33_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip33_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge33_lowland)

# Summarize the model
summary(quadrecip33_lowland)


ggplot(data = ThawDepth_sub_summary_merge33_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 40
ThawDepth_sub_summary_merge40_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 40)

#linear model
lm_40_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge40_lowland)
summary(lm_40_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip40_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge40_lowland)

# Summarize the model
summary(quadrecip40_lowland)


ggplot(data = ThawDepth_sub_summary_merge40_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


####TSF 41 

ThawDepth_sub_summary_merge41_lowland_n1 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 41)%>%
  filter(paired == "n1")

ThawDepth_sub_summary_merge41_lowland_n7 <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 41)%>%
  filter(paired == "n7")


#linear model
lm_41_lowland_n1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge41_lowland_n1 )
lm_41_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge41_lowland_n7 )


summary(lm_41_lowland_n1)
summary(lm_41_lowland_n7)


#non linear model

# Fit the quadratic reciprocal regression model
quadrecip41_lowland_n1 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge41_lowland_n1)
quadrecip41_lowland_n7 <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge41_lowland_n7)


# Summarize the model
summary(quadrecip41_lowland_n1)
summary(quadrecip41_lowland_n7)


ggplot(data = ThawDepth_sub_summary_merge41_lowland_n1, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = ThawDepth_sub_summary_merge41_lowland_n7, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


# New data to predict on
average_prediction_41 <- data.frame(Doy = seq(88, 270, by = 1))

# Get predictions from each model
lowland_41_n1 <- predict(quadrecip41_lowland_n1, newdata = average_prediction_41)
lowland_41_n7 <- predict(quadrecip41_lowland_n7, newdata = average_prediction_41)

# Calculate the average prediction
average_predictions_41_lowland <- (lowland_41_n1 + lowland_41_n7) / 2

average_predictions_41_lowland <- data.frame(Doy = average_prediction_41, 
                                            change_ThawDepthCurrent_tsf_ave_cm = average_predictions_41_lowland)


ggplot(data = average_predictions_41_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 48
ThawDepth_sub_summary_merge48_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 48)

#linear model
lm_48_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge48_lowland)
summary(lm_48_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip48_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge48_lowland)

# Summarize the model
summary(quadrecip48_lowland)


ggplot(data = ThawDepth_sub_summary_merge48_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

######### TSF = 49
ThawDepth_sub_summary_merge49_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 49)

#linear model
lm_49_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge49_lowland)
summary(lm_49_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip49_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge49_lowland)

# Summarize the model
summary(quadrecip49_lowland)


ggplot(data = ThawDepth_sub_summary_merge49_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


######### TSF = 52
ThawDepth_sub_summary_merge52_lowland <- ThawDepth_sub_summary_merge_tsfaverage_relationship_lowland%>%
  filter(tsf == 52)

#linear model
lm_52_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy, data =ThawDepth_sub_summary_merge52_lowland)
summary(lm_52_lowland)

#non linear model

# Fit the quadratic reciprocal regression model
quadrecip52_lowland <- lm(change_ThawDepthCurrent_tsf_ave_cm ~ Doy  + I(Doy^2), data = ThawDepth_sub_summary_merge52_lowland)

# Summarize the model
summary(quadrecip52_lowland)


ggplot(data = ThawDepth_sub_summary_merge52_lowland, aes(x = Doy, y = change_ThawDepthCurrent_tsf_ave_cm)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")



####Find the average of the quadratic coefficient across tsf
coef0 <- coefficients(quadrecip0_lowland)
a0 <- coef0[3]
b0 <- coef0[2]

coef1_i1 <- coefficients(quadrecip1_lowland_i1)
a1_i1 <- coef1_i1[3]
b1_i1 <- coef1_i1[2]

coef1_n7 <- coefficients(quadrecip1_lowland_n7)
a1_n7 <- coef1_n7[3]
b1_n7 <- coef1_n7[2]

coef1_n6 <- coefficients(quadrecip1_lowland_n6)
a1_n6 <- coef1_n6[3]
b1_n6 <- coef1_n6[2]

coef1_n3 <- coefficients(quadrecip1_lowland_n3)
a1_n3 <- coef1_n3[3]
b1_n3 <- coef1_n3[2]


coef2_m1 <- coefficients(quadrecip2_lowland_m1)
a2_m1 <- coef2_m1[3]
b2_m1 <- coef2_m1[2]

coef2_k1 <- coefficients(quadrecip2_lowland_k1)
a2_k1 <- coef2_k1[3]
b2_k1 <- coef2_k1[2]

coef2_n5 <- coefficients(quadrecip2_lowland_n5)
a2_n5 <- coef2_n5[3]
b2_n5 <- coef2_n5[2]

coef2_i1 <- coefficients(quadrecip2_lowland_i1)
a2_i1 <- coef2_i1[3]
b2_i1 <- coef2_i1[2]

coef2_i2 <- coefficients(quadrecip2_lowland_i2)
a2_i2 <- coef2_i2[3]
b2_i2 <- coef2_i2[2]

coef2_n7 <- coefficients(quadrecip2_lowland_n7)
a2_n7 <- coef2_n7[3]
b2_n7 <- coef2_n7[2]


coef3_n4 <- coefficients(quadrecip3_lowland_n4)
a3_n4 <- coef3_n4[3]
b3_n4 <- coef3_n4[2]

coef3_n5 <- coefficients(quadrecip3_lowland_n5)
a3_n5 <- coef3_n5[3]
b3_n5 <- coef3_n5[2]

coef3_n7 <- coefficients(quadrecip3_lowland_n7)
a3_n7 <- coef3_n7[3]
b3_n7 <- coef3_n7[2]

coef3_i1 <- coefficients(quadrecip3_lowland_i1)
a3_i1 <- coef3_i1[3]
b3_i1 <- coef3_i1[2]

coef3_i2 <- coefficients(quadrecip3_lowland_i2)
a3_i2 <- coef3_i2[3]
b3_i2 <- coef3_i2[2]


coef4_i1 <- coefficients(quadrecip4_lowland_i1)
a4_i1 <- coef4_i1[3]
b4_i1 <- coef4_i1[2]

coef4_i2 <- coefficients(quadrecip4_lowland_i2)
a4_i2 <- coef4_i2[3]
b4_i2 <- coef4_i2[2]

coef4_n4 <- coefficients(quadrecip4_lowland_n4)
a4_n4 <- coef4_n4[3]
b4_n4 <- coef4_n4[2]


coef5 <- coefficients(quadrecip5_lowland)
a5 <- coef5[3]
b5 <- coef5[2]

coef8_n4 <- coefficients(quadrecip8_lowland_n4)
a8_n4 <- coef8_n4[3]
b8_n4 <- coef8_n4[2]

coef8_n7 <- coefficients(quadrecip8_lowland_n7)
a8_n7 <- coef8_n7[3]
b8_n7 <- coef8_n7[2]

coef9 <- coefficients(quadrecip9_lowland)
a9 <- coef9[3]
b9 <- coef9[2]

coef10 <- coefficients(quadrecip10_lowland)
a10 <- coef10[3]
b10 <- coef10[2]

coef12 <- coefficients(quadrecip12_lowland)
a12 <- coef12[3]
b12 <- coef12[2]

coef13 <- coefficients(quadrecip13_lowland)
a13 <- coef13[3]
b13 <- coef13[2]

coef15 <- coefficients(quadrecip15_lowland)
a15 <- coef15[3]
b15 <- coef15[2]

coef16 <- coefficients(quadrecip16_lowland)
a16 <- coef16[3]
b16 <- coef16[2]

coef20 <- coefficients(quadrecip20_lowland)
a20 <- coef20[3]
b20 <- coef20[2]

coef21 <- coefficients(quadrecip21_lowland)
a21 <- coef21[3]
b21 <- coef21[2]

coef31 <- coefficients(quadrecip31_lowland)
a31 <- coef31[3]
b31 <- coef31[2]

coef32 <- coefficients(quadrecip32_lowland)
a32 <- coef32[3]
b32 <- coef32[2]

coef33 <- coefficients(quadrecip33_lowland)
a33 <- coef33[3]
b33 <- coef33[2]

coef40 <- coefficients(quadrecip40_lowland)
a40 <- coef40[3]
b40 <- coef40[2]

coef41_n1 <- coefficients(quadrecip41_lowland_n1)
a41_n1 <- coef41_n1[3]
b41_n1 <- coef41_n1[2]

coef41_n7 <- coefficients(quadrecip41_lowland_n7)
a41_n7 <- coef41_n7[3]
b41_n7 <- coef41_n7[2]

coef48 <- coefficients(quadrecip48_lowland)
a48 <- coef48[3]
b48 <- coef48[2]

coef49 <- coefficients(quadrecip49_lowland)
a49 <- coef49[3]
b49 <- coef49[2]

coef52 <- coefficients(quadrecip52_lowland)
a52 <- coef52[3]
b52 <- coef52[2]


a <- data.frame(a0,
                a1_i1,a1_n3,a1_n6,a1_n7,
                a2_i1,a2_i2,a2_k1,a2_m1,a2_n5,a2_n7,
                a3_i1,a3_i2,a3_n4,a3_n5,a3_n7,
                a4_i1,a4_i2,a4_n4,
                a5,
                a8_n4, a8_n7,
                a9,a10,a12,a13,a15,a16,a20,a21,a31,a32,a33,
                a40,
                a41_n1, a41_n7,
                a48,a49,a52)
a_mean <- rowMeans(a)


a_0_12 <- data.frame(a0,
                     a1_i1,a1_n3,a1_n6,a1_n7,
                     a2_i1,a2_i2,a2_k1,a2_m1,a2_n5,a2_n7,
                     a3_i1,a3_i2,a3_n4,a3_n5,a3_n7,
                     a4_i1,a4_i2,a4_n4,
                     a5,
                     a8_n4, a8_n7,
                     a9,a10,a12)
a_0_12_mean <- rowMeans(a_0_12)

a_13_52 <- data.frame(a15,a16,a20,a21,a31,a32,a33,
                      a40,
                      a41_n1, a41_n7,
                      a48,a49,a52)
a_13_52_mean <- rowMeans(a_13_52)

########Now Predict across all time since fire ####################
##Find the average first and last thawing degree day of the year across the dataset 
firstthawingdegreeday <- ThawDepth_sub_lowland%>%
  group_by(year, paired, plotId)%>%
  summarize(Doy_min = min(Doy))%>%
  ungroup()%>%
  summarize(Doy_min_average = mean(Doy_min))

firstthawingdegreeday$Doy_min_average <- format(firstthawingdegreeday$Doy_min_average, digits = 3)

lastthawingdegreeday <- ThawDepth_sub_lowland%>%
  group_by(year, paired, plotId)%>%
  summarize(Doy_max = max(Doy))%>%
  ungroup()%>%
  summarize(Doy_max_average = mean(Doy_max))

lastthawingdegreeday$Doy_max_average <-   format(lastthawingdegreeday$Doy_max_average, digits = 3)

##Mean - Medium Scenario 
##Now take the predicted end of season ALT depths from the modelled data and add the average end of year DOY 
options(scipen = 999)

predicted_ALT_mean <- predicted_thaw_rate_difference_lowland%>%
  rename(predicted_ALT_mean_percentchange_last = predicted_mean_percentchange_ALD)%>%
  mutate(Predicted_DOY_max = lastthawingdegreeday$Doy_max_average)

##Create a new dataframe with the beginning of season at the average DOY min with the change in thaw depth set to zero 

predicted_ALT_mean_zero <- data.frame(predicted_ALT_mean$tsf)%>%
  mutate(predicted_ALT_mean_percentchange_first = 0)%>%
  mutate(Predicted_DOY_min = firstthawingdegreeday$Doy_min_average)%>%
  rename(tsf = predicted_ALT_mean.tsf)


##merge these dataframes together 

predicted_ALT_mean <- merge(predicted_ALT_mean, predicted_ALT_mean_zero, by = "tsf")

##now build slope and intercept from the predicted x and y points 
predicted_ALT_mean$Predicted_DOY_max <- as.numeric(predicted_ALT_mean$Predicted_DOY_max)
predicted_ALT_mean$Predicted_DOY_min <- as.numeric(predicted_ALT_mean$Predicted_DOY_min)

### reserve engineer the linear equation (plus quadratic term) y = ax^2 +bx + c

##Find a parameter (the quadratic parameter: use the average across the tsf that we do have: cannot predict this shape of the parabula for each other year without more Doy data)
predicted_ALT_2_mean <- predicted_ALT_mean%>%
  mutate(a = case_when(tsf >= 0 & tsf <= 12 ~ a_0_12_mean, 
                       tsf > 12 & tsf <= 52 ~ a_13_52_mean,
                       tsf > 52 ~ 0))%>%
  group_by(tsf)%>%
  mutate(b = ((predicted_ALT_mean_percentchange_last - predicted_ALT_mean_percentchange_first - a*((Predicted_DOY_max^2) - (Predicted_DOY_min^2)))/(Predicted_DOY_max - Predicted_DOY_min)))%>%
  mutate(c = predicted_ALT_mean_percentchange_last - (b*Predicted_DOY_max)-(a*Predicted_DOY_max^2))%>%
  ungroup()

##Create a list of all the DOY we want to predict the change in ALD 
Predicted_DOY_list <- c(100:271)

predicted_ALD_allDOY_mean <- predicted_ALT_2_mean%>%
  expand(tsf, full_seq(Predicted_DOY_list,1))


##Join that list of DOY across all tsf 
predicted_ALD_allDOY_mean <- left_join(predicted_ALD_allDOY_mean, predicted_ALT_2_mean, by = c("tsf"))%>%
  rename(Predicted_Doy = "full_seq(Predicted_DOY_list, 1)")

##Now predict the ALD for every doy for each tsf 

predicted_ALD_allDOY_2_mean <- predicted_ALD_allDOY_mean%>%
  mutate(predicted_ALD_change = (a*(Predicted_Doy^2)) + (b*Predicted_Doy) + c)

write.csv(predicted_ALD_allDOY_2_mean, "Output/UpdateWeibullPercentChange_Range/predicted_ALD_change_lowland_percentchange_mean.csv")

##Graph the predicted change in thaw depth 

ggplot(data = predicted_ALD_allDOY_2_mean, aes(x = Predicted_Doy, y = predicted_ALD_change)) +
  geom_point()+ 
  facet_wrap(~tsf) +xlab("DOY") + ylab("Predicted Change in Thaw Depth (percent change)")

ggsave("Output/UpdateWeibullPercentChange_Range/Predicted_ALD_change_lowland_percentchange_mean.png", height = 10, width= 10)




##Max - High Scenario 
##Now take the predicted end of season ALT depths from the modelled data and add the average end of year DOY 
options(scipen = 999)

predicted_ALT_max <- predicted_thaw_rate_difference_lowland%>%
  rename(predicted_ALT_max_percentchange_last = predicted_max_percentchange_ALD)%>%
  mutate(Predicted_DOY_max = lastthawingdegreeday$Doy_max_average)

##Create a new dataframe with the beginning of season at the average DOY min with the change in thaw depth set to zero 

predicted_ALT_max_zero <- data.frame(predicted_ALT_max$tsf)%>%
  mutate(predicted_ALT_max_percentchange_first = 0)%>%
  mutate(Predicted_DOY_min = firstthawingdegreeday$Doy_min_average)%>%
  rename(tsf = predicted_ALT_max.tsf)


##merge these dataframes together 

predicted_ALT_max <- merge(predicted_ALT_max, predicted_ALT_max_zero, by = "tsf")

##now build slope and intercept from the predicted x and y points 
predicted_ALT_max$Predicted_DOY_max <- as.numeric(predicted_ALT_max$Predicted_DOY_max)
predicted_ALT_max$Predicted_DOY_min <- as.numeric(predicted_ALT_max$Predicted_DOY_min)

### reserve engineer the linear equation (plus quadratic term) y = ax^2 +bx + c

##Find a parameter (the quadratic parameter: use the average across the tsf that we do have: cannot predict this shape of the parabula for each other year without more Doy data)
predicted_ALT_2_max <- predicted_ALT_max%>%
  mutate(a = case_when(tsf >= 0 & tsf <= 12 ~ a_0_12_mean, 
                       tsf > 12 & tsf <= 52 ~ a_0_12_mean,
                       tsf > 52 ~ 0))%>%
  group_by(tsf)%>%
  mutate(b = ((predicted_ALT_max_percentchange_last - predicted_ALT_max_percentchange_first - a*((Predicted_DOY_max^2) - (Predicted_DOY_min^2)))/(Predicted_DOY_max - Predicted_DOY_min)))%>%
  mutate(c = predicted_ALT_max_percentchange_last - (b*Predicted_DOY_max)-(a*Predicted_DOY_max^2))%>%
  ungroup()

##Create a list of all the DOY we want to predict the change in ALD 
Predicted_DOY_list <- c(100:271)

predicted_ALD_allDOY_max <- predicted_ALT_2_max%>%
  expand(tsf, full_seq(Predicted_DOY_list,1))


##Join that list of DOY across all tsf 
predicted_ALD_allDOY_max <- left_join(predicted_ALD_allDOY_max, predicted_ALT_2_max, by = c("tsf"))%>%
  rename(Predicted_Doy = "full_seq(Predicted_DOY_list, 1)")

##Now predict the ALD for every doy for each tsf 

predicted_ALD_allDOY_2_max <- predicted_ALD_allDOY_max%>%
  mutate(predicted_ALD_change = (a*(Predicted_Doy^2)) + (b*Predicted_Doy) + c)

write.csv(predicted_ALD_allDOY_2_max, "Output/UpdateWeibullPercentChange_Range/predicted_ALD_change_lowland_percentchange_max.csv")

##Graph the predicted change in thaw depth 

ggplot(data = predicted_ALD_allDOY_2_max, aes(x = Predicted_Doy, y = predicted_ALD_change)) +
  geom_point()+ 
  facet_wrap(~tsf) +xlab("DOY") + ylab("Predicted Change in Thaw Depth (percent change)")

ggsave("Output/UpdateWeibullPercentChange_Range/Predicted_ALD_change_lowland_percentchange_max.png", height = 10, width= 10)




##Min - High Scenario 
##Now take the predicted end of season ALT depths from the modelled data and add the average end of year DOY 
options(scipen = 999)

predicted_ALT_min <- predicted_thaw_rate_difference_lowland%>%
  rename(predicted_ALT_min_percentchange_last = predicted_min_percentchange_ALD)%>%
  mutate(Predicted_DOY_max = lastthawingdegreeday$Doy_max_average)

##Create a new dataframe with the beginning of season at the average DOY min with the change in thaw depth set to zero 

predicted_ALT_min_zero <- data.frame(predicted_ALT_min$tsf)%>%
  mutate(predicted_ALT_min_percentchange_first = 0)%>%
  mutate(Predicted_DOY_min = firstthawingdegreeday$Doy_min_average)%>%
  rename(tsf = predicted_ALT_min.tsf)


##merge these dataframes together 

predicted_ALT_min <- merge(predicted_ALT_min, predicted_ALT_min_zero, by = "tsf")

##now build slope and intercept from the predicted x and y points 
predicted_ALT_min$Predicted_DOY_max <- as.numeric(predicted_ALT_min$Predicted_DOY_max)
predicted_ALT_min$Predicted_DOY_min <- as.numeric(predicted_ALT_min$Predicted_DOY_min)

### reserve engineer the linear equation (plus quadratic term) y = ax^2 +bx + c

##Find a parameter (the quadratic parameter: use the average across the tsf that we do have: cannot predict this shape of the parabula for each other year without more Doy data)
predicted_ALT_2_min <- predicted_ALT_min%>%
  mutate(a = case_when(tsf >= 0 & tsf <= 12 ~ -0.0000001, 
                       tsf > 12 & tsf <= 52 ~ -0.00000001,
                       tsf > 52 ~ 0))%>%
  group_by(tsf)%>%
  mutate(b = ((predicted_ALT_min_percentchange_last - predicted_ALT_min_percentchange_first - a*((Predicted_DOY_max^2) - (Predicted_DOY_min^2)))/(Predicted_DOY_max - Predicted_DOY_min)))%>%
  mutate(c = predicted_ALT_min_percentchange_last - (b*Predicted_DOY_max)-(a*Predicted_DOY_max^2))%>%
  ungroup()

##Create a list of all the DOY we want to predict the change in ALD 
Predicted_DOY_list <- c(100:271)

predicted_ALD_allDOY_min <- predicted_ALT_2_min%>%
  expand(tsf, full_seq(Predicted_DOY_list,1))


##Join that list of DOY across all tsf 
predicted_ALD_allDOY_min <- left_join(predicted_ALD_allDOY_min, predicted_ALT_2_min, by = c("tsf"))%>%
  rename(Predicted_Doy = "full_seq(Predicted_DOY_list, 1)")

##Now predict the ALD for every doy for each tsf 

predicted_ALD_allDOY_2_min <- predicted_ALD_allDOY_min%>%
  mutate(predicted_ALD_change = (a*(Predicted_Doy^2)) + (b*Predicted_Doy) + c)

write.csv(predicted_ALD_allDOY_2_min, "Output/UpdateWeibullPercentChange_Range/predicted_ALD_change_lowland_percentchange_min.csv")

##Graph the predicted change in thaw depth 

ggplot(data = predicted_ALD_allDOY_2_min, aes(x = Predicted_Doy, y = predicted_ALD_change)) +
  geom_point()+ 
  facet_wrap(~tsf) +xlab("DOY") + ylab("Predicted Change in Thaw Depth (percent change)")

ggsave("Output/UpdateWeibullPercentChange_Range/Predicted_ALD_change_lowland_percentchange_min.png", height = 10, width= 10)
