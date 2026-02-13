#### ALT and time since fire 

###

##Library
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(raster)



options(scipen=999)



## Boreal Dataset with Upland vs lowland classification 

ALD_original <- read.csv("TransientThaw/Data/ALD_NA_boreal.csv")
ALD_sub =  list.files(path="TransientThaw/Output/Talucci_Dataset_Upland_lowland_5version/",pattern='*.csv', full.names = TRUE)%>%
  purrr::map(., st_read)%>%
  map_dfr(., ~ .x %>%
            mutate(across(tsf, as.character)))%>%
  bind_rows()


ALD_sub <- merge(ALD_sub, ALD_original, by = plotId:)
ALD_sub_explore <- ALD_sub%>%
  filter(submtNm == "Gibson")%>%
  filter(distur == "burned")
  

#Organize by paired site and adjust the landform based on the highest value

unique(ALD_sub$paired)

ALD_e5 <- ALD_sub%>%
  filter(paired == "e5")%>%
  mutate(landform_Adjusted = "1")

ALD_e1 <- ALD_sub%>%
  filter(paired == "e1")%>%
  mutate(landform_Adjusted = "1")
  
ALD_e7 <- ALD_sub%>%
    filter(paired == "e7")%>%
  mutate(landform_Adjusted = "1") 


ALD_n4 <- ALD_sub%>%
  filter(paired == "n4")%>%
  mutate(landform_Adjusted = "1") 

ALD_n7 <- ALD_sub%>%
  filter(paired == "n7")%>%
  mutate(landform_Adjusted = "1") 

ALD_f1 <- ALD_sub%>%
  filter(paired == "f1")%>%
  mutate(landform_Adjusted = "0") 
  
ALD_n5 <- ALD_sub%>%
    filter(paired == "n5")%>%
  mutate(landform_Adjusted = "1") 
  
ALD_n2 <- ALD_sub%>%
    filter(paired == "n2")%>%
  mutate(landform_Adjusted = "1") 

ALD_i2 <- ALD_sub%>%
  filter(paired == "i2")%>%
  mutate(landform_Adjusted = "1") 

ALD_n6 <- ALD_sub%>%
  filter(paired == "n6")%>%
  mutate(landform_Adjusted = "1") 

ALD_n3 <- ALD_sub%>%
  filter(paired == "n3")%>%
  mutate(landform_Adjusted = "1") 

ALD_e8 <- ALD_sub%>%
  filter(paired == "e8")%>%
  mutate(landform_Adjusted = "1") 

ALD_e6 <- ALD_sub%>%
  filter(paired == "e6")%>%
  mutate(landform_Adjusted = "1") 
  
ALD_n1 <- ALD_sub%>%
    filter(paired == "n1")%>%
  mutate(landform_Adjusted = "1") 

ALD_i1 <- ALD_sub%>%
  filter(paired == "i1")%>%
  mutate(landform_Adjusted = "1") 

ALD_e3 <- ALD_sub%>%
  filter(paired == "e3")%>%
  mutate(landform_Adjusted = "0")

ALD_m4 <- ALD_sub%>%
  filter(paired == "m4")%>%
  mutate(landform_Adjusted = "0")

ALD_m5 <- ALD_sub%>%
  filter(paired == "m5")%>%
  mutate(landform_Adjusted = "0")

ALD_k1 <- ALD_sub%>%
  filter(paired == "k1")%>%
  mutate(landform_Adjusted = "1")

ALD_l2 <- ALD_sub%>%
  filter(paired == "l2")%>%
  mutate(landform_Adjusted = "0")

ALD_e4 <- ALD_sub%>%
  filter(paired == "e4")%>%
  mutate(landform_Adjusted = "0")

ALD_l4 <- ALD_sub%>%
  filter(paired == "l4")%>%
  mutate(landform_Adjusted = "0")

ALD_l5 <- ALD_sub%>%
  filter(paired == "l5")%>%
  mutate(landform_Adjusted = "0")

ALD_m1 <- ALD_sub%>%
  filter(paired == "m1")%>%
  mutate(landform_Adjusted = "1")

ALD_m2 <- ALD_sub%>%
  filter(paired == "m2")%>%
  mutate(landform_Adjusted = "0")

ALD_l3 <- ALD_sub%>%
  filter(paired == "l3")%>%
  mutate(landform_Adjusted = "0")

ALD_m3 <- ALD_sub%>%
  filter(paired == "m3")%>%
  mutate(landform_Adjusted = "0")

ALD_c1 <- ALD_sub%>%
  filter(paired == "c1")%>%
  mutate(landform_Adjusted = "0")

ALD_e2 <- ALD_sub%>%
  filter(paired == "e2")%>%
  mutate(landform_Adjusted = "1")


ALD_sub_landform_adjust <- rbind(ALD_c1,ALD_m3,ALD_l3,ALD_m2,ALD_m1,ALD_l5,ALD_l4,ALD_e4,
                                 ALD_l2,ALD_k1,ALD_m5,ALD_m4,ALD_e3,ALD_i1,ALD_n1,ALD_e6,
                                 ALD_e8,ALD_n3,ALD_n6,ALD_i2,ALD_n2,ALD_n5,ALD_f1,ALD_n7,
                                 ALD_n4,ALD_e7,ALD_e1,ALD_e5, ALD_e2)

ALD_sub_landform_adjust <- st_transform(ALD_sub_landform_adjust, crs = 4326)



##filter out the tundra from the dataset: 

ALD_sub_landform_adjust_boreal <- ALD_sub_landform_adjust%>%
  filter(resBiom != "Tundra")


##Creating separate burned and unburned datafiles (IDK if I actually need to do this...)
ALD_sub_disturb <- ALD_sub_landform_adjust_boreal%>%
  filter(distur == "burned")

ALD_sub_undisturb <- ALD_sub_landform_adjust_boreal%>%
  filter(distur == "unburned")



##########(First attempt at summarizing the dataset): Determine plot-level mean predicted burn depth for burned and unburned sites. The plot is going ####
##to be the smallest unit of measure 
##burned
ALD_sub_disturb_plot <- ALD_sub_disturb%>%
  group_by(paired,siteId, year, lastNm, fireYr, tsf, landform_Adjusted, prmExtn, lat, lon)%>%
  summarise(predDepth = mean(prdDpth))%>%
  ungroup()



##unburned
ALD_sub_undisturb_plot <- ALD_sub_undisturb%>%
  group_by(paired, siteId,year, lastNm,fireYr, tsf, landform_Adjusted, prmExtn)%>%
  summarise(predDepth = mean(prdDpth))%>%
  ungroup()


###Summarized to the paired ID, year, fireYR and time since fire (Create mean, min and max values for each paired ID)
##burned
ALD_sub_disturb_summary <- ALD_sub_disturb_plot%>%
  group_by(paired, fireYr,year, tsf,lastNm, landform_Adjusted,prmExtn)%>%
  summarise(mean_predDepth_burned = mean(predDepth), max_predDepth_burned = max(predDepth), min_predDepth_burned = min(predDepth))


##unburned(summarized to the paired ID and year)
ALD_sub_undisturb_summary <- ALD_sub_undisturb_plot%>%
  group_by(paired, fireYr, year, lastNm, landform_Adjusted, prmExtn)%>%
  summarise(mean_predDepth_unburned = mean(predDepth), max_predDepth_unburned = max(predDepth), min_predDepth_unburned = min(predDepth))


##merge the dataframes by paired ID and year 
ALD_summary_merge <- merge(ALD_sub_undisturb_summary, ALD_sub_disturb_summary, by = c( "paired",  "lastNm", "landform_Adjusted", "prmExtn", "year"))

##Calculate the absolute difference and the relative percent change 
ALD_summary_merge <- ALD_summary_merge%>%
  mutate(change_meanpredDepth_cm = mean_predDepth_burned - mean_predDepth_unburned)%>%
  mutate(change_maxpredDepth_cm = max_predDepth_burned - max_predDepth_unburned)%>%
  mutate(change_minpredDepth_cm = min_predDepth_burned - min_predDepth_unburned)%>%
  mutate(percentChange_meanpredDepth_cm = ((mean_predDepth_burned - mean_predDepth_unburned)/mean_predDepth_unburned)*100)%>%
  mutate(percentChange_maxpredDepth_cm = ((max_predDepth_burned - max_predDepth_unburned)/max_predDepth_unburned)*100)%>%
  mutate(percentChange_minpredDepth_cm = ((min_predDepth_burned - min_predDepth_unburned)/min_predDepth_unburned)*100) 

ALD_summary_merge_sub <- ALD_summary_merge%>%
  dplyr::select(paired, landform_Adjusted, prmExtn, year, tsf, change_meanpredDepth_cm, change_maxpredDepth_cm, change_minpredDepth_cm, percentChange_meanpredDepth_cm,
                percentChange_minpredDepth_cm,percentChange_maxpredDepth_cm,lastNm)
ALD_summary_merge_sub$tsf <- as.numeric(ALD_summary_merge_sub$tsf)

###Include only positive values
ALD_summary_merge_pos <- ALD_summary_merge_sub%>%
  filter(change_meanpredDepth_cm >0)

####make the time since fire numeric 
ALD_summary_merge_pos$tsf <- as.numeric(ALD_summary_merge_pos$tsf)


##Include only the discontinous permafrost results 
ALD_summary_merge_D <- ALD_summary_merge%>%
  filter(prmExtn == "D")

ALD_summary_merge_pos_D <- ALD_summary_merge_pos%>%
  filter(prmExtn == "D")

##Get rid of the extreme points: 
ALD_summary_merge_pos_D_noextreme <-  ALD_summary_merge_pos_D%>%
  filter(change_meanpredDepth_cm <200)

##Combined upland and lowland figure 
ggplot(data = ALD_summary_merge_pos_D_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm, color = landform_Adjusted)) +
  geom_point(data = ALD_summary_merge_pos_D_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm))+
  xlab("Time since fire (years)") + ylab("Difference in Predicted Active Layer Thickness (cm)")+
 #geom_errorbar(aes(ymin=change_minpredDepth_cm, ymax=change_maxpredDepth_cm), width=.2,
               # position=position_dodge(.9)) +
  theme_classic()

ggplot(data = ALD_summary_merge, aes(x = tsf, y = percentChange_meanpredDepth_cm, color = prmExtn)) +
  geom_point(data = ALD_summary_merge_pos, aes(x = tsf, y = percentChange_meanpredDepth_cm))+
  xlab("Time since fire (years)") + ylab("Difference in Predicted Active Layer Thickness (cm)")+
  #geom_errorbar(aes(ymin=change_minpredDepth_cm, ymax=change_maxpredDepth_cm), width=.2,
  # position=position_dodge(.9)) +
  theme_classic()
 
ggsave("TransientThaw/Output/Talucci_explore_boreal_total.png")


####Adding addition data from Zhu et al. 2023: 

tsf <- c(4,5,10,20,30,40,50,6,6,9,36,17,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

change_ALT_cm <- c(80,35,20,9,10,8,5,80,45.5,140,50,6,39,56,74,76,66,61,64,62,61,60,56,62,69,61,64,65,65,18,36,64.5,64.9,74,62,49,59,49,67,68,59.78,60,58,64,60,73)
reference <- c("Nossov et al. 2013", "Gibson et al. 2018","Gibson et al. 2018","Gibson et al. 2018",
               "Gibson et al. 2018","Gibson et al. 2018","Gibson et al. 2018","Fisher et al. 2016", 
               "Zhang et al. 2015", "Viereck et al. 1982", "Viereck et al. 2008", "Barret et al. 2012", 
               "Smith et al. 2015","Smith et al. 2015","Smith et al. 2015", "Smith et al. 2015", 
               "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", 
               "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", 
               "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", 
               "Smith et al. 2015","Smith et al. 2015", "Smith et al. 2015","Smith et al. 2015", 
               "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", 
               "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", 
               "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", "Smith et al. 2015", 
               "Smith et al. 2015", "Smith et al. 2015") 
      

Zhu_dataset <- data.frame(tsf, change_ALT_cm, reference)


ggplot(Zhu_dataset, aes(x = tsf, y = change_ALT_cm))+
  geom_point()
###Upland figure 
##Break the dataframe into upland vs lowland 
ALD_summary_merge_sub_upland <- ALD_summary_merge_D%>%
  filter(landform_Adjusted == 0)

ALD_summary_merge_sub_lowland <- ALD_summary_merge_D%>%
  filter(landform_Adjusted == 1)

##Changes made to uplands: Eliminated the two highest points because these are evidence of abrupt thaw (Douglas and Jorgenson)
ALD_summary_merge_sub_upland_noextreme <- ALD_summary_merge_sub_upland%>%
  filter(change_meanpredDepth_cm <200)

##Exploring using the percent change as a opposed to absolute difference in burned vs unburned

ALD_summary_merge_sub_upland_noextreme$tsf <- as.numeric(ALD_summary_merge_sub_upland_noextreme$tsf)
ggplot(data = ALD_summary_merge_sub_upland_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm, color = lastNm)) +
  geom_point(data = ALD_summary_merge_sub_upland_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm))+
  xlab("Time since fire (years)") + ylab("Relative Change in Predicted Active Layer Thickness (Percent Change)")+
  #geom_errorbar(aes(ymin=change_minpredDepth_cm, ymax=change_maxpredDepth_cm), width=.2,
  # position=position_dodge(.9)) +
  theme_classic()

##Lowland (Dielman, Baltzer and Turetsky: Eliminated these points from lowlands because they are evidence of talik formation? 
ALD_summary_merge_sub_lowland_noextreme <- ALD_summary_merge_sub_lowland%>%
  filter(lastNm != "Dieleman_Baltzer_Turetsky")

ggplot(data = ALD_summary_merge_sub_lowland_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm, color = lastNm)) +
  geom_point(data = ALD_summary_merge_sub_lowland_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm))+
  xlab("Time since fire (years)") + ylab("Relative Change in Predicted Active Layer Thickness (Percent Change)")+
  #geom_errorbar(aes(ymin=change_minpredDepth_cm, ymax=change_maxpredDepth_cm), width=.2,
  # position=position_dodge(.9)) +
  theme_classic()


ALD_summary_merge_sub_lowland_e6 <- ALD_summary_merge_sub_lowland%>%
  filter(paired == "e6"| paired == "i1")


###### WEIBULL CURVE ##### 

###Lowland (Absolute change)
###fitting a Weibull distribution
ALD_weibull_lowland <- ALD_summary_merge_sub_lowland%>%
  filter(change_meanpredDepth_cm >0)%>%
  filter(tsf > 0)%>%
  dplyr::select(tsf, change_meanpredDepth_cm)

##Additions to the dataset include a 0.001 instead of 0 tsf because weilbull can't take zero. 
##The datapoints are substitutions for the negative difference in burn and unburned, subsituting them for zero. 
ALD_summary_addition_lowland <- data.frame(tsf = c(1,1,3,2,40,2),
change_meanpredDepth_cm = c(0.01,0.01,0.01,0.01,0.01,0.01))

ALD_weibull_addition_lowland <- rbind(ALD_summary_addition_lowland, ALD_weibull_lowland)

ALD_weibull_addition_lowland$tsf <- as.numeric(ALD_weibull_addition_lowland$tsf)
ALD_weibull_addition_lowland$change_meanpredDepth_cm <- as.numeric(ALD_weibull_addition_lowland$change_meanpredDepth_cm)

# Your data
ALD_weibull_addition_lowland_means <- ALD_weibull_addition_lowland%>%
  mutate(period = case_when(tsf <= 2 ~ 1,
                            tsf > 2 & tsf <=10 ~ 7.5, 
                            tsf > 10 & tsf <= 30 ~ 20, 
                            tsf > 30 ~ 45))%>%
  group_by(period)%>%
  summarize(mean_changeALT_cm = mean(change_meanpredDepth_cm),max_changeALT_cm = max(change_meanpredDepth_cm),min_changeALT_cm = min(change_meanpredDepth_cm) )

x <- as.numeric(ALD_weibull_addition_lowland_means$period)
y <-as.numeric(ALD_weibull_addition_lowland_means$mean_changeALT_cm )

x <- as.numeric(ALD_weibull_addition_lowland_means$period)
y_max <-as.numeric(ALD_weibull_addition_lowland_means$max_changeALT_cm )
y_min <-as.numeric(ALD_weibull_addition_lowland_means$min_changeALT_cm )


install.packages("fitdistrplus")
library(fitdistrplus)



# Fit the Weibull distribution to 'x'
fit_lowland <- fitdist(x,"weibull")


# Plot the fitted Weibull distribution
plot(fit_lowland)


# Display the summary of the fit
summary(fit_lowland )




# Get the shape and scale parameters
shape <- fit_lowland $estimate["shape"]
scale <-fit_lowland $estimate["scale"]

x_new <- c(0:50) 



# Predicted y values (PDF): The shape is altered based off apriori understanding of change in ALT, scale stays the same
y_pred_pdf <- dweibull(x_new , shape = 1.5, scale = scale)


# Estimate the scaling factor (k): We restrained the peak model tragetory based off the average change in Active layer depth during the first 12 years (Schaedel et al. approach ). 12 years was picked due to peak in the dataset

##Calculating the average 

ALD_weibull_addition_lowland_12_average <- ALD_weibull_addition_lowland%>%
  filter(tsf<= 12)%>%
  summarize(average = mean(change_meanpredDepth_cm)) 

y_adjusted = ALD_weibull_addition_lowland_12_average$average

scaling_factor2 <- max(y) / max(y_pred_pdf)
scaling_factor2_max <- max(y_max) / max(y_pred_pdf)
scaling_factor2_min <- max(y_min) / max(y_pred_pdf)

# Translate the density values to the predicted 'y' values
y_pred <- y_pred_pdf * scaling_factor2
y_pred_max <- y_pred_pdf * scaling_factor2_max
y_pred_min <- y_pred_pdf * scaling_factor2_min

prediction <- data.frame(x = x_new, y_pred_pdf = y_pred)
prediction_max <- data.frame(x = x_new, y_pred_pdf = y_pred_max)
prediction_min <- data.frame(x = x_new, y_pred_pdf = y_pred_min)

ALD_summary_merge_sub_lowland$tsf <- as.numeric(ALD_summary_merge_sub_lowland$tsf)

ggplot(data = ALD_weibull_addition_lowland, aes(x = tsf, y = change_meanpredDepth_cm)) +
  geom_line(data = prediction, aes(x = x, y = y_pred),color = "darkblue")+
  geom_line(data = prediction_max, aes(x = x, y = y_pred_max),color = "darkblue", linetype = 2)+
  geom_line(data = prediction_min, aes(x = x, y = y_pred_min),color = "darkblue", linetype = 2)+
  geom_point(data = , linetype = 2, aes(x = tsf, y = change_meanpredDepth_cm))+
  xlab("Time since fire (years)") + ylab("Difference in Predicted Active Layer Thickness (cm)")+
  theme_classic()
  
ggsave("TransientThaw/Output/timesincefire_fittedcurve_lowland_UPDATED_2.png")

###Write the predicted ALT across TSF to a csv file 
Weibull_model_output_lowland <- prediction%>%
  rename(tsf = x)%>%
  rename(predicted_mean_change_ALD_cm = y_pred_pdf)

write_csv(Weibull_model_output_lowland, "TransientThaw/Output/WeibullModelOutput_Lowland.csv")


#######Lowland (Percent Change)
###fitting a Weibull distribution
ALD_weibull_lowland <- ALD_summary_merge_sub_lowland_noextreme%>%
  filter(percentChange_meanpredDepth_cm >0)%>%
  #filter(tsf > 0)%>%
  dplyr::select(tsf, percentChange_meanpredDepth_cm)

##Additions to the dataset include a 0.001 instead of 0 tsf because weilbull can't take zero. 
##The datapoints are substitutions for the negative difference in burn and unburned, subsituting them for zero. 
ALD_summary_addition_lowland <- data.frame(tsf = c(0.001, 1,1,2,3,3,13,33,49),
                                           percentChange_meanpredDepth_cm = c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01))

ALD_weibull_addition_lowland <- rbind(ALD_summary_addition_lowland, ALD_weibull_lowland)

# Your data

x <- ALD_weibull_addition_lowland$tsf 
y <-ALD_weibull_addition_lowland$percentChange_meanpredDepth_cm 

library(fitdistrplus)



# Fit the Weibull distribution to 'x'
fit_lowland <- fitdist(x,"weibull")


# Plot the fitted Weibull distribution
plot(fit_lowland)


# Display the summary of the fit
summary(fit_lowland )




# Get the shape and scale parameters
shape <- fit_lowland $estimate["shape"]
scale <-fit_lowland $estimate["scale"]

x_new <- c(0:50) 



# Predicted y values (PDF): The shape is altered based off apriori understanding of change in ALT, scale stays the same
y_pred_pdf <- dweibull(x_new , shape = 1.5, scale = scale)


# Estimate the scaling factor (k): We restrained the peak model tragetory based off the average change in Active layer depth during the first 12 years (Schaedel et al. approach ). 12 years was picked due to peak in the dataset

##Calculating the average 

ALD_weibull_addition_lowland_12_average <- ALD_weibull_addition_lowland%>%
  filter(tsf<= 12)%>%
  summarize(average = mean(percentChange_meanpredDepth_cm)) 

y_adjusted = ALD_weibull_addition_lowland_12_average$average

scaling_factor2 <- max(y_adjusted) / max(y_pred_pdf)

# Translate the density values to the predicted 'y' values
y_pred <- y_pred_pdf * scaling_factor2

prediction <- data.frame(x = x_new, y_pred_pdf = y_pred)


ggplot(data = ALD_summary_merge_sub_lowland_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm)) +
  geom_line(data = prediction, aes(x = x, y = y_pred),color = "darkblue")+
  geom_point(data = ALD_summary_merge_sub_lowland_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm))+
  xlab("Time since fire (years)") + ylab("Relative Change in ALT post-fire (% Change)")+
  geom_hline(yintercept = y_adjusted, linetype = 2)+
  theme_classic()

ggsave("TransientThaw/Output/timesincefire_fittedcurve_lowland_UPDATED_percentChange.png")

###Write the predicted ALT across TSF to a csv file 
Weibull_model_output_lowland <- prediction%>%
  rename(tsf = x)%>%
  rename(predicted_mean_change_ALD_percentchange = y_pred_pdf)

write_csv(Weibull_model_output_lowland, "TransientThaw/Output/WeibullModelOutput_Lowland_percentChange.csv")



#### Upland #####

####Following Zhu et al. 2023 protocol, find the mean relative change in ALT during the periods: 0-5, 5-10, 10-20, 20-30,30-40-40-50
###fitting a Weibull distribution

ALD_weibull_upland <- ALD_summary_merge_sub_upland_noextreme%>%
  filter(change_meanpredDepth_cm >0)%>%
  filter(tsf > 0)%>%
  dplyr::select(tsf, change_meanpredDepth_cm)

##Additions to the dataset include a 0.001 instead of 0 tsf because weilbull can't take zero. 
##The datapoints are substitutions for the negative difference in burn and unburned, subsituting them for zero. 
ALD_summary_addition_upland <- data.frame(tsf = c(0.01,2),
                                           change_meanpredDepth_cm = c(14.6,0.01))

ALD_weibull_addition_upland <- rbind(ALD_summary_addition_upland, ALD_weibull_upland)
ALD_weibull_addition_upland$tsf <- as.numeric(ALD_weibull_addition_upland$tsf)

# Try time periods 
ALD_weibull_addition_upland_means <- ALD_weibull_addition_upland%>%
  mutate(period = case_when(tsf <= 1 ~ 1,
                            tsf > 1 & tsf <=10 ~ 7.5, 
                            tsf > 10 & tsf <= 30 ~ 20, 
                            tsf > 30 ~ 45))%>%
  group_by(period)%>%
  summarize(mean_changeALT_cm = mean(change_meanpredDepth_cm),max_changeALT_cm = max(change_meanpredDepth_cm),min_changeALT_cm = min(change_meanpredDepth_cm) )

x <- as.numeric(ALD_weibull_addition_upland_means$period)
y <-as.numeric(ALD_weibull_addition_upland_means$mean_changeALT_cm )

x <- as.numeric(ALD_weibull_addition_upland_means$period)
y_max <-as.numeric(ALD_weibull_addition_upland_means$max_changeALT_cm )
y_min <-as.numeric(ALD_weibull_addition_upland_means$min_changeALT_cm )

####Using The Zhu et al. 2023 method to calculate the weibull curve 
#####Following Zhu et al. 2023 protocol, find the mean relative change in ALT during the periods: 0-5, 5-10, 10-20, 20-30,30-40-40-50
# Your data

##Try time periods 
##x <- c(5,15,40)
##x <- ALD_weibull_addition_upland$tsf 

##y <-ALD_weibull_addition_upland$change_meanpredDepth_cm 


# Fit the Weibull distribution to 'x'
fit_upland <- fitdist(x,"weibull")


# Plot the fitted Weibull distribution
plot(fit_upland)


# Display the summary of the fit
summary(fit_upland )




# Get the shape and scale parameters
shape <- fit_upland$estimate["shape"]
scale <-fit_upland$estimate["scale"]

x_new <- c(0:50) 



# Predicted y values (PDF): The shape is altered based off apriori understanding of change in ALT, scale stays the same
y_pred_pdf <- dweibull(x_new , shape = 1.5, scale = scale)


# Estimate the scaling factor (k): We restrained the peak model tragetory based off the average change in Active layer depth during the first 10 years (Schaedel et al. approach )

##Calculating the average 

ALD_weibull_addition_upland_10_average <- ALD_weibull_addition_upland%>%
  filter(tsf<= 10)%>%
  summarize(average = mean(change_meanpredDepth_cm)) 

y_adjusted = ALD_weibull_addition_upland_10_average$average

#scaling_factor2 <- max(y_adjusted) / max(y_pred_pdf)
scaling_factor2 <- max(y) / max(y_pred_pdf)
scaling_factor2_max <- max(y_max) / max(y_pred_pdf)
scaling_factor2_min <- max(y_min) / max(y_pred_pdf)

# Translate the density values to the predicted 'y' values
y_pred <- y_pred_pdf * scaling_factor2
y_pred_max <- y_pred_pdf * scaling_factor2_max
y_pred_min <- y_pred_pdf * scaling_factor2_min

prediction <- data.frame(x = x_new, y_pred_pdf = y_pred)
prediction_max <- data.frame(x = x_new, y_pred_pdf = y_pred_max)
prediction_min <- data.frame(x = x_new, y_pred_pdf = y_pred_min)

# Translate the density values to the predicted 'y' values
y_pred <- y_pred_pdf * scaling_factor2

prediction <- data.frame(x = x_new, y_pred_pdf = y_pred)


ggplot(data = ALD_weibull_addition_upland, aes(x = tsf, y = change_meanpredDepth_cm)) +
  geom_line(data = prediction, aes(x = x, y = y_pred),color = "yellow4")+
  geom_line(data = prediction_max, aes(x = x, y = y_pred_max),color = "yellow4", linetype = 2)+
  geom_line(data = prediction_min, aes(x = x, y = y_pred_min),color = "yellow4",, linetype = 2)+
  geom_point(data = ALD_weibull_addition_upland, aes(x = tsf, y = change_meanpredDepth_cm))+
  xlab("Time since fire (years)") + ylab("Difference in Predicted Active Layer Thickness (cm)")+
  #geom_hline(yintercept = y_adjusted, linetype = 2)+
  theme_classic()

ggsave("TransientThaw/Output/timesincefire_fittedcurve_upland_UPDATED_2.png")


###Write the predicted ALT across TSF to a csv file 
Weibull_model_output_upland <- prediction%>%
  rename(tsf = x)%>%
  rename(predicted_mean_change_ALD_cm = y_pred_pdf)

write_csv(Weibull_model_output_upland, "TransientThaw/Output/WeibullModelOutput_Upland.csv")


#### Upland (percent Change) #####
###fitting a Weibull distribution
ALD_weibull_upland <- ALD_summary_merge_sub_upland_noextreme%>%
  filter(percentChange_meanpredDepth_cm >0)%>%
  filter(tsf > 0)%>%
  dplyr::select(tsf, percentChange_meanpredDepth_cm)

##Additions to the dataset include a 0.001 instead of 0 tsf because weilbull can't take zero. 
##The datapoints are substitutions for the negative difference in burn and unburned, subsituting them for zero. 
ALD_summary_addition_upland <- data.frame(tsf = c(0.01,2, 2,13,40),
                                          percentChange_meanpredDepth_cm = c(24.6,0.01,0.01,0.01,0.01))

ALD_weibull_addition_upland <- rbind(ALD_summary_addition_upland, ALD_weibull_upland)

# Your data

x <- ALD_weibull_addition_upland$tsf 
y <-ALD_weibull_addition_upland$percentChange_meanpredDepth_cm 


# Fit the Weibull distribution to 'x'
fit_upland <- fitdist(x,"weibull")


# Plot the fitted Weibull distribution
plot(fit_upland)


# Display the summary of the fit
summary(fit_upland )




# Get the shape and scale parameters
shape <- fit_upland$estimate["shape"]
scale <-fit_upland$estimate["scale"]

x_new <- c(0:50) 



# Predicted y values (PDF): The shape is altered based off apriori understanding of change in ALT, scale stays the same
y_pred_pdf <- dweibull(x_new , shape = 1.5, scale = scale)


# Estimate the scaling factor (k): We restrained the peak model tragetory based off the average change in Active layer depth during the first 10 years (Schaedel et al. approach )

##Calculating the average 

ALD_weibull_addition_upland_10_average <- ALD_weibull_addition_upland%>%
  filter(tsf<= 10)%>%
  summarize(average = mean(percentChange_meanpredDepth_cm)) 

y_adjusted = ALD_weibull_addition_upland_10_average$average

scaling_factor2 <- max(y_adjusted) / max(y_pred_pdf)

# Translate the density values to the predicted 'y' values
y_pred <- y_pred_pdf * scaling_factor2

prediction <- data.frame(x = x_new, y_pred_pdf = y_pred)


ggplot(data = ALD_summary_merge_sub_upland_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm)) +
  geom_line(data = prediction, aes(x = x, y = y_pred),color = "yellow4")+
  geom_point(data = ALD_summary_merge_sub_upland_noextreme, aes(x = tsf, y = percentChange_meanpredDepth_cm))+
  xlab("Time since fire (years)") + ylab("Relative Change in ALT post-fire (% Change)")+
  geom_hline(yintercept = y_adjusted, linetype = 2)+
  theme_classic()

ggsave("TransientThaw/Output/timesincefire_fittedcurve_upland_UPDATED_percentchange.png")


###Write the predicted ALT across TSF to a csv file 
Weibull_model_output_upland <- prediction%>%
  rename(tsf = x)%>%
  rename(predicted_mean_change_ALD_percent_change = y_pred_pdf)

write_csv(Weibull_model_output_upland, "TransientThaw/Output/WeibullModelOutput_Upland_percentchange.csv")








######OLD SCRIPT#####
Weibull_model_output <- prediction%>%
  rename(tsf = x)%>%
  rename(predicted_mean_change_ALD_cm = y_pred_pdf)

write_csv(Weibull_model_output, "TransientThaw/Output/WeibullModelOutput_mean_ALT.csv")




ALD_weibull_addition_10 <- ALD_weibull_addition%>%
  filter(tsf <= 10)




  curve(
    dweibull(x, shape, scale),
    from = 0,
    to = 50,
    main = "Weibull Distribution",
    xlab = "x",
    ylab = "Density",
    col = "blue",
    lwd = 2
  )


  # Your provided data
  x <- c(2.000, 4.000, 1.000, 2.000, 3.000, 4.000, 32.000, 14.000, 2.000, 8.000, 15.000, 3.000, 14.000, 10.000)
  y <- c(54.93486, 13.12500, 50.19267, 85.22004, 100.95248, 106.08264, 35.00000, 29.76172, 21.08332, 254.67355, 38.06989, 26.60215, 43.10000, 226.78352)

  
  # Combine into a data frame
  data <- data.frame(x, y)
  
  data$log_x <- log(data$x)
  
  # Plot the data
  plot(data$x, data$y, main = "Scatter Plot of Data", xlab = "x", ylab = "y", pch = 19, col = "blue")
  
  model <- glm(y ~ log_x, data = data, family = gaussian) 
  
 scale <- exp(model$coefficients[1])
 shape <-  1 / model$coefficients
 
 x_range <- seq(min(data$x), max(data$x), length.out = 100)
 y_fitted <- dweibull(x_range, shape = 1 / model$coefficients[2], scale = exp(model$coefficients[1])) 
 plot(data$x, data$y, pch = 16)
 plot(x_range, y_fitted, col = "red", lwd = 2) 
 
 
 
 fit <- nls(
   y ~ dweibull(x, shape = shape, scale = scale), 
   data = data,
   start = list(shape = 1, scale = )
 )
 fit
 