
library(tidyverse)
library(ggplot2)

BootLakeALT <- read.csv("TransientThaw/Data/BootLakeALT_processed.csv")%>%
  filter(!is.na(pair))

BootLakeALT$Depth.to.Permafrost_cm  <- as.numeric(BootLakeALT$Depth.to.Permafrost_cm)

BootLakeALT_summary <- BootLakeALT%>%
  group_by(TSF, Disturbance, pair)%>%
  summarize(ALT_mean = mean(Depth.to.Permafrost_cm))%>%
  mutate(Control_ALT_mean = case_when(pair== "North" ~ 44.4,
                                      pair== "South" ~ 63.42424,
                                      pair== "Top" ~ 62.78788,
                                      pair== "Toe" ~ 48.44 ))%>%
  filter(Disturbance != "Control")%>%
  mutate(Difference_ALT = ALT_mean - Control_ALT_mean)

TSF_summary <- BootLakeALT_summary%>%
  group_by(TSF)%>%
  summarize(Difference_ALT = mean(Difference_ALT))






