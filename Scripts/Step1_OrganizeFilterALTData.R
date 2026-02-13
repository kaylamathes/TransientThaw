##Library
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(raster)


##Active Layer Depth: Anna Talucci's data 

ALD <- read.csv("TransientThaw/Data/PermafrostFireDatacubeFinal_20240718.csv")

unique(ALD$biome)
###Filtering the datasets for only boreal interior and separating dataset out by disturbed/undisturbed. then turn some columns into numeric 
ALD_sub <- ALD%>%
  filter(biome == "boreal")%>%
  filter(paired == "c1" | paired == "m3" |  paired == "l3" |  paired == "m2" 
         | paired == "m1" |  paired == "l5" | paired == "l4" |  paired == "e4"
         | paired == "l2" | paired == "k1" | paired == "m5" | paired == "m4"
         | paired == "e3" | paired == "i1"| paired == "n1"| paired == "e6"
         | paired == "e8" | paired == "n3"| paired == "n6" | paired == "i2"
         | paired == "n2"| paired == "n5"| paired == "f1"| paired == "n7"
         | paired == "n4" | paired == "e7" | paired == "e1"| paired == "e5"
         | paired == "e2") 




                        

##Creating separate burned and unburned datafiles (IDK if I actually need to do this...)
ALD_sub_disturb <- ALD_sub%>%
  filter(distur == "burned")

ALD_sub_undisturb <- ALD_sub%>%
  filter(distur == "unburned")

##Converting some variables to numeric 
ALD_sub$tsf <- as.numeric(ALD_sub$tsf)
ALD_sub$predDepth <- as.numeric(ALD_sub$predDepth)
ALD_sub$msrDepth <- as.numeric(ALD_sub$msrDepth)


##Save the filtered data

##Create a point datafile 
ALD_sub_sf = st_as_sf(ALD_sub, coords = c("lon", "lat"), crs = 4326)



##save the point datafile as a shapefile
st_write(ALD_sub_sf, "TransientThaw/Output/ALD_sub_sf_expanded.shp", driver ="ESRI Shapefile")

##Create a csv file
write.csv(ALD_sub,"TransientThaw/Output/ALD_boreal_expanded.csv", row.names = FALSE)
