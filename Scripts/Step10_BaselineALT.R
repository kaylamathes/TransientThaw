library(terra)
library(raster)
library(ncdf4)


##ABoVE Active Layer Data product: Yi et al. 2018

##https://d3o6w55j8uz1ro.cloudfront.net/s3-d0f68fa49c8cba12794bb586349f2341/ornl-cumulus-prod-public.s3.us-west-2.amazonaws.com/above/Sat_ActiveLayer_Thickness_Maps/comp/Sat_ActiveLayer_Thickness_Maps.pdf?A-userid=None&Expires=1766421500&Signature=WENdxlaqnWtkiSWE7hIzGjXephaIV4Mtv4MBcXCf4v9GT4cMlV-z3wCJhf85x9so8SsXH~4t0OtEsh8fgkZytytbzb8-WQAyWOTkZj1ozhp60G6aXo-LpymA96F4sSOjBrsKsfugaqdbbQ8H5vDk4p9HpPdANwowWFU0FSOsv6TYGIKBt4BMLlCtA5vL2s6RgFYMgVxJ5iXGw52i5p6Z-l02Knq0G7wnAoTfZe5KWv3P-BhFLVb~lG-E16b76QzYrxEOK8~qEzNf-Qn0LTQ8vmMyWEEf6EGgtbqPrXWiPljAZoYXNmEqJarsTI27P5NSuR7EfCKzCf9Mfj8F1fSaVQ__&Key-Pair-Id=K27DT4M1A26S8M

##Open the NC file file and save as a tif file

##Upload the tif file into GEE
##ALT
ALT_Alaska <- raster("/Users/kmathes/Desktop/Sat_ActiveLayer_Thickness_Maps_1760/data/Alaska_active_layer_thickness_1km_2001-2015.nc4")
print(ALT_Alaska)

writeRaster(ALT_Alaska, "/Users/kmathes/Desktop/PermafrostThaw/ALT_Alaska.tif")

