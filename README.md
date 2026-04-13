# TransientThaw

### 1. Determine area that will undergo transient thaw (spatially explicit)
Area that didn’t undergo other types of thaw (Will depend on outcomes from the other thaw types. 

Separate remaining area into Upland and Lowland 
      Upland vs lowland
      Method: Using TPI and slope thresholds (Genet’s guidance): 
      Upland: Slope > 5  OR TPI > 5 OR TPI < -5 
      Lowland: -5 < Topographic Position Index < 5

Determine which landform type by burn point: 
Script: EE: Step 07_Landform_determination: 
Inputs: Burn points from all counterfactual fires: 
Location: GEE Assets
Output: csvs for landform determination (Lowland = 0, Upland = 1) for each FABDEM tile. 
Location: Google cloud bucket (TransientThaw/Landform_classification_counterfactual_perimeter/)

Combine all burn point landform classification files into one upland and one lowland csv (burn point) for each fire 
Script: R: Step07_Landform_classification
Inputs: csvs for landform determination (Lowland = 0, Upland = 1) for each FABDEM tile
Location: Google cloud bucket (TransientThaw/Landform_classification_counterfactual_perimeter/)
Outputs: CSV for upland and lowland burn points for each fire. 
Location:TransientThaw/Data/Counterfactual_landform_classification/

Run a spatial intersection to determine the area of upland and lowland for each counterfactual fire perimeter 
Script: R: Step08_Landform_Intersection_ensembleperimeters 

Inputs: CSV for upland and lowland burn points for each fire and valid counterfactual ensemble perimeter shapefiles.
Location: TransientThaw/Data/Counterfactual_landform_classification/ 
Location:  FsPro_Runs/

Outputs: 
Partial geometry files for the counterfactual fires by upland and lowlands (This is to help with the processing time for this task) 
Location:"TransientThaw/Data/Counterfactual_landform/Partial_shapefiles/
CVS with the complete counterfactual perimeters breakdown of upland and lowland area 
Location: "TransientThaw/Data/Counterfactual_landform/”

Status: In progress


### 2. Model the fire-caused change in active layer thickness through year since fire (Not spatially explicit) 
Filter all the Talucci dataset to include just the NA boreal: 
 Script: R: Step 1_OrganizeFilterALTData.R
 Input: Talucci et al. 2025 Dataset
           Location:  “Data/PermafrostFireDatacubeFinal_2024817.csv” 
 Output: Subset of this dataset (no other alterations) 
Location: Output/ALD_boreal_expanded.csv” 

Classify upland and lowland for each of the Talucci dataset points (This is the same process as determining the upland and lowland for the fire perimeters) 
 Script: GEE: Upland_Lowland_Classification
 Input: Subset of Talucci dataset (step2a) 
Location: Output/ALD_boreal_expanded.csv
 Output: Classify each of the Talucci points by upland and lowland 
Location: TransientThaw/Output/Talucci_Dataset_Upland_Lowland_5version

Talucci et al. data fitted to a weibull curve one for upland and one for lowland. 
This has been updated to include means, max and mins percent change over time periods (year 1, years 2-10, 11-30, >30). The low, medium and high scenarios will incorporate the min, mean, max curves respectively. 

Script: R: TransientThaw/Step04_ALT_TimeSinceFire 
Inputs: All FABDEM separated fies from Talucci’s data set in the NA boreal with a landform type (Used the GEE script from step 1 for this) 
Location: TransientThaw/Output/Talucci_Dataset_Upland_Lowland_5version
Outputs: Output for the model on percent change in ALT for all years since fire (50) for upland and lowland (min, mean and max) 
Location: "Output/UpdateWeibullPercentChange_Range/WeibullModelOutput_Upland_percentChange_Range.csv" AND
"Output/UpdateWeibullPercentChange_Range/WeibullModelOutput_Lowland_percentChange_Range.csv"
 			
Status: Done

### 3. Model daily percent changes in fire-cause thaw depth (Not spatially explicit) 
Determine the daily mean temperature for each of the Talucci’s data point from the year of measurement using ERA5 reanalysis 
Scripts: GEE: Step_02_ERA5_dailytemp 
Inputs: The boreal NA datapoints from Talucci dataset (Step 2a) 
	Location: Output/ALD_boreal_expanded.csv” 
Outputs: Daily ERA5 temperature for each year from the Talucci Datset
	Location: TransientThaw/Output
 /ERA5_dailyTemperature_EE_expanded”

Clean the ERA5 temperature data for the Talucci dataset and merge into a single CSV file 
	Script: R: Step_03_ERA5CleanOrganizeData_Mathes
	Input: All the ERA5 temp data files from the GEE code in step 3.a 
Location: TransientThaw/Output
 /ERA5_dailyTemperature_EE_expanded”
Output: Merged ERA temperature data for the Talucci dataset:  
Location:TransientThaw/Output/ERA5_dailyTemperature_EE_expanded/ERA_dailyTemperature_expanded_summary.csv


Derive daily thawing season relative fire-cause percent change in thaw depths for both upland and lowland R script: 

Script: R: Step05s_DerviveALD_DailyTemperature_upland/lowland 
Inputs:
I.  Daily ERA5 temperature summary for each of the Talucci data 
points. 
Location: TransientThaw/Output/ERA5_dailyTemperature_EE_expanded/ERA_dailyTemperature_expanded_summary.csv (Step 3.b) 
II. Modeled percent change in annual ALT for every year since fire 
from weibull distribution by upland/lowland
	Location: Output/UpdateWeibullPercentChange_Range/
WeibullModelOutput_upland_percentChange_Range.csv and 
Output/UpdateWeibullPercentChange_Range/
WeibullModelOutput_Lowland_percentChange_Range.csv 

Outputs: Output for the model on percent change in ALT for all years since fire (50) for upland and lowland (min, mean and max) 
Location: "Output/UpdateWeibullPercentChange_Range/WeibullModelOutput_Upland_percentChange_Range.csv" AND
"Output/UpdateWeibullPercentChange_Range/WeibullModelOutput_Lowland_percentChange_Range.csv"
 			
Status: Done

### 4. Calculate daily volume of thawed permafrost for up to 50 years post fire 
Find baseline Active layer thickness using [Yi et al. 2018]

 Convert to tif file that can be ingested into GEE 
Script: R script: Step10_BaselineALT
Input: Downloaded raster file of Alaska ALT (Yi et al. 2018) 
		Location: Data/ Sat_ActiveLayer_Thickness_Maps_1760/data/
Alaska_active_layer_thickness_1km_2001-2015.nc4
Output: Tif file 
		Location: “PermafrostThaw/ALT_Alaska.tif"

 Counterfactual intersections with the average baseline ALT for each perimeter 
 Script: EE: ALT_Counterfactual_Intersection	
 Input: 
I. ALT tif of Alaska (Step 4a.1) 
  Location: PermafrostThaw/ALT_Alaska.tif
II. Counterfactual fire perimeter ensembles valid 
	Location: FSpro_Runs 

Output: average baseline ALT intersections of all counterfactual fire perimeters by fire 
	Location: TransientThaw/Data/Counterfactual_ALT_Baseline/ALT_Counterfactual_Intersections/

		
Status: Done

Calculate the daily thaw deep trajectory for the baseline ALT for each counterfactual perimeter using the non-linear equation in the dataset model (step 3) and the average start and stop growing season dates (step 3) 

Calculate the daily change fire-induced thaw deep for each counterfactual perimeter for up to 50 years post-fire by multiplying step 4c with step 3a. 
			
Calculate daily fire-induced thawed permafrost volume (m3) by multiplying step 4d (m) with upland and lowland fire perimeters for each counterfactual perimeter (m2) 

Script: R: Step09_UPDATED2_DailyVolumeThawedPermafrostUpland.R and Step09_UPDATED_DailyVolumeThawedPermafrostLowland.R

Input: 
the non-spatial percent change in daily active layer depth for 50 years since fire (upland/lowland and min, mean, max) 
		Location: Output/UpdateWeibullPercentChange_Range/
The average baseline ALT for all counterfactual perimeters for each fire 
				Location: 
TransientThaw/Data/Counterfactual_ALT_Baseline
ALT_Counterfactual_Intersections/ (Step 4a.2) 
Upland and lowland perimeter areas for each counterfactual perimeter 
Location: TransientThaw/Data/Counterfactual_landform/

Output: None: Continue with the script 

Status: In progress (Bottled necked with the Intersection and area determination from Upland/Lowland)	 Step 1

### 5: Convert from daily fire-thawed permafrost volume to carbon pool vulnerable to emissions using the Palmtag Carbon density at different depths 

Calculate the average carbon density at different depths (100,200,300) for each counterfactual perimeter 
Script: EE: CarbonDensityIntersection
Input:
 I. Counterfactual fire perimeter ensembles valid 
	Location: FSpro_Runs 
II. Palmtag et al. 2022 carbon density tif files 
	Location: uploaded asset to GEE 

Output: Average carbon density all 100,200,300 cm depths for all counterfactual fire perimeter (This is also used for the progressive thaw workflow, so it is housed in the Progressive Thaw R Project) 
	Location: ProgressiveThaw/Output/Counterfactual_CarbonDensity/
	
Status: Done
	
% Organic carbon to determine which Gerrevink model (Using the Soil Organic carbon variable in dg/kg and divide by 100 to get percent C from SoilGrid) 

Script:  EE: percentC_Intersection	
	Input: 
 I. Counterfactual fire perimeter ensembles valid 
	Location: FSpro_Runs 
II. Soil grids percent carbon at 0,5,15,30,60 and 100 
		Location: Asset uploaded to GEE 

		Output: Mean percent C at each depth for all counterfactual fire perimeters 
			Location: TransientThaw/Data/
Counterfactual_CarbonFraction_Intersection/

Status: Done

Convert to daily-thawed carbon pool from the daily thawed volume of permafrost

Script: R: Step09_UPDATED2_DailyVolumeThawedPermafrostUpland.R and Step09_UPDATED_DailyVolumeThawedPermafrostLowland.R
Input: Carbon Density intersections for all counterfactual perimeters 
	Location:ProgressiveThaw/Output/Counterfactual_CarbonDensity/

Output: Nothing, continue with the script 

Status: In progress (Bottled necked with the Intersection and area determination from Upland/Lowland)	 Step 1


### 6: Apply Gerrevinks emission percent to the daily thawed carbon pool and sum to total carbon loss over 50 years post-fire. 

Script: R: Step09_UPDATED2_DailyVolumeThawedPermafrostUpland.R and Step09_UPDATED_DailyVolumeThawedPermafrostLowland.R

			Input: Output from Gerrevink carbon loss model (van Gerrevink et al. 
2026) 
Location: ProgressiveThaw/Data/
CarbonDensity/Gerrevink_percentcarbonloss.csv

Output: 
Partial annual carbon loss over 50 years broken up by counterfactual perimeter (computational limits) 
					Location: TransientThaw/Output/CarbonLoss/Partial/
Total annual carbon loss over 50 years from post-fire transient thaw 
	Location: TransientThaw/Output/CarbonLoss/

Status: In progress (Bottled necked with the Intersection and area determination from Upland/Lowland)	 Step 1
