# Heat Stress Paper: Code for Visualization
By: Zhan Wang, Department of Agricultural Economics, Purdue University.

Contact: zhanwang@purdue.edu
# Introduction
This repository stores the R code to visualize results for the working paper of agricultural impacts of human heat stress in the US. The archive of model, data and simulations can be downloaded from https://github.com/wangzhan90/HeatStressPaper_model.

# Description
## \in
This folder contains a mapping file that connects farming resource regions with their short names and index.
## \out
This folder will store figures and tables generated.
## \shp
The folder contains the shapefile of farming resource regions in the US. This shapefile is created by mapping county level shapefile to farming resource regions with the "county-to-ERS Resource Region aggregation in Excel" file from USDA-ERS (https://www.ers.usda.gov/data-products/arms-farm-financial-and-crop-production-practices/documentation.aspx, accessed on 2024/4/28).
## \temp
This folder will store temporary files generated during the visualization.
## visualizeResult.R
The code for results processing and visualization.

# How to use
## Step 1: Download and run model simulations
Please download the model, data and simulation archive and follow the "Readme.txt" file to run simulations for all 13 GCMs. Solutions files from simulations should be located under the "SIMPLEG-US_HeatStress\out" folder in the format of "SIMPLEG_IGS_Labor_3C_<GCM name>_CMIP6.sl4".

## Step 2: Download the code for visualization
Please download the “visualizeResult” folder and put it under the folder “SIMPLEG-US_HeatStress”.

## Step 3: Run the code to visualize results
Please run “SIMPLEG-US_HeatStress\visualizeResult\visualizeResult.R”, which will automatically read in solution files and create figures used in this paper. It will also generate several tables to assist analysis. 


