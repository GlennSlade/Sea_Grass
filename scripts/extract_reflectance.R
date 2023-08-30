# Script to extract reflectance data from drone image data based on training points

library(terra)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(splines)
library(rgeos)
library(tidyverse)
library(viridis)
library(gridExtra)
library(DescTools)
library(sf)
library(exactextractr)
library(writexl) 
library (raster)
library(dplyr)
library(remotes)


#----1. Read in files for WV2 ----


Phill_50 <- rast("E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_Refl_stack.tif")
Phill_NDVI<- rast("E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_NDVI.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Train_SF <- read_sf(dsn = 'E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in', layer = "extract_points_poly")

Phill_50
Train_SF

#Train_SF <-terra::project(Train_SF, y="EPSG:32630")


#---2. Extract data for 15cm polygons around data points


Refl_Extract <- exact_extract(Phill_NDVI,Train_SF,"mean")
Refl_Extract2 <-bind_cols(Train_SF,Refl_Extract)

Refl_Extract3 <- exact_extract(Phill_50,Train_SF,"mean")
Refl_Extract4 <-bind_cols(Refl_Extract2,Refl_Extract3)

write.csv(Refl_Extract4, file = "E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_out/reflectance_extract.csv")
