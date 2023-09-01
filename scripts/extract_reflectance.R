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
Phill_UM <- rast ("E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_out/Philleigh_50m_mesam_SG.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Train_SF <- read_sf(dsn = 'E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in', layer = "extract_points_poly2")

Phill_50
Train_SF

#Train_SF <-terra::project(Train_SF, y="EPSG:32630")


#---2. Extract data for 15cm polygons around data points


Refl_Extract <- exact_extract(Phill_NDVI,Train_SF,"mean")
Refl_Extract2 <-bind_cols(Train_SF,Refl_Extract)

Refl_Extract3 <- exact_extract(Phill_50,Train_SF,"mean")
Refl_Extract4 <-bind_cols(Refl_Extract2,Refl_Extract3)

#st_geometry(Refl_Extract4) <- NULL

write.csv(Refl_Extract4, file = "E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_out/reflectance_extract.csv")

Refl_Extract5 <- exact_extract(Phill_UM,Train_SF,"mean")
Refl_Extract6 <-bind_cols(Refl_Extract4,Refl_Extract5)

Refl_Extract7 <- Refl_Extract6%>%st_drop_geometry(Refl_Extract6)


plot (Phill_UM)
write.csv(Refl_Extract7, file = "E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_out/reflectance_extract_UM.csv")

DF8 <- Refl_Extract7 %>% group_by(Type) %>% summarise(across(everything(), list(mean)))

