### Script to Extract Data from Drone survey Classifications based on WV2 pixel grid
### matching pixels
### Imports classified drone image data set and calculates fractional cover for each 
### class in each WV2 pixel and then samples that data to produce training data set
### for classification of WV2 image.

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
#library (raster)
library(dplyr)

#-------0. Read in shape file with end member



#----1. Read in files for 50m Philleigh survey ----

stack <- rast("data_in/Philleigh_50m/Philleigh_50m_ReflStack_crop.tif")

#----2. Extract end member spectral profile -----



#-----3. Run mesma -------

