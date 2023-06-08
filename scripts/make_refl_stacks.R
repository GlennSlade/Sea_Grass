#Make rast reflectance stacks from Philleigh Pix4d reflectance maps

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

#Philleigh 30m  stack

# read in data
#Philleigh_30m_clipper <- read_sf(dsn = 'E:/Glenn/Philleigh/Processed/Philleigh_30m', layer = "Philleigh_30m_clip")
#Philleigh_30m_clip <- vect(Philleigh_30m_clipper)

Philleigh_30m_Blue_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_30m/4_index/reflectance/Philleigh_30m_transparent_reflectance_blue.tif")
Philleigh_30m_Green_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_30m/4_index/reflectance/Philleigh_30m_transparent_reflectance_green.tif")
Philleigh_30m_Red_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_30m/4_index/reflectance/Philleigh_30m_transparent_reflectance_Red.tif")
Philleigh_30m_RedEdge_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_30m/4_index/reflectance/Philleigh_30m_transparent_reflectance_Red edge.tif")
Philleigh_30m_NIR_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_30m/4_index/reflectance/Philleigh_30m_transparent_reflectance_NIR.tif")

#make stack

Philleigh_30m_ReflStack <- c(Philleigh_30m_Blue_Refl,Philleigh_30m_Green_Refl,Philleigh_30m_Red_Refl,Philleigh_30m_RedEdge_Refl,Philleigh_30m_NIR_Refl) 

Blue = Philleigh_30m_Blue_Refl 
Green =Philleigh_30m_Green_Refl 
Red = Philleigh_30m_Red_Refl   
RE = Philleigh_30m_RedEdge_Refl 
NIR = Philleigh_30m_NIR_Refl 

#NDVI
NDVI = (NIR - Red)/(NIR + Red)


#plot (NDVI)
writeRaster(NDVI,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_30m_NDVI.tif", overwrite=TRUE)

#RENDVI

RENDVI = (RE - Red)/(RE + Red)

#plot (RENDVI)
writeRaster(RENDVI,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_30m_RENDVI.tif", overwrite=TRUE)

#writeRaster(Philleigh_30m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_30m_Refl_Stack.tif")
writeRaster(Philleigh_30m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_30m_Refl_Stack.tif", overwrite=TRUE)

#Philleigh 50m  stack

# read in data
#Philleigh_50m_clipper <- read_sf(dsn = 'E:/Glenn/Philleigh/Processed/Philleigh_50m', layer = "Philleigh_50m_clip")
#Philleigh_50m_clip <- vect(Philleigh_50m_clipper)

Philleigh_50m_Blue_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_50m/4_index/reflectance/Philleigh_50m_transparent_reflectance_blue.tif")
Philleigh_50m_Green_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_50m/4_index/reflectance/Philleigh_50m_transparent_reflectance_green.tif")
Philleigh_50m_Red_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_50m/4_index/reflectance/Philleigh_50m_transparent_reflectance_Red.tif")
Philleigh_50m_RedEdge_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_50m/4_index/reflectance/Philleigh_50m_transparent_reflectance_Red edge.tif")
Philleigh_50m_NIR_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_50m/4_index/reflectance/Philleigh_50m_transparent_reflectance_NIR.tif")

#make stack

Philleigh_50m_ReflStack <- c(Philleigh_50m_Blue_Refl,Philleigh_50m_Green_Refl,Philleigh_50m_Red_Refl,Philleigh_50m_RedEdge_Refl,Philleigh_50m_NIR_Refl) 

Blue = Philleigh_50m_Blue_Refl 
Green =Philleigh_50m_Green_Refl 
Red = Philleigh_50m_Red_Refl   
RE = Philleigh_50m_RedEdge_Refl 
NIR = Philleigh_50m_NIR_Refl 

#NDVI
NDVI = (NIR - Red)/(NIR + Red)

#plot (NDVI)
writeRaster(NDVI,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_NDVI.tif", overwrite=TRUE)

#RENDVI

RENDVI = (RE - Red)/(RE + Red)

#plot (RENDVI)
writeRaster(RENDVI,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_RENDVI.tif", overwrite=TRUE)

#writeRaster(Philleigh_50m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_50m_Refl_Stack.tif")
writeRaster(Philleigh_50m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_Refl_Stack.tif", overwrite=TRUE)

#Philleigh 95m  stack

# read in data
#Philleigh_95m_clipper <- read_sf(dsn = 'E:/Glenn/Philleigh/Processed/Philleigh_95m', layer = "Philleigh_95m_clip")
#Philleigh_95m_clip <- vect(Philleigh_95m_clipper)

Philleigh_95m_Blue_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_blue.tif")
Philleigh_95m_Green_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_green.tif")
Philleigh_95m_Red_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_Red.tif")
Philleigh_95m_RedEdge_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_Red edge.tif")
Philleigh_95m_NIR_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_NIR.tif")

#make stack

Philleigh_95m_ReflStack <- c(Philleigh_95m_Blue_Refl,Philleigh_95m_Green_Refl,Philleigh_95m_Red_Refl,Philleigh_95m_RedEdge_Refl,Philleigh_95m_NIR_Refl) 

Blue = Philleigh_95m_Blue_Refl 
Green =Philleigh_95m_Green_Refl 
Red = Philleigh_95m_Red_Refl   
RE = Philleigh_95m_RedEdge_Refl 
NIR = Philleigh_95m_NIR_Refl 

#NDVI
NDVI = (NIR - Red)/(NIR + Red)

#plot (NDVI)
writeRaster(NDVI,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_95m/Philleigh_95m_NDVI.tif", overwrite=TRUE)

#RENDVI

RENDVI = (RE - Red)/(RE + Red)

#plot (RENDVI)
writeRaster(RENDVI,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_95m/Philleigh_95m_RENDVI.tif", overwrite=TRUE)

#writeRaster(Philleigh_95m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_95m_Refl_Stack.tif")
writeRaster(Philleigh_95m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_95m/Philleigh_95m_Refl_Stack.tif", overwrite=TRUE)