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
library(tidyterra)
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

#MSAVI2

MSAVI2 =  (2 * NIR + 1 - sqrt( (2 * NIR + 1)^2 - 8 * (NIR - Red) )) / 2 

writeRaster(MSAVI2,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_30m_MSAVI2.tif", overwrite=TRUE)


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
Philleigh_50m_NDVI <-  rast("E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_NDVI.tif")

Philleigh_50m_NDVI

Philleigh_50m_NDVI <- Philleigh_50m_NDVI %>% rename (NDVI = Philleigh_50m_transparent_reflectance_nir)
Philleigh_50m_NDVI



#make stack

Philleigh_50m_ReflStack2 <- c(Philleigh_50m_Blue_Refl,Philleigh_50m_Green_Refl,Philleigh_50m_Red_Refl,Philleigh_50m_RedEdge_Refl,Philleigh_50m_NIR_Refl,Philleigh_50m_NDVI) 

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

#MSAVI2

MSAVI2 =  (2 * NIR + 1 - sqrt( (2 * NIR + 1)^2 - 8 * (NIR - Red) )) / 2 

writeRaster(MSAVI2,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_MSAVI2.tif", overwrite=TRUE)

aoi_terra <- vect("E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_clip.shp")

# cropping to study area (no vegetation)
MSAVI2_crop <- crop(MSAVI2, aoi_terra)
MSAVI2_mask <- mask(MSAVI2_crop, aoi_terra)
writeRaster(MSAVI2_mask,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_MSAVI2_crop.tif", overwrite=TRUE)

NDVI_crop <- crop(NDVI, aoi_terra)
NDVI_mask <- mask(NDVI_crop, aoi_terra)
writeRaster(NDVI_mask,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_NDVI_crop.tif", overwrite=TRUE)

RENDVI_crop <- crop(RENDVI, aoi_terra)
RENDVI_mask <- mask(RENDVI_crop, aoi_terra)
writeRaster(RENDVI_mask,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_RENDVI_crop.tif", overwrite=TRUE)

ReflStack_crop <- crop(Philleigh_50m_ReflStack, aoi_terra)
ReflStack_mask <- mask(ReflStack_crop, aoi_terra)
writeRaster(ReflStack_mask,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_ReflStack_crop.tif", overwrite=TRUE)



#writeRaster(Philleigh_50m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_50m_Refl_Stack.tif")
writeRaster(Philleigh_50m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_Refl_Stack.tif", overwrite=TRUE)
writeRaster(Philleigh_50m_ReflStack2,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_Refl_Stack2.tif", overwrite=TRUE)

#Philleigh 95m  stack

# read in data
#Philleigh_95m_clipper <- read_sf(dsn = 'E:/Glenn/Philleigh/Processed/Philleigh_95m', layer = "Philleigh_95m_clip")
#Philleigh_95m_clip <- vect(Philleigh_95m_clipper)

Philleigh_95m_Blue_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_blue.tif")
Philleigh_95m_Green_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_green.tif")
Philleigh_95m_Red_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_Red.tif")
Philleigh_95m_RedEdge_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_Red edge.tif")
Philleigh_95m_NIR_Refl <-  rast("E:/Glenn/Philleigh/Processed/Philleigh_95m/4_index/reflectance/Philleigh_95m_transparent_reflectance_NIR.tif")

#NDVI
NDVI = (NIR - Red)/(NIR + Red)

#plot (NDVI)
writeRaster(NDVI,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_95m/Philleigh_95m_NDVI.tif", overwrite=TRUE)


Philleigh_95m_NDVI <-  rast("E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_95m/Philleigh_95m_NDVI.tif")
Philleigh_95m_NDVI %>% rename(NDVI = Philleigh_95m_transparent_reflectance_nir)

#make stack

Philleigh_95m_ReflStack <- c(Philleigh_95m_Blue_Refl,Philleigh_95m_Green_Refl,Philleigh_95m_Red_Refl,Philleigh_95m_RedEdge_Refl,Philleigh_95m_NIR_Refl) 
Philleigh_95m_ReflStack2 <- c(Philleigh_95m_Blue_Refl,Philleigh_95m_Green_Refl,Philleigh_95m_Red_Refl,Philleigh_95m_RedEdge_Refl,Philleigh_95m_NIR_Refl,Philleigh_95m_NDVI ) 
Philleigh_95m_ReflStack2
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

#MSAVI2

MSAVI2 =  (2 * NIR + 1 - sqrt( (2 * NIR + 1)^2 - 8 * (NIR - Red) )) / 2 

writeRaster(MSAVI2,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_95m/Philleigh_95m_MSAVI2.tif", overwrite=TRUE)


#writeRaster(Philleigh_95m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_30m/Philleigh_95m_Refl_Stack.tif")
writeRaster(Philleigh_95m_ReflStack,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_95m/Philleigh_95m_Refl_Stack.tif", overwrite=TRUE)
writeRaster(Philleigh_95m_ReflStack2,"E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_95m/Philleigh_95m_Refl_Stack2.tif", overwrite=TRUE)


