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
library(raster)
library(RStoolbox)

#-------0. Read in shape file with end member

end <-read_sf(dsn = 'data_in/Philleigh_50m', layer = "end_members")

#----1. Read in files for 50m Philleigh survey ----

stack <- rast("data_in/Philleigh_50m/Philleigh_50m_ReflStack_crop.tif")

#----2. Extract end member spectral profile -----

end_spectra <- exact_extract(stack,end,"mean")
end_spectra_DF <-bind_cols(end,end_spectra)
end_spectra_DF<- st_drop_geometry(end_spectra_DF)
end_spectra_DF<-dplyr::select(end_spectra_DF,-c(1))
end_spectra_DF<-dplyr::select(end_spectra_DF,-c(1))
#-----3. Run mesma -------

probs <- mesma(stack, end_spectra_DF, method = "NNLS")

# generates multilayer raster containing one band per endmember, with each
# value representing the estimated presence probability of the endmember per pixel
# (0 to 1), and an RMSE band. 

#----4. Write unmixed raster brick

writeRaster(probs,"data_out/Philleigh_50m_unmixed.tif", overwrite=TRUE)


B1 <-probs$layer.1 #algae 1
B2<- probs$layer.2 #sea grass
B3<- probs$layer.3#sed
B4<- probs$layer.4#Algae 2
B5<- probs$layer.5#sed2
B6<- probs$layer.6#sed3
B7<- probs$layer.7#water
# Write individual layers and consolidate multilayer classes
Algae <- B1+B4
Sediment<- B3+B5+B6

writeRaster(Algae,"data_out/Philleigh_50m_mesam_algae.tif", overwrite=TRUE)
writeRaster(Sediment,"data_out/Philleigh_50m_mesam_sediment.tif", overwrite=TRUE)
writeRaster(B2,"data_out/Philleigh_50m_mesam_SG.tif", overwrite=TRUE)
writeRaster(B7,"data_out/Philleigh_50m_mesam_water.tif", overwrite=TRUE)












