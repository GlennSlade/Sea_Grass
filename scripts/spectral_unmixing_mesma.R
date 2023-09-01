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

end <-read_sf(dsn = 'E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in', layer = "extract_points_poly2")

#----1. Read in files for 50m Philleigh survey ----

stack <- rast("E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_in/Philleigh_50m/Philleigh_50m_Refl_stack2.tif")
stack

#stack %>% rename(NDVI = Philleigh_50m_transparent_reflectance_nir)

#----2. Extract end member spectral profile -----

end_spectra <- exact_extract(stack,end,"mean")
end_spectra_DF <-bind_cols(end,end_spectra)
end_spectra_DF<- st_drop_geometry(end_spectra_DF)
end_spectra_DF<-dplyr::select(end_spectra_DF,-c(1))
end_spectra_DF<-dplyr::select(end_spectra_DF,-c(2))
end_spectra_DF<-dplyr::select(end_spectra_DF,-c(2))
#end_spectra_DF<-dplyr::select(end_spectra_DF,-c(1))
end_mean <- end_spectra_DF%>%group_by(Type) %>% summarise(across(everything(), list(mean)))
end_mean<-dplyr::select(end_mean,-c(1))
#-----3. Run mesma -------

probs <- mesma(stack, end_mean, method = "NNLS")

# generates multilayer raster containing one band per endmember, with each
# value representing the estimated presence probability of the endmember per pixel
# (0 to 1), and an RMSE band. 

#----4. Write unmixed raster brick

writeRaster(probs,"data_out/Philleigh_50m_unmixed_2.tif", overwrite=TRUE)


B1 <-probs$layer.1 #SG
B2<- probs$layer.2 #ALGAE
B3<- probs$layer.3#Sediment
B4<- probs$layer.4#Water
#B5<- probs$layer.5#sed2
#B6<- probs$layer.6#sed3
#B7<- probs$layer.7#water
# Write individual layers and consolidate multilayer classes


writeRaster(B1,"data_out/Philleigh_50m_mesam_SG.tif", overwrite=TRUE)
writeRaster(B2,"data_out/Philleigh_50m_mesam_Algae.tif", overwrite=TRUE)
writeRaster(B3,"data_out/Philleigh_50m_mesam_Sediment.tif", overwrite=TRUE)
writeRaster(B4,"data_out/Philleigh_50m_mesam_water.tif", overwrite=TRUE)












