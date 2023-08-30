
# Libraries
library(viridis)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(splines)
library(gridExtra)
library(DescTools)
library(sf)
library(writexl)
library(cowplot)

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      axis.text.x = element_text(angle = 90),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 7, color = "black"),
      legend.title = element_text(size = 7, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

#-------1. Read in data in CSV Files --------

REFL <- read.csv("E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_out/reflectance_extract_Philleigh.csv")

#-------2. Preparing data --------

dim (REFL)
summary (REFL)
head (REFL)
str(REFL)
#as.factor (REFL$Type)

#-----3.   Plotting Data -------
# NDVI
NDVIbp <- ggplot(data = REFL, mapping = aes (x=Type, y=NDVI, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","green","burlywood1"
                                                 ),outlier.shape = NA)
NDVIbp2 <- NDVIbp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Dwarf Sea Grass", "Algae", "Sediment" 
                                                                        ))
NDVI_BP <- NDVIbp2 + ylab("Calibrated Reflectance NDVI") + ggtitle("Mean NDVI Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (NDVI_BP)

# Blue

Bluebp <- ggplot(data = REFL, mapping = aes (x=Type, y=Blue, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","green","burlywood1"
  ),outlier.shape = NA)
Bluebp2 <- Bluebp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Dwarf Sea Grass", "Algae", "Sediment" 
))
Blue_BP <- Bluebp2 + ylab("Calibrated Reflectance Blue") + ggtitle("Mean Calibrated Reflectance Blue \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (Blue_BP)

# Green

Greenbp <- ggplot(data = REFL, mapping = aes (x=Type, y=Green, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","green","burlywood1"
  ),outlier.shape = NA)
Greenbp2 <- Greenbp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Dwarf Sea Grass", "Algae", "Sediment" 
))
Green_BP <- Greenbp2 + ylab("Calibrated Reflectance Green") + ggtitle("Mean Calibrated Reflectance Green \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (Green_BP)

# Red


Redbp <- ggplot(data = REFL, mapping = aes (x=Type, y=Red, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","green","burlywood1"
  ),outlier.shape = NA)
Redbp2 <- Redbp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Dwarf Sea Grass", "Algae", "Sediment" 
))
Red_BP <- Redbp2 + ylab("Calibrated Reflectance Red") + ggtitle("Mean Calibrated Reflectance Red \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (Red_BP)



# Red Edge
RedEdgebp <- ggplot(data = REFL, mapping = aes (x=Type, y=RedEdge, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","green","burlywood1"
  ),outlier.shape = NA)
RedEdgebp2 <- RedEdgebp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Dwarf Sea Grass", "Algae", "Sediment" 
))
RedEdge_BP <- RedEdgebp2 + ylab("Calibrated Reflectance RedEdge") + ggtitle("Mean Calibrated Reflectance RedEdge \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (RedEdge_BP)


# NIR
NIRbp <- ggplot(data = REFL, mapping = aes (x=Type, y=NIR, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","green","burlywood1"
  ),outlier.shape = NA)
NIRbp2 <- NIRbp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Dwarf Sea Grass", "Algae", "Sediment" 
))
NIR_BP <- NIRbp2 + ylab("Calibrated Reflectance NIR") + ggtitle("Mean Calibrated Reflectance NIR \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (NIR_BP)

#----4. Saving plots and combining----

Plotcombined <-  plot_grid(Blue_BP,Green_BP,Red_BP, RedEdge_BP,NIR_BP,NDVI_BP, nrow = 3, rel_heights = c(0.33,0.33,0.33))

ggsave(
  Plotcombined,
  filename = "E:/Glenn/Philleigh/R_Scripts/Sea_Grass/data_out/reflectance_extract_Philleigh.png",
  width =17,
  height = 22,
  units = "cm"
)