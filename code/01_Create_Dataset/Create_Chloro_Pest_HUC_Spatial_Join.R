
library(dataRetrieval)
library(dplyr)
library(beepr)
library(lubridate)
library(tidyverse)
library(data.table)
library(sf)

AZO.points<- sf::st_read("/Volumes/messierkp/Projects/CensoredGP/WaterData-Pesticides/nwis_triazines_alachlors/data_AZO_file.shp")


HUC12<- sf::st_read("/Volumes/messierkp/Projects/CensoredGP/WaterData-Pesticides/GIS-Data/HUC12.shp")

AZO.HUC.join <- sf::st_join(AZO.points,HUC12)

AZO.HUC.join$huc10 <- str_sub(AZO.HUC.join$huc12,1,10)
AZO.HUC.join$huc08 <- str_sub(AZO.HUC.join$huc12,1,8)
AZO.HUC.join$huc06 <- str_sub(AZO.HUC.join$huc12,1,6)
AZO.HUC.join$huc04 <- str_sub(AZO.HUC.join$huc12,1,4)
AZO.HUC.join$huc02 <- str_sub(AZO.HUC.join$huc12,1,2)

sf::st_write(AZO.HUC.join,"/Volumes/messierkp/Projects/CensoredGP/WaterData-Pesticides/nwis_triazines_alachlors/data_AZO_watershed_huc_join.shp")
