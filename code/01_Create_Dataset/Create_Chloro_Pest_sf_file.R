
library(dataRetrieval)
library(dplyr)
library(beepr)
library(lubridate)
library(tidyverse)
library(data.table)
library(sf)
data.AZO.year.avg <- get(load(file = "/Volumes/messierkp/Projects/CensoredGP/WaterData-Pesticides/nwis_triazines_alachlors/data_AZO_year_avg_20221120.RData"))

data.AZO.year.avg$ChemicalName[data.AZO.year.avg$ChemicalName=="2-Chloro-4-isopropylamino-6-amino-s-triazine"]="Deethylatrazine"
data.AZO.year.avg$ChemicalName[data.AZO.year.avg$ChemicalName=="2-Chloro-4,6-diamino-s-triazine"]="Diaminoatrazine"

AZO.sf.point<- sf::st_as_sf(data.AZO.year.avg,coords = c("Longitude","Latitude"),crs = "EPSG:4326")


save(AZO.sf.point,file = "/Volumes/messierkp/Projects/CensoredGP/WaterData-Pesticides/nwis_triazines_alachlors/data_AZO_sf_file.RData")

sf::st_write(AZO.sf.point,"/Volumes/messierkp/Projects/CensoredGP/WaterData-Pesticides/nwis_triazines_alachlors/data_AZO_file.shp")
