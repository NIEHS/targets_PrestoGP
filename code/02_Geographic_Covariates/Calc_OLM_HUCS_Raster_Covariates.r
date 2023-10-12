## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries and sourcing,echo=FALSE----------------------------------------
library(sf)
library(terra)
library(exactextractr)
library(lubridate)
library(tidyverse)
library(data.table)
# library(nhdplusTools)
sf_use_s2(FALSE)

## ----pesticide and NHD WBD data, echo=FALSE-----------------------------------
# Read in main pesticide data here
# data.AZO <- sf::st_read("input/data_process/data_AZO_watershed_huc_join.shp")

# # For efficient extraction, we just need the geometry
# AZO.geometry <- sf::st_geometry(data.AZO)

# # US bounding box
# US.bb <- terra::ext(c(-124.7844079, -66.9513812, 24.7433195, 49.3457868))

# # NHD WBD Layer names
# WBD.layers <- st_layers("input/WBD-National/WBD_National_GDB.gdb")

# WBD <- st_read("input/WBD-National/WBD_National_GDB.gdb", layer = "WBDHU8") %>%
#   st_transform("EPSG:4326")


## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 2
path_base <-
  ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/input/",
    ifelse(COMPUTE_MODE == 2,
      "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/",
      ifelse(COMPUTE_MODE == 3,
        "/opt/",
        ifelse(COMPUTE_MODE == 4,
          "/tmp/",
          stop("COMPUTE_MODE should be one of 1, 2, 3, or 4.\n")
        )
      )
    )
  )


## ----OLM raster data,echo=FALSE-----------------------------------------------
# Cropped Raster directory
# cropped.raster.dir <- "/Volumes/SHAG/OpenLandMapData/OLM_Combined/"
cropped.raster.dir <- ""#"OpenLandMapData/OLM_Combined/"

if (length(list.files(
  path = paste0(path_base, cropped.raster.dir),
  pattern = "*.tif$", full.names = T
)) == 7) { # If the cropped rasters have been generated,read those in

  pH.stack <- terra::rast(paste0(cropped.raster.dir, "OLM_US_Crop_pH.tif"))
  Clay_Content.stack <- terra::rast(paste0(cropped.raster.dir, "OLM_US_Crop_Clay_Content.tif"))
  Bulk_Density.stack <- terra::rast(paste0(cropped.raster.dir, "OLM_US_Crop_Bulk_Density.tif"))
  Sand_Content.stack <- terra::rast(paste0(cropped.raster.dir, "OLM_US_Crop_Sand_Content.tif"))
  Organic_Carbon.stack <- terra::rast(paste0(cropped.raster.dir, "OLM_US_Crop_Organic_Carbon.tif"))
  soil_order.stack <- terra::rast(paste0(cropped.raster.dir, "OLM_US_Crop_Soil_Order.tif"))
  texture.stack <- terra::rast(paste0(cropped.raster.dir, "OLM_US_Crop_Soil_Texture_Class.tif"))
} else { # otherwise generate them which takes a bit of time (~ 1 hour)

  # Directories of individual raw OLM data
  OLM.dir.pH <- "input/OpenLandMapData/pH"
  OLM.dir.Clay_Content <- "input/OpenLandMapData/Clay_Content"
  OLM.dir.Bulk_Density <- "input/OpenLandMapData/Bulk_Density"
  OLM.dir.Sand_Content <- "input/OpenLandMapData/Sand_Content"
  OLM.dir.Organic_Carbon <- "input/OpenLandMapData/Organic_Carbon"
  OLM.dir.Soil_Order <- "input/OpenLandMapData/Soil_Order"
  OLM.dir.Texture <- "input/OpenLandMapData/USDA_Texture_Class"


  # Get Raster data filenames/paths
  OLM.filenames.pH <- list.files(
    path = OLM.dir.pH,
    pattern = "*.tif", full.names = T
  )

  pH.stack <- terra::rast(OLM.filenames.pH) %>% terra::crop(US.bb)
  beepr::beep(sound = 2)

  #
  OLM.filenames.Clay_Content <- list.files(
    path = OLM.dir.Clay_Content,
    pattern = "*.tif", full.names = T
  )

  Clay_Content.stack <- terra::rast(OLM.filenames.Clay_Content) %>% terra::crop(US.bb)
  beepr::beep(sound = 2)

  #
  OLM.filenames.Bulk_Density <- list.files(
    path = OLM.dir.Bulk_Density,
    pattern = "*.tif", full.names = T
  )

  Bulk_Density.stack <- terra::rast(OLM.filenames.Bulk_Density) %>% terra::crop(US.bb)
  beepr::beep(sound = 2)

  #
  OLM.filenames.Sand_Content <- list.files(
    path = OLM.dir.Sand_Content,
    pattern = "*.tif", full.names = T
  )

  Sand_Content.stack <- terra::rast(OLM.filenames.Sand_Content) %>% terra::crop(US.bb)
  beepr::beep(sound = 2)

  #
  OLM.filenames.Organic_Carbon <- list.files(
    path = OLM.dir.Organic_Carbon,
    pattern = "*.tif", full.names = T
  )

  Organic_Carbon.stack <- terra::rast(OLM.filenames.Organic_Carbon) %>% terra::crop(US.bb)
  beepr::beep(sound = 2)

  #
  OLM.filenames.Soil_Order <- list.files(
    path = OLM.dir.Soil_Order,
    pattern = "*.tif", full.names = T
  )

  soil_order.stack <- terra::rast(OLM.filenames.Soil_Order) %>% terra::crop(US.bb)

  #
  OLM.filenames.Texture <- list.files(
    path = OLM.dir.Texture,
    pattern = "*.tif", full.names = T
  )

  texture.stack <- terra::rast(OLM.filenames.Texture) %>% terra::crop(US.bb)


  # Write the cropped rasters for future use and time saving
  writeRaster(pH.stack, "input/OpenLandMapData/OLM_Combined/OLM_US_Crop_pH.tif")
  writeRaster(Clay_Content.stack, "input/OpenLandMapData/OLM_Combined/OLM_US_Crop_Clay_Content.tif")
  writeRaster(Bulk_Density.stack, "input/OpenLandMapData/OLM_Combined/OLM_US_Crop_Bulk_Density.tif")
  writeRaster(Sand_Content.stack, "input/OpenLandMapData/OLM_Combined/OLM_US_Crop_Sand_Content.tif")
  writeRaster(Organic_Carbon.stack, "input/OpenLandMapData/OLM_Combined/OLM_US_Crop_Organic_Carbon.tif")
  writeRaster(soil_order.stack, "input/OpenLandMapData/OLM_Combined/OLM_US_Crop_Soil_Order.tif")
  writeRaster(texture.stack, "input/OpenLandMapData/OLM_Combined/OLM_US_Crop_Soil_Texture_Class.tif")
}




# Combine the rasters in which we are calculating a mean value into one raster stack with
# many layers
OLM.stack.values <- c(
  pH.stack, Clay_Content.stack, Bulk_Density.stack, Sand_Content.stack,
  Organic_Carbon.stack, soil_order.stack
)

OLM.stack.classes <- texture.stack


# The soil texturer needs updating of its class names

texture_classes <- data.frame("classes" = c(
  "clay", "silty_clay", "sandy_clay",
  "clay_loam", "silty_clay_loam", "sandy_clay_loam", "loam",
  "silt_loam", "sandy_loam", "silt",
  "loamy_sand", "sand"
), "value" = 1:12)

for (i in 1:6) {
  levels(OLM.stack.classes[[i]]) <- list(data.frame(
    ID = 1:12,
    textures = texture_classes$classes
  ))
}

names(OLM.stack.classes) <- c(
  "Texture_000cm", "Texture_010cm", "Texture_030cm",
  "Texture_060cm", "Texture_100cm", "Texture_200cm"
)


## ----Calculate exact grid points ,echo=TRUE-----------------------------------


# HUC08
HUC.rast.vals <- data.table("huc08" = data.AZO$huc08)

huc08.unique <- unique(data.AZO$huc08)

HUC.rast.vals[, paste0("huc08", ".", names(OLM.stack.values)) := NA_real_]

HUC.rast.class <- data.table("huc08" = data.AZO$huc08)

class.df <- expand.grid(levels(OLM.stack.classes)[[1]]$ID, c(
  "Texture_000cm", "Texture_010cm", "Texture_030cm",
  "Texture_060cm", "Texture_100cm", "Texture_200cm"
))
class.df <- class.df[order(class.df$Var1), ]

class.possible.names <- paste0("frac_", class.df$Var1, ".", class.df$Var2)


HUC.rast.class[, paste0("huc08", ".", class.possible.names) := 0]

# for (i in 1:length(huc08.unique)) {
#   print(i)

  # get index - for rows of output - where data match the given HUC08
  # idx.row <- data.AZO$huc08 == huc08.unique[i]

  # get the given HUC08 geometry from the WBD data
  # huc08.polygon <- dplyr::filter(WBD, huc8 == huc08.unique[i])
  huc08.polygon <- sf::read_sf(paste0(path_base, "WBD_National_GPKG.gpkg"), layer = "WBDHU8")

  # calculate the mean raster values in the HUC
  huc08.val <- exact_extract(OLM.stack.values, st_geometry(huc08.polygon), fun = "mean", stack_apply = TRUE)

  # Assign the extracted values to the appropriate location in the output
  HUC.rast.vals[, 2:ncol(HUC.rast.vals)] <- huc08.val

  # calculate the fraction of each raster class in the HUC
  extract.raster.classes <- exact_extract(OLM.stack.classes, huc08.polygon, fun = "frac", stack_apply = TRUE)

  # # get indexs - for columns of outout - where classes match the output
  idx.col <- which(class.possible.names %in% colnames(extract.raster.classes)) + 1

  # Assign the extracted class fractions to the appropriate location in the output
  HUC.rast.class[, idx.col] <- extract.raster.classes


# Correct some column names that will be problematic later

HUC.rast.vals <- HUC.rast.vals %>%
  rename_with(function(x) str_replace_all(x, "[.]", "_")) %>%
  rename_with(function(x) str_replace_all(x, "_{2}", "_")) %>%
  rename_with(function(x) str_replace_all(x, "sol_order_usda_soiltax_", "")) %>%
  rename_with(function(x) str_replace_all(x, "_1950_2017_v0_1", "")) %>%
  select(-huc08)

# Classes column names updates
HUC.rast.class <- HUC.rast.class %>%
  rename_with(function(x) str_replace_all(x, "[.]", "_")) %>%
  select(-huc08)

# Rename the number with the class name - ugly but it works
HUC.rast.class <- dplyr::rename_with(HUC.rast.class, ~ gsub("frac_1_", paste0("frac_", texture_classes$classes[1], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_2_", paste0("frac_", texture_classes$classes[2], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_3_", paste0("frac_", texture_classes$classes[3], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_4_", paste0("frac_", texture_classes$classes[4], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_5_", paste0("frac_", texture_classes$classes[5], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_6_", paste0("frac_", texture_classes$classes[6], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_7_", paste0("frac_", texture_classes$classes[7], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_8_", paste0("frac_", texture_classes$classes[8], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_9_", paste0("frac_", texture_classes$classes[9], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_10_", paste0("frac_", texture_classes$classes[10], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_11_", paste0("frac_", texture_classes$classes[11], "_"), .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("frac_12_", paste0("frac_", texture_classes$classes[12], "_"), .x, fixed = TRUE))


## ----Save the data to a geopackage (OSG open source format) ,echo=TRUE--------
data.AZO.HUC08.OLM <- cbind(data.AZO, HUC.rast.vals, HUC.rast.class)
sf::st_write(data.AZO.HUC08.OLM, paste0(path_base, "/Covariates/AZO_HUC08_OLM.gpkg"))

