library(pacman)
p_load(terra, sf, dplyr, purrr, data.table, future, future.apply, exactextractr, bread, conflicted)
sf_use_s2(FALSE)

## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 4
path_base <-
  ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/input/",
    ifelse(COMPUTE_MODE == 2,
      "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/",
      ifelse(COMPUTE_MODE == 3,
        "/opt/",
        ifelse(COMPUTE_MODE == 4,
          "~/Documents/",
          stop("COMPUTE_MODE should be one of 1, 2, 3, or 4.\n")
        )
      )
    )
  )

# path_base <- "~/Documents/"
## wbd
ext_mainland <- c(xmin = -126, xmax = -64, ymin = 24, ymax = 51)
ext_mainland <- terra::ext(ext_mainland)


## AZO
azo <- terra::vect(paste0(path_base, "data_process/data_AZO_watershed_huc_join.shp"))
azo_s <- azo |>
  _[,c("site_no", "Year")] |>
  unique() |>
  terra::project("EPSG:5070")

path_gnat <- paste0(path_base, "gNATSGO_CONUS/gNATSGO_Tabular_CSV/")
path_gnat_files <- list.files(
  path = path_gnat, pattern = "*.csv$",
  full.names = TRUE
)

raster_mapid <- paste0(path_base, "gNATSGO_CONUS/gNATSGO_mukey_grid/gNATSGO-mukey.tif")
# to avoid GDAL errors: https://github.com/rspatial/terra/issues/314
raster_mapid <- terra::rast(raster_mapid, opts = "HONOUR_VALID_RANGE=NO")


azo_s_mapid <- terra::extract(raster_mapid, azo_s)
azo_s_mapid[["ID"]] <- unlist(azo_s[["site_no"]])

## ... mask / crop raster
## ... connect attribute tables ....
## ... extract values
# sf::sf_extSoftVersion()

gnat65 <- bread::bread(path_gnat_files[65])
gnat59 <- bread::bread(path_gnat_files[59])
cotaxmoistcl <- bread::bread(path_gnat_files[39])
bread::bread(path_gnat_files[2], first_row = 1, last_row = 1000)

# chconsistence
# chfrags
# chorizon
# chpores
# chstructgrp

