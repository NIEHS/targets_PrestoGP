library(pacman)
p_load(terra, sf, dplyr, purrr, data.table, future, future.apply, exactextractr, bread, conflicted)
sf_use_s2(FALSE)

## data path determination
## compute_mode 
## 1 (wine mount), 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 1
path_base <-
  ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/input/",
    ifelse(COMPUTE_MODE == 2,
      "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/",
      ifelse(COMPUTE_MODE == 3,
        "/opt/", stop("COMPUTE_MODE should be one of 1, 2, or 3.\n")
      )
    )
  )

# path_base <- "~/Documents/"
## wbd
ext_mainland <- c(xmin = -126, xmax = -64, ymin = 24, ymax = 51)
ext_mainland <- terra::ext(ext_mainland)

path_gnat <- paste0(path_base, "gNATSGO_CONUS/gNATSGO_Tabular_CSV/")
path_gnat_files <- list.files(
  path = path_gnat, pattern = "*.csv$",
  full.names = TRUE
)

raster_mapid <- paste0(path_base, "gNATSGO_CONUS/gNATSGO_mukey_grid/gNATSGO-mukey.tif")
raster_mapid <- terra::rast(raster_mapid)


## ... mask / crop raster
## ... connect attribute tables ....
## ... extract values
# sf::sf_extSoftVersion()

gnat65 <- bread::bread(path_gnat_files[65])
gnat59 <- bread::bread(path_gnat_files[59])
cotaxmoistcl <- bread::bread(path_gnat_files[39])
