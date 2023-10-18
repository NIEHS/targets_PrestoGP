### AZO - TWI

pkgs <- c("rlang", "dplyr", "data.table", "tidyr", "sf", "terra")
invisible(lapply(pkgs, library, character.only = TRUE, quietly = TRUE, logical.return = TRUE))
Sys.setenv("sf_use_s2" = FALSE)

## data
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
          "~/Downloads/",
          stop("COMPUTE_MODE should be one of 1, 2, 3, or 4.\n")
        )
      )
    )
  )



## AZO
## No HUC info in RData object.
load("./input/data_process/data_AZO_year_avg.RData")
azo <- data.AZO.year.avg
azo <- st_as_sf(azo, coords = c("Longitude", "Latitude"), crs = 4326)
# azo = sf::read_sf(paste0("./data/data_process/data_AZO_watershed_huc_join.shp"))
azo_s <- azo %>%
  dplyr::select(site_no, Year) %>%
  unique() %>%
  st_transform("EPSG:5072")
azo_t <- terra::vect(azo_s)


## TWI
twi <- paste0(path_base, "TWI/COUS_TWI_epsg5072_30m_unmasked.tif")
twi_ras <- terra::rast(twi)


## Extract TWI
azo_twi <- terra::extract(twi_ras, azo_t)
azo_twi[["site_no"]] <- unlist(azo_t[["site_no"]])
azo_twi[["Year"]] <- unlist(azo_t[["Year"]])

write.csv(azo_twi, gsub("input", "output/AZO_TWI_extract.csv", path_base), row.names = FALSE)
