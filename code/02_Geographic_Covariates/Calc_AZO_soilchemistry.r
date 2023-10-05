### AZO - Soil Chemistry


pkgs <- c("rlang", "dplyr", "data.table", "tidyr", "sf", "terra")
invisible(lapply(pkgs, library, character.only = TRUE, quietly = TRUE, logical.return = TRUE))
sf_use_s2(FALSE)


## data
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
          "~/Downloads/",
          stop("COMPUTE_MODE should be one of 1, 2, 3, or 4.\n")
        )
      )
    )
  )

# future::plan(sequential)

## AZO
## No HUC info in RData object.
load("./input/data_process/data_AZO_year_avg.RData")
azo <- data.AZO.year.avg
azo <- st_as_sf(azo, coords = c("Longitude", "Latitude"), crs = 4326)
# azo = sf::read_sf(paste0("./data/data_process/data_AZO_watershed_huc_join.shp"))
azo_s <- azo %>%
  dplyr::select(site_no, Year) %>%
  unique() %>%
  st_transform("EPSG:4326")



## soil
## geochemical survey
ngs <- read_sf(sprintf("%sngs/ngs.shp", path_base)) %>%
  st_transform(st_crs(azo_s))

## NGDB soil
ngdbsoil <- read_sf(sprintf("%sngdbsoil-csv/ngdbsoil/ngdbsoil.shp", path_base))
ngdbsoil_chem <- fread(sprintf("%sngdbsoil-csv/ngdbsoil/chemistry.csv", path_base))

ngdbsoil_chem[lab_id == "D-547288", ]

## bedrock
bedrock <- read_sf(sprintf("%sngdbrock/ngdbrock.shp", path_base))
bedrock_attr <- fread(sprintf("%sngdbrock-tab/tblRockGeoData.txt", path_base), sep = "\t", stringsAsFactors = TRUE, quote = "", na.strings = "NULL")

# readLines(sprintf("%sngdbrock-tab/tblRockGeoData.txt", path_base), n = 10)


## Join the nearest
azo_ngs <- azo_s %>%
  st_join(ngs, join = st_nearest_feature)
