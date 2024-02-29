### AZO - Soil Chemistry

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

## wbd
ext_mainland <- c(xmin = -128, xmax = -60, ymin = 22, ymax = 55)
ext_mainland <- terra::ext(ext_mainland) %>%
  st_bbox(crs = 4326) %>%
  st_as_sfc()
ext_mainland <- ext_mainland %>%
  st_transform(st_crs(shp_sgmc))


## soil
## geochemical survey
ngs <- read_sf(sprintf("%sngs/ngs.shp", path_base)) %>%
  st_transform(st_crs(azo_s))

## NGDB soil
ngdbsoil <- read_sf(sprintf("%sngdbsoil-csv/ngdbsoil/ngdbsoil.shp", path_base))
ngdbsoil_chem <- fread(sprintf("%sngdbsoil-csv/ngdbsoil/chemistry.csv", path_base))

ngdbsoil_chem_wide <- ngdbsoil_chem |>
  # tidytable::filter(qvalue >= 0) |>
  tidytable::select(lab_id, species, unit, qvalue) |>
  tidytable::pivot_wider(names_from = c(species, unit), values_from = qvalue,
    values_fn = \(x) if (all(x>=0)) mean(x, na.rm = T) else max(x, na.rm = T)) |>
  tidytable::mutate(tidytable::across(everything(), ~ifelse(is.nan(.), NA, .)))

ngdbsoil_attr <- ngdbsoil[ext_mainland, -2:-4] |>
  left_join(ngdbsoil_chem_wide, by = c("LAB_ID" = "lab_id"))

# ngdbsoil_chem[lab_id == "D-547288", ]

## bedrock
bedrock <- read_sf(sprintf("%sngdbrock/ngdbrock.shp", path_base))
bedrock_attr <- fread(sprintf("%sngdbrock-tab/tblRockGeoData.txt", path_base), sep = "\t", stringsAsFactors = TRUE, quote = "", na.strings = "NULL")
bedrock_chem <- fread(sprintf("%sngdbrock-tab/xtbMajorChem.txt", path_base), sep = "\t", stringsAsFactors = TRUE, quote = "", na.strings = "NULL")

# readLines(sprintf("%sngdbrock-tab/tblRockGeoData.txt", path_base), n = 10)
brattr_sf <- st_as_sf(bedrock_attr, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
brattr_sf <- brattr_sf |>
  dplyr::filter(!is.na(XNDRYCLASS) & !is.na(GEOL_AGE)) |>
  dplyr::select(LAB_ID, XNDRYCLASS, GEOL_AGE)
# plot(brattr_sf[,"SPEC_NAME"])

## Join the nearest
azo_ngs <- azo_s %>%
  st_join(ngs, join = st_nearest_feature) %>%
  st_join(brattr_sf, join = st_nearest_feature) %>%
  st_join(ngdbsoil_attr, join = st_nearest_feature)

write.csv(st_drop_geometry(azo_ngs), sprintf("%s/azo_soilchemistry.csv", path_base), row.names = FALSE)
