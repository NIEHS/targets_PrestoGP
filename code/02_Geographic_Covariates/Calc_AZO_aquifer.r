
## Load packages
source("code/00_Load_Packages/Load_Packages.r")


### HUC-aquifers ####
# abbrev: [aqui]fer, [geol]ogy, [ecor]egions
path_aqui <- file.path(path_base, "input/Aquifers/aquifrp025/")
path_ecor <- file.path(path_base, "input/Ecoregion/")
# path_geol = paste0(path_base, "input/Geology/USGS_StateGeologicMapCompilation_ver1.1.gdb")
path_geol <- file.path(path_base, "input/Geology/SGMC_Geology.gpkg")
path_wbd <- file.path(path_base, "input/WBD-National/WBD_National_GPKG.gpkg")

path_output <- "output/"

ext_mainland <- c(xmin = -126, xmax = -64, ymin = 22, ymax = 51)
ext_mainland <- terra::ext(ext_mainland)

####
huc08 <- terra::vect(path_wbd, layer = "WBDHU8")
huc08 <- huc08[ext_mainland, ]
huc08 <- huc08[, "huc8"]
huc08 <- st_as_sf(huc08)
huc08$huc_split <- substr(huc08$huc8, 1, 4)
# subset
fname <- paste0(path_output, "HUC08_Aquifer.csv")
# fname = gsub("\\.tif", "_huc08_ext.csv", x)
# fname = gsub("/opt/PRISM", "/mnt", fname)
sf_aqui <- st_as_sf(shp_aqui)[, "AQ_NAME"]

split(huc08, huc08$huc_split) %>%
  future.apply::future_lapply(function(k) {
    huc08_aquifer <- st_join(k[, "huc8"], sf_aqui)
    huc08_aquifer <- huc08_aquifer %>%
      st_drop_geometry() %>%
      dplyr::group_by(huc8) %>%
      tidyr::nest() %>%
      dplyr::transmute(combined = map_chr(data, ~ paste(.x, collapse = "|"))) %>%
      dplyr::ungroup()
    return(huc08_aquifer)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)
gc()

huc10 <- terra::vect(path_wbd, layer = "WBDHU10")
huc10 <- huc10[ext_mainland, ]
# huc10 = terra::project(huc10, "EPSG:5070")
huc10 <- huc10[, "huc10"]
huc10 <- st_as_sf(huc10)
huc10$huc_split <- substr(huc10$huc10, 1, 4)
# subset
huc10 <- huc10[which(huc10$huc_split %in% unique(huc10$huc_split)), ]
fname <- paste0(path_output, "HUC10_Aquifer.csv")

# fname = gsub("\\.tif", "_huc10_ext.csv", x)
# fname = gsub("/opt/USDA_NASS", "/mnt", fname)
split(huc10, huc10$huc_split) %>%
  future.apply::future_lapply(function(k) {
    huc10_aquifer <- st_join(k[, "huc10"], sf_aqui)
    huc10_aquifer <- huc10_aquifer %>%
      st_drop_geometry() %>%
      dplyr::group_by(huc10) %>%
      tidyr::nest() %>%
      dplyr::transmute(combined = map_chr(data, ~ paste(.x, collapse = "|"))) %>%
      dplyr::ungroup()
    return(huc10_aquifer)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)
gc()

huc12 <- terra::vect(path_wbd, layer = "WBDHU12")
huc12 <- huc12[ext_mainland, ]
# huc12 = terra::project(huc12, "EPSG:5070")
huc12 <- huc12[, "huc12"]
huc12 <- st_as_sf(huc12)
huc12$huc_split <- substr(huc12$huc12, 1, 6)
# subset
huc12 <- huc12[which(huc12$huc_split %in% unique(huc12$huc_split)), ]
fname <- paste0(path_output, "HUC12_Aquifer.csv")
# fname = gsub("\\.tif", "_huc12_ext.csv", x)
# fname = gsub("/opt/USDA_NASS", "/mnt", fname)
split(huc12, huc12$huc_split) %>%
  future.apply::future_lapply(function(k) {
    huc12_aquifer <- st_join(k[, "huc12"], sf_aqui)
    huc12_aquifer <- huc12_aquifer %>%
      st_drop_geometry() %>%
      dplyr::group_by(huc12) %>%
      tidyr::nest() %>%
      dplyr::transmute(combined = map_chr(data, ~ paste(.x, collapse = "|"))) %>%
      dplyr::ungroup()
    return(huc12_aquifer)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)
