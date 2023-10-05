######## pacman initialization ########
library(pacman)
p_load(terra, sf, dplyr, purrr, future, future.apply, exactextractr, conflicted)
sf_use_s2(FALSE)

## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 3
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

NUM_WORKERS <- 10

future::plan(multicore, workers = NUM_WORKERS)

# abbrev: [aqui]fer, [geol]ogy, [ecor]egions
path_aqui <- paste0(path_base, "Aquifers/aquifrp025/")
path_ecor <- paste0(path_base, "Ecoregion/")
# path_geol = paste0(path_base, "input/Geology/USGS_StateGeologicMapCompilation_ver1.1.gdb")
path_geol <- paste0(path_base, "Geology/SGMC_Geology.gpkg")
path_wbd <- paste0(path_base, "WBD-National/WBD_National_GPKG.gpkg")

path_output <- "/mnt/"

## geol
# lyrs_sgmc = sf::st_layers(path_geol)
# target is SGMC_Geology
shp_sgmc <- sf::read_sf(path_geol, layer = "SGMC_Geology")

## wbd
ext_mainland <- c(xmin = -126, xmax = -72, ymin = 24, ymax = 51)
ext_mainland <- terra::ext(ext_mainland) %>%
  st_bbox(crs = 4326) %>%
  st_as_sfc()
ext_mainland <- ext_mainland %>%
  st_transform(st_crs(shp_sgmc))

## Main extraction ####
### Geology spatial join ####
## Key field is UNIT_NAME
shp_sgmc <- shp_sgmc[, "UNIT_NAME"]

huc08 <- sf::read_sf(path_wbd, layer = "WBDHU8")
ext_mainland_huc <- sf::st_transform(ext_mainland, st_crs(huc08))





huc12 <- sf::read_sf(path_wbd, layer = "WBDHU12")
huc12 <- huc12[ext_mainland_huc, ]
huc12 <- huc12[, "huc12"]
huc12$huc_split <- substr(huc12$huc12, 1, 6)
# subset
huc12 <- huc12[which(huc12$huc_split %in% unique(huc12$huc_split)), ]
fname <- paste0(path_output, "HUC12_Geology.csv")


huc12_split <- split(huc12, huc12$huc_split)
# shp_sgmc10 = lapply(huc10_split,
#     function(huc) {
#         shp_sgmc[st_as_sfc(st_bbox(k)),]
#     })

huc12_split %>%
  future.apply::future_lapply(function(huc) {
    huc <- st_transform(huc, st_crs(shp_sgmc))
    huc12_geology <- st_join(huc[, 1], shp_sgmc)
    huc12_geology <- huc12_geology %>%
      st_drop_geometry() %>%
      dplyr::group_by(huc12) %>%
      dplyr::mutate(combined = paste0(unique(UNIT_NAME), collapse = "|")) %>%
      # tidyr::nest() %>%
      dplyr::summarize(combined = unique(combined)) %>%
      dplyr::ungroup()
    huc12_geology <- bind_cols(
      huc12 = huc12_geology[, 1],
      strsplit(huc12_geology$combined, "|", fixed = TRUE) %>%
        lapply(function(x) matrix(x, nrow = 1) %>% data.frame()) %>%
        do.call(bind_rows, .)
    )
    return(huc12_geology)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)
gc()
