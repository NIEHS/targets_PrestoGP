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
huc08 <- huc08[ext_mainland_huc, ]
# huc08 = terra::project(huc08, "EPSG:5070")
huc08 <- huc08[, "huc8"]
huc08 <- st_as_sf(huc08)
huc08$huc_split <- substr(huc08$huc8, 1, 4)
# subset
fname <- paste0(path_output, "HUC08_Geology.csv")

huc08_split <- split(huc08, huc08$huc_split)
# shp_sgmc08 = lapply(huc08_split,
#     function(huc) {
#         huc_e = st_transform(huc, st_crs(shp_sgmc))
#         shp_sgmc[st_as_sfc(st_bbox(huc_e)),]
#     })


huc08_split %>%
  future.apply::future_lapply(function(huc) {
    huc <- st_transform(huc, st_crs(shp_sgmc))
    huc08_geology <- st_join(huc[, 1], shp_sgmc)
    huc08_geology <- huc08_geology %>%
      st_drop_geometry() %>%
      dplyr::group_by(huc8) %>%
      dplyr::mutate(combined = paste0(unique(UNIT_NAME), collapse = "|")) %>%
      # tidyr::nest() %>%
      dplyr::summarize(combined = unique(combined)) %>%
      dplyr::ungroup()
    huc08_geology <- bind_cols(
      huc08 = huc08_geology[, 1],
      strsplit(huc08_geology$combined, "|", fixed = TRUE) %>%
        lapply(function(x) matrix(x, nrow = 1) %>% data.frame()) %>%
        do.call(bind_rows, .)
    )
    return(huc08_geology)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)
gc()

# future.apply::future_mapply(function(sgmc, huc) {
#         huc = st_transform(huc, st_crs(sgmc))
#         #sub_sgmc = shp_sgmc[st_as_sfc(st_bbox(k)),]
#         huc08_geology = st_join(huc, sgmc)
#         huc08_geology = huc08_geology %>%
#             st_drop_geometry() %>%
#             dplyr::group_by(huc8) %>%
#             tidyr::nest() %>%
#             dplyr::transmute(combined = map_chr(data, ~paste(.x, collapse = "|"))) %>%
#             dplyr::ungroup()
#         return(huc08_geology)
#     }, shp_sgmc08, huc08_split, future.seed = TRUE, SIMPLIFY = FALSE) %>%
#     do.call(dplyr::bind_rows, .) %>%
#     write.csv(., fname, row.names = FALSE)



huc10 <- sf::read_sf(path_wbd, layer = "WBDHU10")
huc10 <- huc10[ext_mainland_huc, ]
# huc10 = terra::project(huc10, "EPSG:5070")
huc10 <- huc10[, "huc10"]
huc10$huc_split <- substr(huc10$huc10, 1, 4)
# subset
huc10 <- huc10[which(huc10$huc_split %in% unique(huc10$huc_split)), ]
fname <- paste0(path_output, "HUC10_Geology.csv")

huc10_split <- split(huc10, huc10$huc_split)
# shp_sgmc10 = lapply(huc10_split,
#     function(huc) {
#         shp_sgmc[st_as_sfc(st_bbox(k)),]
#     })

huc10_split %>%
  future.apply::future_lapply(function(huc) {
    huc <- st_transform(huc, st_crs(shp_sgmc))
    huc10_geology <- st_join(huc[, 1], shp_sgmc)
    huc10_geology <- huc10_geology %>%
      st_drop_geometry() %>%
      dplyr::group_by(huc10) %>%
      dplyr::mutate(combined = paste0(unique(UNIT_NAME), collapse = "|")) %>%
      # tidyr::nest() %>%
      dplyr::summarize(combined = unique(combined)) %>%
      dplyr::ungroup()
    huc10_geology <- bind_cols(
      huc10 = huc10_geology[, 1],
      strsplit(huc10_geology$combined, "|", fixed = TRUE) %>%
        lapply(function(x) matrix(x, nrow = 1) %>% data.frame()) %>%
        do.call(bind_rows, .)
    )
    return(huc10_geology)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)
gc()

# future.apply::future_mapply(function(sgmc, huc) {
#         huc = st_transform(huc, st_crs(sgmc))
#         huc10_geology = st_join(huc, sgmc)
#         huc10_geology = huc10_geology %>%
#             st_drop_geometry() %>%
#             dplyr::group_by(huc10) %>%
#             tidyr::nest() %>%
#             dplyr::transmute(combined = map_chr(data, ~paste(.x, collapse = "|"))) %>%
#             dplyr::ungroup()
#         return(huc10_geology)
#     }, shp_sgmc10, huc10_split, future.seed = TRUE, SIMPLIFY = FALSE) %>%
#     do.call(dplyr::bind_rows, .) %>%
#     write.csv(., fname, row.names = FALSE)




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


# future.apply::future_mapply(function(sgmc, huc) {
#         huc = st_transform(huc, st_crs(sgmc))
#         #sub_sgmc = shp_sgmc[st_as_sfc(st_bbox(k)),]
#         huc12_geology = st_join(huc, sgmc)
#         huc12_geology = huc12_geology %>%
#             st_drop_geometry() %>%
#             dplyr::group_by(huc12) %>%
#             tidyr::nest() %>%
#             dplyr::transmute(combined = map_chr(data, ~paste(.x, collapse = "|"))) %>%
#             dplyr::ungroup()
#         return(huc12_geology)
#     }, shp_sgmc12, huc12_split, future.seed = TRUE, SIMPLIFY = FALSE) %>%
#     do.call(dplyr::bind_rows, .) %>%
#     write.csv(., fname, row.names = FALSE)
