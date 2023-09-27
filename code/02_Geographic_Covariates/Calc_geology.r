######## pacman initialization ########
if (!require(pacman)) {
    install.packages('pacman')
    library(pacman)
    }

p_load(terra, sf, dplyr, tidytable, future, exactextractr, conflicted)
sf_use_s2(FALSE)

## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE = 3
path_base = 
    ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/",
    ifelse(COMPUTE_MODE == 2,
    "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/",
    ifelse(COMPUTE_MODE == 3,
    "/opt/", stop("COMPUTE_MODE should be one of 1, 2, or 3.\n"))))

future::plan(multicore, workers = 16)

# abbrev: [aqui]fer, [geol]ogy, [ecor]egions
path_aqui = paste0(path_base, "input/Aquifers/aquifrp025/")
path_ecor = paste0(path_base, "input/Ecoregion/")
# path_geol = paste0(path_base, "input/Geology/USGS_StateGeologicMapCompilation_ver1.1.gdb")
path_geol = paste0(path_base, "input/Geology/SGMC_Geology.gpkg")
path_wbd = paste0(path_base, "input/WBD-National/WBD_National_GPKG.gpkg")

path_output = "/mnt/"

## geol
# lyrs_sgmc = sf::st_layers(path_geol)
# target is SGMC_Geology
shp_sgmc = sf::read_sf(path_geol, layer = "SGMC_Geology")


## Main extraction ####
### Geology spatial join ####
## Key field is UNIT_NAME
shp_sgmc = shp_sgmc[,"UNIT_NAME"]

huc08 = terra::vect(wbdpath, layer = "WBDHU8")
huc08 = huc08[ext_mainland,]
#huc08 = terra::project(huc08, "EPSG:5070")
huc08 = huc08[,'huc8']
huc08 = st_as_sf(huc08)
huc08$huc_split = substr(huc08$huc8, 1, 4)
# subset
fname = paste0(path_output, "HUC08_Geology.csv")
# fname = gsub("\\.tif", "_huc08_ext.csv", x)
# fname = gsub("/opt/PRISM", "/mnt", fname)

split(huc08, huc08$huc_split) %>%
    future_lapply(function(k) {
        k = st_transform(k, st_crs(shp_sgmc))
        sub_sgmc = shp_sgmc[st_as_sfc(st_bbox(k)),]
        huc08_geology = st_join(k, sub_sgmc)
        huc08_geology = huc08_geology %>%
            st_drop_geometry() %>%
            dplyr::group_by(huc8) %>%
            tidyr::nest() %>% 
            dplyr::transmute(combined = map_chr(data, ~paste(.x, collapse = "|"))) 
            dplyr::ungroup()
        return(huc08_geology)
    }) %>%
    do.call(dplyr::bind_rows, .) %>%
    write.csv(., fname, row.names = FALSE)
gc()

huc10 = terra::vect(wbdpath, layer = "WBDHU10")
huc10 = huc10[ext_mainland,]
#huc10 = terra::project(huc10, "EPSG:5070")
huc10 = huc10[,'huc10']
huc10$huc_split = substr(huc10$huc10, 1, 4)
# subset
huc10 = huc10[which(huc10$huc_split %in% unique(huc10$huc_split)),]
fname = paste0(path_output, "HUC10_Geology.csv")

split(huc10, huc10$huc_split) %>%
    future_lapply(function(k) {
        k = st_transform(k, st_crs(shp_sgmc))
        sub_sgmc = shp_sgmc[st_as_sfc(st_bbox(k)),]
        huc10_geology = st_join(k, sub_sgmc)
        huc10_geology = huc10_geology %>%
            st_drop_geometry() %>%
            dplyr::group_by(huc10) %>%
            tidyr::nest() %>% 
            dplyr::transmute(combined = map_chr(data, ~paste(.x, collapse = "|"))) 
            dplyr::ungroup()
        return(huc10_geology)
    }) %>%
    do.call(dplyr::bind_rows, .) %>%
    write.csv(., fname, row.names = FALSE)
gc()

huc12 = terra::vect(wbdpath, layer = "WBDHU12")
huc12 = huc12[ext_mainland,]
#huc12 = terra::project(huc12, "EPSG:5070")
huc12 = huc12[,'huc12']
huc12$huc_split = substr(huc12$huc12, 1, 6)
# subset
huc12 = huc12[which(huc12$huc_split %in% unique(huc12$huc_split)),]
fname = paste0(path_output, "HUC12_Geology.csv")

split(huc12, huc12$huc_split) %>%
    future_lapply(function(k) {
        k = st_transform(k, st_crs(shp_sgmc))
        sub_sgmc = shp_sgmc[st_as_sfc(st_bbox(k)),]
        huc10_geology = st_join(k, sub_sgmc)
        huc10_geology = huc12_geology %>%
            st_drop_geometry() %>%
            dplyr::group_by(huc12) %>%
            tidyr::nest() %>% 
            dplyr::transmute(combined = map_chr(data, ~paste(.x, collapse = "|"))) 
            dplyr::ungroup()
        return(huc12_geology)
    }) %>%
    do.call(dplyr::bind_rows, .) %>%
    write.csv(., fname, row.names = FALSE)

