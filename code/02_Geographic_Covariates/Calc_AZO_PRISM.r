######## pacman initialization ########
library(pacman)
p_load(terra, sf, dplyr, purrr, future, future.apply, exactextractr, conflicted)
sf_use_s2(FALSE)

## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 2
path_base <-
  ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/",
    ifelse(COMPUTE_MODE == 2,
      "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/",
      ifelse(COMPUTE_MODE == 3,
        "/opt/", stop("COMPUTE_MODE should be one of 1, 2, or 3.\n")
      )
    )
  )

doFuture::registerDoFuture()
future::plan(future::multicore, workers = 40L)

# abbrev: [aqui]fer, [geol]ogy, [ecor]egions
path_aqui <- file.path(path_base, "input/Aquifers/aquifrp025/")
path_ecor <- file.path(path_base, "input/Ecoregion/")
# path_geol = paste0(path_base, "input/Geology/USGS_StateGeologicMapCompilation_ver1.1.gdb")
path_geol <- file.path(path_base, "input/Geology/SGMC_Geology.gpkg")
path_wbd <- file.path(path_base, "input/WBD-National/WBD_National_GPKG.gpkg")

path_output <- "output/"

ext_mainland <- c(xmin = -126, xmax = -64, ymin = 22, ymax = 51)
ext_mainland <- terra::ext(ext_mainland)


# assumptions: we know the file extensions of entries in each path
# cleaning
## aqui: single file (reprojection is required)
## ecor: xlsx files (concatenation by HUC)
## geol: check layers, then choose the geologic maps only (reprojection may be required)
shp_aqui <- sf::read_sf(path_aqui |> paste0("aquifrp025.shp"))
### alluvial/glacial aquifers


# xlsx_ecor = list.files(path = path_ecor, pattern = "*.xlsx$", full.names = TRUE)
# xlsx_ecor08 = grep("20200506", xlsx_ecor, value = TRUE)
# xlsx_ecor12 = grep("20230425", xlsx_ecor, value = TRUE)
# xlsx_ecor08 = xlsx_ecor08[-length(xlsx_ecor08)]
# xlsx_ecor12 = xlsx_ecor12[-length(xlsx_ecor12)]

# tbl_ecor08 = lapply(xlsx_ecor08, \(x) readxl::read_excel(x, sheet = 4))
# tbl_ecor12 = lapply(xlsx_ecor12, \(x) readxl::read_excel(x, sheet = 4))

# # TODO: ecor tables should be prefiltered with column names that we need to consider
# tbl_ecor08 = do.call(dplyr::bind_rows, tbl_ecor08)
# tbl_ecor12 = do.call(dplyr::bind_rows, tbl_ecor12)


## geol
# lyrs_sgmc = sf::st_layers(path_geol)
# target is SGMC_Geology
# shp_sgmc = sf::read_sf(path_geol, layer = "SGMC_Geology")
# v_sgmc = terra::vect(path_geol, layer = "SGMC_Geology")
# shp_sgmc %>% st_transform("EPSG:5070") %>% write_sf(path_base %>% paste0("input/Geology/SGMC_Geology.gpkg"))



## prism
path_prism <- file.path(path_base, "input/PRISM/")
bils_prism = list.files(path_prism, pattern = ".bil$", recursive = TRUE, full.names = TRUE)
# dirs_prism_string = strsplit(dirs_prism, split = "_") %>%
#     lapply(function(x) x[2]) %>%
#     Reduce(c, .)
# dirs_prism_string[10] = "us_dem"
rast_prism <- terra::rast(bils_prism)
# rast_prism = lapply(bils_prism, terra::rast) %>%
#     Reduce(f = c, x = .)
# names(rast_prism) = dirs_prism_string
# terra::writeCDF(rast_prism, "~/Documents/PRISM_4km_combined.nc4")

# # hard-coded layer names for direct CDF reading
dirs_prism_string =
    c("ppt", "solclear", "solslope", "soltotal", "soltrans", "tdmean",
     "tmax", "tmean", "tmin", "us_dem", "vpdmax", "vpdmin")
# ## ... edit dirs_prism if necessary....
# rast_prism = terra::rast(paste0(path_prism, "PRISM_4km_combined.nc"))
names(rast_prism) = dirs_prism_string
# rast_prism
# ## wbd

## To run the main part, uncomment until "HUC-aquifers" header
huc08 = terra::vect(path_wbd, layer = "WBDHU8")
huc08 = huc08[ext_mainland, ]
huc08 = huc08[,'huc8']
huc08$huc_split = substr(huc08$huc8, 1, 4)
# subset
huc08 = huc08[which(huc08$huc_split %in% unique(huc08$huc_split)),]
fname = paste0(path_output, "HUC08_PRISM_mean.csv")

prism_huc08 <-
  split(huc08, huc08$huc_split) %>%
  future.apply::future_lapply(function(k) {
    prism <- terra::crop(rast_prism, terra::ext(k))
    huc08_prism <-
      exactextractr::exact_extract(
        prism,
        st_as_sf(k),
        function(x, w) mean(x, w = w, na.rm = TRUE),
        stack_apply = TRUE,
        force_df = TRUE
      )

    huc08_prism$HUC <- k$huc8
    return(huc08_prism)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)
gc()

huc10 = terra::vect(path_wbd, layer = "WBDHU10")
huc10 = huc10[ext_mainland,]
huc10 = huc10[,'huc10']
huc10$huc_split = substr(huc10$huc10, 1, 4)
# subset
huc10 = huc10[which(huc10$huc_split %in% unique(huc10$huc_split)),]
fname = paste0(path_output, "HUC10_PRISM_mean.csv")

prism_huc10 <-
  split(huc10, huc10$huc_split) %>%
  future.apply::future_lapply(function(k) {
    prism <- terra::crop(rast_prism, terra::ext(k))
    huc10_prism <-
      exactextractr::exact_extract(
        prism,
        st_as_sf(k),
        function(x, w) mean(x, w = w, na.rm = TRUE),
        stack_apply = TRUE,
        force_df = TRUE
      )

    huc10_prism$HUC <- k$huc10
    return(huc10_prism)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)
gc()

huc12 = terra::vect(path_wbd, layer = "WBDHU12", filter = terra::vect(ext_mainland))
# huc12 = huc12[ext_mainland,]
huc12 = huc12[,'huc12']
huc12$huc_split = substr(huc12$huc12, 1, 6)
# subset
huc12 = huc12[which(huc12$huc_split %in% unique(huc12$huc_split)),]
fname = paste0(path_output, "HUC12_PRISM_mean.csv")

prism_huc12 <-
  split(huc12, huc12$huc_split) %>%
  future.apply::future_lapply(function(k) {
    prism <- terra::crop(rast_prism, terra::ext(k))
    huc12_prism <-
      exactextractr::exact_extract(
        prism,
        st_as_sf(k),
        function(x, w) mean(x, w = w, na.rm = TRUE),
        stack_apply = TRUE,
        force_df = TRUE
      )

    huc12_prism$HUC <- k$huc12
    return(huc12_prism)
  }, future.seed = TRUE) %>%
  do.call(dplyr::bind_rows, .) %>%
  write.csv(., fname, row.names = FALSE)

