pkgs <- c("terra", "sf", "dplyr", "purrr", "future", "future.apply", "exactextractr", "conflicted")
invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))
sf_use_s2(FALSE)

## data path determination
## compute_mode 
## 1 (wine mount), 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 2
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
ext_mainland <- c(xmin = -126, xmax = -64, ymin = 22, ymax = 51)
ext_mainland <- terra::ext(ext_mainland)

path_tc <- file.path(path_base, "terraClimate/NetCDF/")
path_tc_files <- list.files(
  path = path_tc, pattern = "*.nc$",
  full.names = TRUE
)

## AZO
azo <- sf::read_sf(paste0(path_base, "data_process/data_AZO_watershed_huc_join.shp"))
azo_s <- azo %>%
  dplyr::select(site_no, Year, starts_with("huc")) %>%
  unique() %>%
  st_transform("EPSG:4326")


# some bands should be summed
bandnames <- c(
  "aet", "def", "PDSI", "pet", "ppt", "q", "soil", "srad",
  "swe", "tmax", "tmin", "vap", "vpd", "ws"
)
bandnames_sorted <- sort(bandnames)


# aet (Actual Evapotranspiration, monthly total), units = mm
# def (Climate Water Deficit, monthly total), units = mm
# PDSI (Palmer Drought Severity Index, at end of month), units = unitless
# pet (Potential evapotranspiration, monthly total), units = mm
# ppt (Precipitation, monthly total), units = mm
# q (Runoff, monthly total), units = mm
# soil (Soil Moisture, total column - at end of month), units = mm
# srad (Downward surface shortwave radiation), units = W/m2
# swe (Snow water equivalent - at end of month), units = mm
# tmax (Max Temperature, average for month), units = C
# tmin (Min Temperature, average for month), units = C
# vap (Vapor pressure, average for month), units  = kPa
# vpd (Vapor Pressure Deficit, average for month), units = kpa
# ws (Wind speed, average for month), units = m/s
# sum: aet, def, pet, ppt, q, soil, swe(?)
# mean: PDSI, srad, tmax(?), tmin(?), vap, vpd, ws


# single nc file, yearly aggregation by fun value
preprocess <- function(ras, fun) {
  terra::tapp(ras, "years", fun)
}


## new strategy (10062023): take sum and mean then choose one if necessary
# band for summation
bandnames_sum <- bandnames#[c(...)]

# band for averaging
bandnames_avg <- bandnames#[c(...)]

# description
# mean: temporally marginal pixel mean (i.e., monthly -> yearly)
# sum: temporally marginal pixel sum (i.e., monthly -> yearly)
# Lines below take time to run.
# Preprocessed data are stored in
# input/terraClimate/terraClimate_sum.nc and input/terraClimate/terraClimate_mean.nc
# netcdf_read_sum <-
#   split(bandnames, bandnames) |>
#   lapply(function(x) {
#     grep(paste0("(", x, ")"), path_tc_files, value = TRUE)
#   }) |>
#   lapply(function(x) {
#     preprocess(terra::rast(x, win = ext_mainland, snap = "out"), "sum")
#   }) |>
#   Reduce(f = c, x = _)

# netcdf_read_mean <-
#   split(bandnames, bandnames) |>
#   lapply(function(x) {
#     grep(paste0("(", x, ")"), path_tc_files, value = TRUE)
#   }) |>
#   lapply(function(x) {
#     preprocess(terra::rast(x, win = ext_mainland, snap = "out"), "mean")
#   }) |>
#   Reduce(f = c, x = _)

# nrs <- netcdf_read_sum
# nrm <- netcdf_read_mean

# ## experiment: retain netcdf spec
# nrsds <-
#   lapply(
#     seq_along(bandnames),
#     function(b) {
#       index <- ((b - 1) * 23) + seq(1, 23)
#       ni <- netcdf_read_sum[[index]]
#       names(ni) <- rep(bandnames[b], 23)
#       return(ni)
#     }
#   ) |>
#   do.call(sds, args = _)
# varnames(nrsds) <- sprintf("sum_%s", bandnames)
# writeCDF(nrsds, paste0(path_base, "terraClimate/terraClimate_sum.nc"), overwrite = TRUE)

# nrmds <-
#   lapply(
#     seq_along(bandnames),
#     function(b) {
#       index <- ((b - 1) * 23) + seq(1, 23)
#       ni <- netcdf_read_mean[[index]]
#       names(ni) <- rep(bandnames[b], 23)
#       return(ni)
#     }
#   ) |>
#   do.call(sds, args = _)
# varnames(nrmds) <- sprintf("mean_%s", bandnames)
# writeCDF(nrmds, paste0(path_base, "terraClimate/terraClimate_mean.nc"), overwrite = TRUE)

## preprocessing code ends ####

yearrange <- seq(2000, 2022)

## HUC extraction
path_wbd <- file.path(path_base, "WBD-National/WBD_National_GPKG.gpkg")

huc08 <- terra::vect(path_wbd, layer = "WBDHU8")
huc08 <- huc08[ext_mainland, ]
huc08 <- huc08[,'huc8']
huc08$huc_split <- substr(huc08$huc8, 1, 4)
# subset
huc08 <- huc08[which(huc08$huc_split %in% unique(huc08$huc_split)),]
fname <- paste0(path_output, "HUC08_terraClimate_mean.csv")

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

huc10 <- terra::vect(path_wbd, layer = "WBDHU10")
huc10 <- huc10[ext_mainland,]
huc10 <- huc10[, "huc10"]
huc10$huc_split <- substr(huc10$huc10, 1, 4)
# subset
huc10 <- huc10[which(huc10$huc_split %in% unique(huc10$huc_split)),]
fname <- paste0(path_output, "HUC10_PRISM_mean.csv")

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

huc12 <- terra::vect(path_wbd, layer = "WBDHU12", filter = terra::vect(ext_mainland))
huc12 <- huc12[, "huc12"]
huc12$huc_split <- substr(huc12$huc12, 1, 6)
# subset
huc12 <- huc12[which(huc12$huc_split %in% unique(huc12$huc_split)),]
fname <- paste0(path_output, "HUC12_PRISM_mean.csv")

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



# point extract (deprecated )... ####
extract_and_clean <-
  function(
    ras0, pnt0, poly0, pnt_uniqueid = "site_no", poly_uniqueid = "huc8"
  ) {
    filt <- unique(unlist(pnt0[[pnt_uniqueid]]))
    poly0p <- poly0[unlist(poly0[[poly_uniqueid]]) %in% filt, ]
    extracted <- exactextractr::exact_extract(
      ras0,
      sf::st_as_sf(poly0p),
      fun = function(x, w) mean(x, w = w, na.rm = TRUE),
      stack_apply = TRUE,
      force_df = TRUE,
      append_cols = poly_uniqueid
    )
    return(extracted)
  }

extract_by_year <- function(ras, pnt, poly, pnt_id, poly_id, suffix = NULL) {
  years <- seq(2000, 2022)
  pnt_list <- split(pnt, unlist(pnt[["Year"]]))
  if (!is.null(suffix)) {
    suffix <- paste0("_", suffix)
  } else {
    suffix <- ""
  }
  future::plan(future::multicore, workers = 23)

  extracted <-
    future.apply::future_mapply(
      FUN = \(r, pnt) {
        PNT_UID <- pnt_id
        POLY_UID <- poly_id
        ras <- terra::rast(ras)
        rasout <- subset(ras, time(ras) == r)
        extr0 <- extract_and_clean(rasout, pnt, poly0 = poly, PNT_UID, POLY_UID)
        # without unlist, the file export will result in 9.8GB per file.
        extr0[["year"]] <- unique(unlist(pnt[["Year"]]))
        extr0 <-
          stats::setNames(
            extr0,
            c(POLY_UID,
              sprintf("tclim%s_%s_%s",
                      suffix,
                      bandnames_sorted,
                      PNT_UID
                      ),
              "year")
          )
        return(extr0)
      },
      years, pnt_list, SIMPLIFY = FALSE,
      future.seed = TRUE
    )
  extracted <- Reduce(dplyr::bind_rows, extracted)
  future::plan(future::sequential)
  return(extracted)
}

# run per HUC level
terraclimate_huc08_sum <- extract_by_year(
  file.path(path_base, "terraClimate/terraClimate_sum.nc"),
  azo_s,
  st_as_sf(huc08),
  "huc08",
  "huc8",
  "sum"
)
terraclimate_huc10_sum <- extract_by_year(
  file.path(path_base, "terraClimate/terraClimate_sum.nc"),
  azo_s,
  st_as_sf(huc10),
  "huc10",
  "huc10",
  "sum"
)
terraclimate_huc12_sum <- extract_by_year(
  file.path(path_base, "terraClimate/terraClimate_sum.nc"),
  azo_s,
  st_as_sf(huc12),
  "huc12",
  "huc12",
  "sum"
)

terraclimate_huc08_mean <- extract_by_year(
  file.path(path_base, "terraClimate/terraClimate_mean.nc"),
  azo_s,
  st_as_sf(huc08),
  "huc08",
  "huc8",
  "mean"
)
terraclimate_huc10_mean <- extract_by_year(
  file.path(path_base, "terraClimate/terraClimate_mean.nc"),
  azo_s,
  st_as_sf(huc10),
  "huc10",
  "huc10",
  "mean"
)
terraclimate_huc12_mean <- extract_by_year(
  file.path(path_base, "terraClimate/terraClimate_mean.nc"),
  azo_s,
  st_as_sf(huc12),
  "huc12",
  "huc12",
  "mean"
)

dir_output <- gsub("input", "output", path_base)

# save intermediate results (will be used to join to the "big table")
write.csv(terraclimate_huc08_sum,
  paste0(dir_output, "terraclimate_huc08_azo_sum.csv"),
  row.names = FALSE)
write.csv(terraclimate_huc10_sum,
  paste0(dir_output, "terraclimate_huc10_azo_sum.csv"),
  row.names = FALSE)
write.csv(terraclimate_huc12_sum,
  paste0(dir_output, "terraclimate_huc12_azo_sum.csv"),
  row.names = FALSE)

write.csv(terraclimate_huc08_mean,
  paste0(dir_output, "terraclimate_huc08_azo_mean.csv"),
  row.names = FALSE)
write.csv(terraclimate_huc10_mean,
  paste0(dir_output, "terraclimate_huc10_azo_mean.csv"),
  row.names = FALSE)
write.csv(terraclimate_huc12_mean,
  paste0(dir_output, "terraclimate_huc12_azo_mean.csv"),
  row.names = FALSE)
