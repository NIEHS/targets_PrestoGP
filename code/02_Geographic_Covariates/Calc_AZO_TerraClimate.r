library(pacman)
p_load(terra, sf, dplyr, purrr, future, future.apply, exactextractr, conflicted)
sf_use_s2(FALSE)

## data path determination
## compute_mode 
## 1 (wine mount), 2 (hpc compute node), 3 (container internal)
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

# path_base <- "~/Documents/"
## wbd
ext_mainland <- c(xmin = -126, xmax = -64, ymin = 24, ymax = 51)
ext_mainland <- terra::ext(ext_mainland)

path_tc <- paste0(path_base, "terraClimate/NetCDF/")
path_tc_files <- list.files(
  path = path_tc, pattern = "*.nc$",
  full.names = TRUE
)

## AZO
azo <- sf::read_sf(paste0(path_base, "data_process/data_AZO_watershed_huc_join.shp"))
azo_s <- azo %>%
  dplyr::select(site_no, Year) %>%
  unique() %>%
  st_transform("EPSG:4326")


# some bands should be summed
bandnames <- c(
  "aet", "def", "PDSI", "pet", "ppt", "q", "soil", "srad",
  "swe", "tmax", "tmin", "vap", "vpd", "ws"
)
bandnames_sorted <- sort(bandnames)


# aet: actual evapotranspiration -- sum
# def: climate water deficit soil water-balance -- sum
# pdsi: palmer drought severity index -- ?
# pet: reference evapotranspiration -- mean
# pr: precipitation accumulation -- sum
# ro: runoff -- sum
# soil: soil moisture -- mean
# srad: downward surface shortwave radiation -- ? -- mean/sum (explore)
# swe: snow water equivalent -- mean
# tmin: minimum temperature -- mean
# tmax: maximum temperature -- mean
# vap: vapor pressure -- mean
# vpd: vapor pressure deficit -- ?
# vs: wind-speed at 10 meters -- mean


# single nc file, yearly aggregation by fun value
preprocess <- function(ras, extent, fun) {
  ras <- terra::crop(ras, extent)
  terra::tapp(ras, "years", fun)
}

# all median: exploration
# netcdf_read_median <-
#   split(bandnames, bandnames) |>
#   lapply(function(x) {
#     grep(paste0("(", x, ")"), path_tc_files, value = TRUE)
#   }) |>
#   lapply(function(x) {
#     xr <- Reduce(c, lapply(x, terra::rast)) |>
#       preprocess(ext_mainland, "median")
#     names(xr) <- regmatches(x, 
#       regexpr(paste0("(", 
#         paste0(bandnames, collapse = "|"), ")"), x))
#   }) |>
#   Reduce(f = c, x = _)

# bandnames <- c("aet", "def")
# bandnames_sorted <- bandnames

## new strategy (10062023): take sum and mean then choose one if necessary
# band for summation
bandnames_sum <- bandnames#[c(...)]

# band for averaging
bandnames_avg <- bandnames#[c(...)]


netcdf_read_sum <-
  split(bandnames, bandnames) |>
  lapply(function(x) {
    grep(paste0("(", x, ")"), path_tc_files, value = TRUE)
  }) |>
  lapply(function(x) {
    Reduce(c, lapply(x, terra::rast)) |> preprocess(ext_mainland, "sum")
  }) |>
  Reduce(f = c, x = _)

netcdf_read_mean <-
  split(bandnames, bandnames) |>
  lapply(function(x) {
    grep(paste0("(", x, ")"), path_tc_files, value = TRUE)
  }) |>
  lapply(function(x) {
    Reduce(c, lapply(x, terra::rast)) |> preprocess(ext_mainland, "mean")
  }) |>
  Reduce(f = c, x = _)


##
# ... extract ...
extract_by_year <- function(ras, pnt) {
  years <- seq(2000, 2022)
  # split by year (raster)
  ras_list <- split(years, years) |>
    lapply(function(y) {
      rasout <- ras[as.character(y)]
      names(rasout) <- bandnames_sorted
      return(rasout)
    })
  
  pnt_list <- split(pnt, unlist(pnt[["Year"]]))
  
  extract_and_clean <- function(
    ras0, pnt0, pnt_uniqueid = "site_no") {
      #rownames(pnt0) <- unlist(pnt0[["site_no"]])
      pnt0[["ID"]] <- pnt0[[pnt_uniqueid]]
      extracted <- terra::extract(ras0, pnt0, ID = FALSE)
      #extracted[[pnt_uniqueid]] <- pnt0[[pnt_uniqueid]]
      return(extracted)
    }
  
  future::plan(future::multicore, workers = 24)
  
  extracted <- 
    future.apply::future_mapply(
      FUN = \(r, p) {
        PNT_UID <- "site_no"
        extr0 <- extract_and_clean(r, p, PNT_UID)
        extr0[["year"]] <- p[["Year"]]
        extr0[[PNT_UID]] <- p[[PNT_UID]]
        return(extr0)
      },
      ras_list, pnt_list, SIMPLIFY = FALSE
    )
  extracted <- Reduce(dplyr::bind_rows, extracted)

  return(extracted)
}

azo_t <- terra::vect(azo_s)
## exec extract_by_year
terra_pnt_sum <- extract_by_year(
  netcdf_read_sum, azo_t
)

terra_pnt_mean <- extract_by_year(
  netcdf_read_mean, azo_t
)

# dir_output <- "/mnt/"
dir_output <- gsub("input", "output", path_base)

write.csv(terra_pnt_sum, paste0(dir_output, "terraclimate_yearly_azo_sum.csv"), row.names = FALSE)
write.csv(terra_pnt_mean, paste0(dir_output, "terraclimate_yearly_azo_mean.csv"), row.names = FALSE)