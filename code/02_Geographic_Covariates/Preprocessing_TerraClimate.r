library(pacman)
p_load(terra, sf, dplyr, purrr, future, future.apply, exactextractr, conflicted)
sf_use_s2(FALSE)

## data path determination
## compute_mode 
## 1 (wine mount), 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 1
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

## wbd
ext_mainland <- c(xmin = -126, xmax = -64, ymin = 24, ymax = 51)
ext_mainland <- terra::ext(ext_mainland)

path_tc <- paste0(path_base, "terraClimate/NetCDF/")
path_tc_files <- list.files(
  path = path_tc, pattern = "*.nc$",
  full.names = TRUE
)


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
netcdf_read_median <-
  split(bandnames, bandnames) |>
  lapply(function(x) {
    grep(paste0("(", x, ")"), path_tc_files, value = TRUE)
  }) |>
  lapply(function(x) {
    xr <- Reduce(c, lapply(x, terra::rast)) |>
      preprocess(ext_mainland, "median")
    names(xr) <- regmatches(x, 
      regexpr(paste0("(", 
        paste0(bandnames, collapse = "|"), ")"), x))
  }) |>
  Reduce(f = c, x = _)




# band for summation
bandnames_sum <- bandnames[c(...)]

# band for averaging
bandnames_avg <- bandnames[c(...)]


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
