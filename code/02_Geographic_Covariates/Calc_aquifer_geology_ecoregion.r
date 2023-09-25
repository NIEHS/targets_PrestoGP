######## pacman initialization ########
if (!require(pacman)) {
    install.packages('pacman')
    library(pacman)
    }

p_load(terra, sf, dplyr, tidytable, conflicted)
sf_use_s2(FALSE)

## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE = 1
path_base = 
    ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/",
    ifelse(COMPUTE_MODE == 2,
    "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/",
    ifelse(COMPUTE_MODE == 3,
    "/opt/", warning("COMPUTE_MODE should be one of 1, 2, or 3.\n"))))

# abbrev: [aqui]fer, [geol]ogy, [ecor]egions
path_aqui = paste0(path_base, "input/Aquifers/aquifrp025/")
path_ecor = paste0(path_base, "input/Ecoregion/")
path_geol = paste0(path_base, "input/Geology/USGS_StateGeologicMapCompilation_ver1.1.gdb")

# assumptions: we know the file extensions of entries in each path
# cleaning
## aqui: single file (reprojection is required)
## ecor: xlsx files (concatenation by HUC)
## geol: check layers, then choose the geologic maps only (reprojection may be required)
shp_aqui = sf::read_sf(path_aqui |> paste0("aquifrp025.shp"))
### alluvial/glacial aquifers


xlsx_ecor = list.files(path = path_ecor, pattern = "*.xlsx$", full.names = TRUE)
xlsx_ecor08 = grep("20200506", xlsx_ecor, value = TRUE)
xlsx_ecor12 = grep("20230425", xlsx_ecor, value = TRUE)
xlsx_ecor08 = xlsx_ecor08[-length(xlsx_ecor08)]
xlsx_ecor12 = xlsx_ecor12[-length(xlsx_ecor12)]

tbl_ecor08 = lapply(xlsx_ecor08, \(x) readxl::read_excel(x, sheet = 4))
tbl_ecor12 = lapply(xlsx_ecor12, \(x) readxl::read_excel(x, sheet = 4))

# TODO: ecor tables should be prefiltered with column names that we need to consider
tbl_ecor08 = do.call(dplyr::bind_rows, tbl_ecor08)
tbl_ecor12 = do.call(dplyr::bind_rows, tbl_ecor12)


## geol
# lyrs_sgmc = sf::st_layers(path_geol)
# target is SGMC_Geology
shp_sgmc = sf::read_sf(path_geol, layer = "SGMC_Geology")
# shp_sgmc %>% st_transform("EPSG:5070") %>% write_sf(path_base %>% paste0("input/Geology/SGMC_Geology.gpkg"))



## prism
path_prism = paste0(path_base, "input/PRISM/")
# bils_prism = list.files(path_prism, pattern = "*.bil$", recursive = TRUE, full.names = TRUE)
# dirs_prism = list.dirs(path = path_prism, full.names = FALSE, recursive = FALSE)
# dirs_prism_string = strsplit(dirs_prism, split = "_") %>%
#     lapply(function(x) x[2]) %>%
#     Reduce(c, .)
# dirs_prism_string[10] = "us_dem"
# rast_prism = lapply(bils_prism, terra::rast) %>%
#     Reduce(f = c, x = .)
# names(rast_prism) = dirs_prism_string
# terra::writeCDF(rast_prism, "~/Documents/PRISM_4km_combined.nc4")

# hard-coded layer names for direct CDF reading
dirs_prism_string = 
    c("ppt", "solclear", "solslope", "soltotal", "soltrans", "tdmean",
     "tmax", "tmean", "tmin", "us_dem", "vpdmax", "vpdmin")
## ... edit dirs_prism if necessary....
rast_prism = terra::rast(paste0(path_prism, "PRISM_4km_combined.nc"))
names(rast_prism) = dirs_prism_string
rast_prism
## wbd
ext_mainland = c(xmin = -125, xmax = -62, ymin = 24, ymax = 61)
ext_mainland = terra::ext(ext_mainland)

# huc08 = terra::vect(paste0(path_base, "WBD_National/WBD_National.gdb"))
huc08 = terra::vect("~/Downloads/WBD_National_GPKG/WBD_National_GPKG.gpkg", layer = "WBDHU8")
huc08 = huc08[ext_mainland,]
# huc08_extr_test = terra::extract(rast_prism, huc08, exact = TRUE, weights = TRUE, fun = mean)
huc08_extr_test = terra::zonal(rast_prism, huc08, exact = TRUE, weights = TRUE, fun = 'mean', na.rm = TRUE)


huc10 = terra::vect("~/Downloads/WBD_National_GPKG/WBD_National_GPKG.gpkg", layer = "WBDHU10")
huc10 = huc10[ext_mainland,]
huc10_extr_test = terra::zonal(rast_prism, huc10, exact = TRUE, weights = TRUE, fun = 'mean', na.rm = TRUE)


huc12 = terra::vect("~/Downloads/WBD_National_GPKG/WBD_National_GPKG.gpkg", layer = "WBDHU12")
huc12 = huc12[ext_mainland,]
huc12_extr_test = terra::zonal(rast_prism, huc12, exact = TRUE, weights = TRUE, fun = 'mean', na.rm = TRUE)
