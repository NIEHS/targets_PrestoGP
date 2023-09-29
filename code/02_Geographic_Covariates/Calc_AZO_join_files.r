######## pacman initialization ########
library(pacman)
p_load(sf, terra, dplyr, purrr)
sf_use_s2(F)


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

# future::plan(sequential)

## AZO 
azo = sf::read_sf(paste0(path_base, "data_process/data_AZO_watershed_huc_join.shp"))
azo_s = azo %>%
    dplyr::select(site_no, Year) %>%
    unique %>%
    st_transform("EPSG:4326")



# abbrev: [aqui]fer, [geol]ogy, [ecor]egions
path_aqui = paste0(path_base, "Aquifers/aquifrp025/")
# path_geol = paste0(path_base, "input/Geology/USGS_StateGeologicMapCompilation_ver1.1.gdb")
path_geol = paste0(path_base, "Geology/SGMC_Geology.gpkg")
# path_wbd = paste0(path_base, "WBD-National/WBD_National_GPKG.gpkg")

path_output = "/mnt/"

## geol
# lyrs_sgmc = sf::st_layers(path_geol)
# target is SGMC_Geology
shp_sgmc = sf::read_sf(path_geol, layer = "SGMC_Geology")
shp_aqui = sf::read_sf(path_aqui |> paste0("aquifrp025.shp"))

## wbd
ext_mainland = c(xmin = -126, xmax = -72, ymin = 24, ymax = 51)
ext_mainland = terra::ext(ext_mainland) %>%
    st_bbox(crs = 4326) %>%
    st_as_sfc()
ext_mainland = ext_mainland %>%
    st_transform(st_crs(shp_sgmc))

## Main extraction ####
### Geology spatial join ####
## Key field is UNIT_NAME
shp_sgmc = shp_sgmc[,"UNIT_NAME"]


path_prism = paste0(path_base, "PRISM/")

# hard-coded layer names for direct CDF reading
dirs_prism_string = 
    c("ppt", "solclear", "solslope", "soltotal", "soltrans", "tdmean",
     "tmax", "tmean", "tmin", "us_dem", "vpdmax", "vpdmin")
## ... edit dirs_prism if necessary....
rast_prism = terra::rast(paste0(path_prism, "PRISM_4km_combined.nc"))
names(rast_prism) = dirs_prism_string
# rast_prism


azo_s_sgmc = st_transform(azo_s, st_crs(shp_sgmc))
azo_s_prism = terra::project(vect(azo_s), crs(rast_prism))
azo_s_aqui = st_transform(azo_s, shp_aqui)

# sgmc
ext_azo_sgmc = st_join(azo_s_sgmc, shp_sgmc)
ext_azo_aqui = st_join(azo_s_aqui, shp_aqui)
ext_azo_prism = terra::extract(rast_prism, azo_s_prism)

ext_azo_prism = azo_s %>%
    st_drop_geometry %>%
    cbind(ext_azo_prism)

write.csv(ext_azo_sgmc, sprintf("%sAZO_Geology.csv", path_output))
write.csv(ext_azo_prism, sprintf("%sAZO_PRISM.csv", path_output))
write.csv(ext_azo_aqui, sprintf("%sAZO_PrimaryAquifer.csv", path_output))

