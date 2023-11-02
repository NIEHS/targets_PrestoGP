## Join all calculated covariates
## 10/13/2023
## Insang Song
pkgs <- c("tidytable", "rlang", "terra", "sf", "bit64")
sapply(pkgs, library, character.only = TRUE)
Sys.setenv(sf_use_s2 = FALSE)



## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 1
path_base <-
  ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/output/",
    ifelse(COMPUTE_MODE == 2,
      "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/output/",
      ifelse(COMPUTE_MODE == 3,
        "/opt/",
        ifelse(COMPUTE_MODE == 4,
          "/tmp/AZO/",
          stop("COMPUTE_MODE should be one of 1, 2, 3, or 4.\n")
        )
      )
    )
  )

`%s%` <- function(x, y) paste0(x, y)

### AZO
azo <- sf::read_sf("./input/data_process/data_AZO_watershed_huc_join.shp")
azo_s <- azo %>%
  dplyr::select(site_no, Year) %>%
  unique() %>%
  st_transform("EPSG:4326")



## NASS cleaning (HUCs and year)
# nass_tables <- {path_base %s% 
#   "/NASS/HUCS_8_10_12_NASS.rds"} %>%
#     readRDS(.)

nass_yearly <- 
    {path_base %s% "NASS/"} %>%
    list.files(path = ., pattern = "csv$", full.names = TRUE) %>%
    split(., rep(2008:2022, each = 3)) %>%
    lapply(function(x) {
        lapply(x, data.table::fread) %>%
        data.table::rbindlist(., use.names = TRUE, fill = TRUE)
    }) %>%
    mapply(function(x, y) {
        # x: data.table, y: year
        x %>% mutate(year = y)
    }, ., seq(2008, 2022) %>% split(.,.), SIMPLIFY = FALSE) %>%
    data.table::rbindlist(., use.names = TRUE, fill = TRUE)

nass_yearly_k <- copy(nass_yearly)
names(nass_yearly)
# columns with trailing whitespace (actually duplicated) fix
nass_yearly_k <- nass_yearly_k %>%
  mutate(Pears = ifelse(is.na(Pears) & !is.na(`Pears `), `Pears `, Pears),
         `Perennial Ice/Snow` = ifelse(is.na(`Perennial Ice/Snow`) & !is.na(`Perennial Ice/Snow `), `Perennial Ice/Snow `, `Perennial Ice/Snow`)) %>%
  tidytable::select(-layer, -zone, -`Perennial Ice/Snow `, -`Pears `)

nass_names <- names(nass_yearly_k)
nass_names <- gsub("/", "_", nass_names)
nass_names <- gsub("[[:punct:]]", "_", nass_names)
nass_names <- trimws(nass_names)
nass_names <- gsub("(Dbl Crop )", "dbl_", nass_names)
nass_names <- gsub(" ", "", nass_names)
nass_names <- tolower(nass_names)
hucyear_index <- grep("(huc|year)", nass_names)
(nass_names)

colnames(nass_yearly_k) <- nass_names

nass_yearly_clean <- nass_yearly_k %>%
    tidytable::select(huc, year, 2:38, 40:119, 121:ncol(.)) %>%
    tidytable::mutate(across(everything(), ~ifelse(is.na(.), 0L, .))) %>%
    # calculating mean cell counts
    tidytable::group_by(huc) %>%
    tidytable::summarize(across(corn:perennialice_snow, ~mean(., na.rm = TRUE))) %>%
    tidytable::ungroup()
    

nass_yearly_clean_val <- nass_yearly_clean[,-1] %>%
    as.matrix() %>%
    {./rowSums(.)}

nass_yearly_clean[, 2:128] <- data.table(nass_yearly_clean_val)
nass_yearly_clean <- nass_yearly_clean %>%
    mutate(huclevel = cut(nchar(as.character(huc)), c(6,8,10,12), labels = c("HUC08", "HUC10", "HUC12")))
nass_yearly_cleanl <- nass_yearly_clean %>%
    split(., unlist(.$huclevel))

nass_huc08 <- nass_yearly_cleanl$HUC08 %>%
  select(-huclevel)
#    mutate(huc = sprintf("%08d", as.integer(huc)))
nass_huc10 <- nass_yearly_cleanl$HUC10 %>%
  select(-huclevel)
#    mutate(huc = sprintf("%10d", as.integer(huc)))
nass_huc12 <- nass_yearly_cleanl$HUC12 %>%
  select(-huclevel)
#    mutate(huc = ifelse(nchar(as.character(huc)) == 11, paste0("0", as.character(huc)), as.character(huc)))

names(nass_huc08)[-1] <- paste0("nass_huc08_", names(nass_huc08)[-1])
names(nass_huc10)[-1] <- paste0("nass_huc10_", names(nass_huc10)[-1])
names(nass_huc12)[-1] <- paste0("nass_huc12_", names(nass_huc12)[-1])


### OLM (HUC)
olm_files <- list.files(
    path_base %s% "HUC_covariates/OLM/",
    "OLM.csv$",
    full.names = TRUE
)
olm_huc08 <- data.table::fread(olm_files[1])
olm_huc10 <- data.table::fread(olm_files[2])
olm_huc12 <- data.table::fread(olm_files[3])
names(olm_huc10)[1] <- "HUC10"


### point-based spatial join products (join by site_no and year)
## aquifer
aqui_file = path_base %s% "AZO_covariates/AZO_PrimaryAquifer.csv"
aqui <- data.table::fread(aqui_file)
names(aqui)[-1:-2] <- paste0("aquifer_", names(aqui)[-1:-2])
aqui <- aqui[,-c(4, 6)]

## soilchem 
schem_file <- path_base %s% "AZO_covariates/AZO_soilchemistry.csv"
schem <- data.table::fread(schem_file)
names(schem)[-1:-2] <- paste0("soilchem_", names(schem)[-1:-2])
schem_keep <- c(1,2,9:55,61:141)
schem <- schem %>% select(all_of(schem_keep))

## terraClimate (mean)
terra_mean_file <- path_base %s% "AZO_covariates/AZO_terraclimate_yearly_mean.csv"
terra_mean <- data.table::fread(terra_mean_file)
terra_mean <- terra_mean[,-1]
names(terra_mean)[1:(ncol(terra_mean)-2)] <- paste0("terraClimate_mean_", names(terra_mean)[1:(ncol(terra_mean)-2)])
# terra_mean_sel <- c("aet", "def", "pet", "ppt", "q")

## terraClimate (sum)
terra_sum_file <- path_base %s% "AZO_covariates/AZO_terraclimate_yearly_sum.csv"
terra_sum <- data.table::fread(terra_sum_file)
terra_sum <- terra_sum[,-1]
names(terra_sum)[1:(ncol(terra_sum)-2)] <- paste0("terraClimate_sum_", names(terra_sum)[1:(ncol(terra_sum)-2)])

## PRISM
prism_file <- path_base %s% "AZO_covariates/AZO_PRISM.csv"
prism <- data.table::fread(prism_file)
prism <- prism[,-3]
names(prism)[3:ncol(prism)] <- paste0("prism_", names(prism)[3:ncol(prism)])

## geol
geol_file <- path_base %s% "AZO_covariates/AZO_Geology.csv"
geol <- data.table::fread(geol_file)
names(geol)[3] <- "geology_unit_type"

## Pesticides based on toxicological/chemical similarity
## Top 20 (by the rank of 20-year total high estimates)

## quick fix ...
# azo <- sf::read_sf("./input/data_process/data_AZO_watershed_huc_join.shp")
# azo_s <- azo %>%
#   dplyr::select(ChmclNm, site_no, Year) %>%
#   # unique() %>%
#   st_transform("EPSG:5070")
# azo_covar <- readRDS("output/data_AZO_covariates.rds")
# azo_covar <- azo_covar %>%
#   dplyr::left_join(azo_pest, by = c("ChmclNm", "site_no", "Year"))
## quick fix end


cnty_pests_top20_clean <- readRDS("./output/County_pesticide_similarity_top20.rds") |>
  dplyr::select(-1:-2) %>%
  st_transform("EPSG:5070")
azo_pest <- st_join(azo_s, cnty_pests_top20_clean) %>%
  st_drop_geometry()




## Join all
azo
list_factors <- c("geology_unit_type", "aquifer_ROCK_NAME", "aquifer_AQ_NAME")

azo_covar <- azo %>%
    bind_cols(data.frame(st_coordinates(.))) %>%
    st_drop_geometry() %>%
    mutate(across(starts_with("huc"), ~bit64::as.integer64(.))) %>%
    left_join(nass_huc08, by = c("huc08" = "huc")) %>%
    left_join(nass_huc10, by = c("huc10" = "huc")) %>%
    left_join(nass_huc12, by = c("huc12" = "huc")) %>%
    left_join(olm_huc08, by = c("huc08" = "HUC08")) %>%
    left_join(olm_huc10, by = c("huc10" = "HUC10")) %>%
    left_join(olm_huc12, by = c("huc12" = "HUC12")) %>%
    left_join(aqui) %>%
    left_join(schem) %>%
    left_join(terra_mean, by = c("site_no" = "site_no", "Year" = "year")) %>%
    left_join(terra_sum, by = c("site_no" = "site_no", "Year" = "year")) %>%
    left_join(prism) %>%
    left_join(geol) %>%
    left_join(azo_pest) %>%
    mutate(across(all_of(list_factors), ~as.factor(.)))



saveRDS(azo_covar, "output/data_AZO_covariates.rds", compress = "xz")


# minimal metadata (for covariate specification)
# it does not include full description of
# NAQWA site attributes.
azo_covar_spec <- tribble(
    ~prefix,    ~description,
    "X|Y",    "coordinates (EPSG:5070)",
    "Year",   "survey year",
    "ChmclNm",  "chemical compound name",
    "cncntrt",  "concentration",
    "left_cns", "left censoring flag",
    "aquifer",  "Primary aquifer",
    "soilchem", "National geochemical survey",
    "huc08|huc10|huc12",  "HUC-08 -10 -12 level ecoregion indicators from EPA",
    "terraClimate_mean", "terraClimate annual mean from monthly data",
    "terraClimate_sum", "terraClimate annual sum from monthly data",
    "prism",    "PRISM 30-year climate normals (1991-2020) from Oregon State University",
    "geology",  "USGS State Geologic Map Compilation",
    "nass",     "USDA NASS Cropscape (2008-)",
    "olm",      "OpenLandMap",
    "pest",     "USGS NAWQA Pesticide county-level estimates (low and high)"
)
# write.csv(azo_covar_spec, "output/data_AZO_covariates_prefixes.csv", row.names = FALSE)

prefixes <- azo_covar_spec$prefix
colindex <- grep(paste("^(", paste(prefixes, collapse = "|"), ")", sep = ""), names(azo_covar))

# get the covariate matrix from the data.table
azo_covar_mat <- azo_covar %>%
    select(all_of(colindex)) %>%
    as.matrix()
# saveRDS(azo_covar_mat, file = "output/data_AZO_design_matrix.rds")
