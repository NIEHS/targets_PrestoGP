# Created by use_targets().
# Main targets file for the project.
# Created by Kyle P Messier

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(geotargets)
library(PrestoGP)
library(tibble)
library(sf)
library(terra)
library(qs)
library(tidyverse)
library(skimr)
library(rsample)
library(stats)
library(parsnip)
library(fastDummies)
library(scales)
library(ggridges)
library(broom)
library(data.table)
library(exactextractr)
library(crew)
library(crew.cluster)
library(chopin)
library(dataRetrieval)


sf::sf_use_s2(FALSE)
terra::terraOptions(memfrac = 0.1)


tar_config_set(
  store = "/ddn/gs1/group/set/pipeline/PrestoGP_Pesticides"
)


crew_default <-
  crew_controller_local(
    name = "controller_default",
    tls = crew::crew_tls(mode = "automatic"),
    launch_max = 10L,
    workers = 12L
  )



controller_geo1 <- crew::crew_controller_local(
  name = "controller_geo1",
  workers = 4,
  launch_max = 10L
)
controller_geo2 <- crew::crew_controller_local(
  name = "controller_geo2",
  workers = 3L,
  launch_max = 5L
)

tar_option_set(
  packages = c(
    "PrestoGP", "tibble", "sf", "terra", "qs", "tidyverse", "skimr",
    "rsample", "stats", "ggplot2", "geotargets", "tarchetypes",
    "parsnip", "fastDummies", "stringr","chopin",
    "scales", "ggridges", "spatialsample", "broom", "yardstick", "data.table",
    "exactextractr", "dataRetrieval", "lubridate", "dplyr", "chopin"
  ),
  format = "qs",
  controller = crew_controller_group(crew_default, controller_geo1, controller_geo2),
  resources = tar_resources(
    crew = tar_resources_crew(
      controller = "controller_default"
    )
  ),
  garbage_collection = TRUE
  #error = "abridge"
)


# Run the R scripts in the R/ folder with your custom functions:
tar_source(
  sprintf("./%s", c(
  "code/01_Create_Dataset/Target_Pesticide_Data.R",
  "code/01_Create_Dataset/check_characteristics.R",
  "code/02_Geographic_Covariates/Calc_OLM.R",
  "code/02_Geographic_Covariates/Calc_terraClimate.R",
  "code/02_Geographic_Covariates/Calc_TWI.R",
  "code/03_Pesticide_Analysis/Target_Helpers.R"  
)))

# data_AZO_covariates_cleaned_03032024
#  The TARGET LIST
list(
  tar_target( # This target is the WBD database
    name = wbd_data,
    command = "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/WBD-National/WBD_National_GDB.gdb",
    format = "file"
  ),

  tar_target(
    state_list,
    state.abb[c(-2, -11)], # Remove Alaska and Hawaii
  ),
  tar_target(
    wqp_params_yml,
    'param_names.yml',
    format = "file"
  ),
  
  # Load yml file containing common parameter groups and WQP characteristic names
  tar_target(
    wqp_params,
    yaml::read_yaml(wqp_params_yml) 
  ),
  
  # Format a table that indicates how various WQP characteristic names map onto 
  # more commonly-used parameter names
  tar_target(
    char_names_crosswalk,
    crosswalk_characteristics(wqp_params)
  ),
  
  # Get a vector of WQP characteristic names to match parameter groups of interest
  tar_target(
    pest_char_names,
    filter_characteristics(char_names_crosswalk)
  ),
    
    tar_target( # This target creates the state-level Pesticide data from NWIS
      name = state_pesticide,
      command = get_pesticide_data(state_list, pest_char_names),
      pattern = map(state_list)
    ),
    
  tar_target( # This target combines the state-level data into one dataset
    name = state_pesticide_combined,
    command = combine_state_data(state_pesticide)
  ),
  tar_target( # This target gets the censoring aspects of the data
    name = pesticide_censored,
    command = get_censored_data(state_pesticide_combined)
  ),
  tar_target( # This target gets the daily averages
    name = pesticide_daily,
    command = get_daily_averages(pesticide_censored)
  ),
  tar_target( # This target gets the yearly averages
    name = pesticide_yearly,
    command = get_yearly_averages(pesticide_daily)
  ),
  tar_target( # filter concentrations with vals <= 0
    name = pesticide_yearly_filtered,
    command = filter(pesticide_yearly, concentration > 0)
  ),
  tar_target( # This target re-projects the data into an sf object
    name = sf_pesticide,
    # use st_sf to create an sf object with the Albers Equal Area projection
    command = st_as_sf(pesticide_yearly_filtered, coords = c("Longitude", "Latitude"), crs = 4326)
  ),
  tar_target( # This target joins the point pesticide data with HUC data
    name = sf_pesticide_huc,
    command = join_pesticide_huc(sf_pesticide, wbd_data)
  ),
  tar_target(
    olm_names,
    command = c("Bulk_Density", "pH", "Clay_Content", "Organic_Carbon", "Sand_Content", "Soil_Order", "USDA_Texture_Class"),
    iteration = "vector"
  ),
  tar_target(
    name = olm_layer_files,
    command = list.files(
      sprintf(
        "/pipeline/input/OpenLandMapData/%s", olm_names
      ),
      pattern = "*.tif",
      full.names = TRUE
    ),
    pattern = map(olm_names),
    iteration = "vector"
  ),
  tar_terra_rast(
    name = olm_layer_rast,
    command = terra::rast(olm_layer_files, win = c(-124.7844079, -66.9513812, 24.7433195, 49.3457868)),
    pattern = map(olm_layer_files),
    iteration = "list"
  ),
  tar_target( # Create a small 500m buffer around the pesticide data
    name = sf_pesticide_buffer,
    command = sf::st_buffer(sf_pesticide_huc, dist = 500)
  ),
  # tar_target(
  #   random_points,
  #   command = sf_pesticide_buffer %>% sample_n(100, replace = FALSE)
  # ),
  tar_target( # Calculate OLM small buffer variables
    name = olm_buffer,
    command = calc_olm_point(sf_pesticide_buffer, olm_layer_rast),
    pattern = map(olm_layer_rast),
    resources = tar_resources(
      crew = tar_resources_crew(controller = "controller_geo1")
    )
  ),
  tar_target( # Calculate OLM HUC12 variables
    name = olm_huc12,
    command = calc_olm_huc(sf_pesticide_huc, olm_layer_rast, wbd_data, "huc12"),
    pattern = map(olm_layer_rast),
    resources = tar_resources(
      crew = tar_resources_crew(controller = "controller_geo1")
    )
  ),
  tar_target( # Calculate OLM HUC10 variables
    name = olm_huc10,
    command = calc_olm_huc(sf_pesticide_huc, olm_layer_rast, wbd_data, "huc10"),
    pattern = map(olm_layer_rast)
  ),
  tar_target( # Calculate OLM HUC08 variables
    name = olm_huc08,
    command = calc_olm_huc(sf_pesticide_huc, olm_layer_rast, wbd_data, "huc8"),
    pattern = map(olm_layer_rast)
  ),
  tar_target(
    terra_climate_names,
    command = sort(c(
      "aet", "def", "PDSI", "pet", "ppt", "q", "soil", "srad",
      "swe", "tmax", "tmin", "vap", "vpd", "ws"
    )),
    iteration = "list"
  ),
  tar_target(
    name = terra_climate_layer_files,
    command = list.files(
      sprintf(
        "/pipeline/input/terraClimate/NetCDF/%s", terra_climate_names
      ),
      pattern = "*.nc",
      full.names = TRUE
    ),
    iteration = "list",
    pattern = map(terra_climate_names)
  ),
  tar_terra_rast( # This target calculates the mean of the climate data and combines them all together by year
    name = terra_climate_mean,
    command = terra::rast(terra_climate_layer_files, win = c(-124.7844079, -66.9513812, 24.7433195, 49.3457868)),
    pattern = map(terra_climate_layer_files),
    iteration = "list"
  ),
  tar_terra_rast( # This target calculates the yearly mean of the climate data by applying over
    # each monthly value (i.e. the groups of 12)
    name = terra_climate_yearly,
    command = terra::tapp(terra_climate_mean, index = rep(1:23, each = 12), fun = mean, na.rm = TRUE),
    pattern = map(terra_climate_mean),
    iteration = "list"
  ),
  tar_target(
    name = terra_climate_buffer,
    command = calc_tc_point(sf_pesticide_buffer, terra_climate_yearly, terra_climate_names),
    pattern = map(terra_climate_yearly, terra_climate_names),
    iteration = "list"
  ),
  tar_target( # Calculate terraClimate HUC08 variables
    name = tc_huc08,
    command = calc_tc_huc(sf_pesticide_huc, terra_climate_yearly, wbd_data, "huc8", terra_climate_names),
    pattern = map(terra_climate_yearly, terra_climate_names),
    iteration = "list"
  ),
  tar_target( # Calculate terraClimate HUC10 variables
    name = tc_huc10,
    command = calc_tc_huc(sf_pesticide_huc, terra_climate_yearly, wbd_data, "huc10", terra_climate_names),
    pattern = map(terra_climate_yearly, terra_climate_names),
    iteration = "list"
  ),
  tar_target( # Calculate terraClimate HUC12 variables
    name = tc_huc12,
    command = calc_tc_huc(sf_pesticide_huc, terra_climate_yearly, wbd_data, "huc12", terra_climate_names),
    pattern = map(terra_climate_yearly, terra_climate_names),
    iteration = "list"
  ),
  tar_target(
    name = twi_path,
    command = list.files("/pipeline/input/TWI/", full.names = T, pattern = "^CONUS*.*.tif"),
    resources = tar_resources(
      crew = tar_resources_crew(controller = "controller_geo2")
    )
    # format = "file"
  ),

  # tar_terra_rast( # Geotarget for TWI raster, static spatial only
  #   name = twi_layer_rast,
  #   command = terra::rast(twi_path),
  #   iteration = "list"
  # ),
  tar_target(
    name = huc_levels,
    command = c(8, 10, 12),
    iteration = "vector",
    resources = tar_resources(
      crew = tar_resources_crew(controller = "controller_geo1")
    )
  ),
  tar_target(
    name = nass_files,
    command = list.files("/pipeline/input/USDA_NASS", "*.tif$", full.names = TRUE)
  ),
  tar_target(
    name = huc_nass,
    command = calc_nass(
      base_path = "/pipeline/",
      nass_file = nass_files,
      wbd_path = "input/WBD-National/WBD_National_GDB.gdb",
      huc_level = huc_levels
    ),
    # since the study period is 2008-2022
    pattern = cross(nass_files, huc_levels),
    iteration = "list",
    resources = tar_resources(
      crew = tar_resources_crew(controller = "controller_default")
    )
  ),
  tar_target(
    name = huc_twi,
    command =
      calc_twi(
        twi_file = twi_path,
        wbd_path = "/pipeline/input/WBD-National/WBD_National_GDB_reexport.gpkg", huc_level = huc_levels),
    resources = tar_resources(
      crew = tar_resources_crew(controller = "controller_geo2")
    ),
    iteration = "list",
    pattern = map(huc_levels)
  )
)

# tar_target( # This target runs skimr::skim to look at the summary stats
#   name = explore_skim_outcomes,
#   command = skim(sf_pesticide)
# ),
# tar_target( # This target runs skimr::skim to look at the summary stats
#   name = explore_skim_covariates,
#   command = skim(sf_covariates)
# ),
# tar_target( # This target makes a ridge density plot of the outcomes
#   name = plot_outcome_ridgelines,
#   command = plot_pesticide_ridges(sf_pesticide_partition[[1]]) #List element 1 is the Pesticide data
# ),
# tar_target(# This target prepares the numeric and factor covariates for analysis
#   name = sf_pesticide_partition_cleaned,
#   command = covariate_prep(sf_pesticide_partition, 0.00001)
# ),
# tar_target( # This target pivots the covariates
#   name = sf_covariates_pivot,
#   command = pivot_covariates(sf_pesticide_partition_cleaned[[1]])
# ),
# list( # Dynamic branching with tar_group_by and plotting covariates
#   tar_group_by(
#     sf_explore_cov_maps,
#     sf_covariates_pivot,
#     covariate
#   ),
#   tar_target(
#     plot_covariate_maps,
#     plot_exploratory_covariates(sf_explore_cov_maps),
#     pattern = map(sf_explore_cov_maps),
#     iteration = "list"
#   )
# ),
# list( # Dynamic branching with tar_group_by and plotting maps of pesticide outcome
#   tar_group_by(
#     sf_plot_outcome_maps,
#     sf_pesticide_partition_cleaned[[1]],
#     ChmclNm
#   ),
#   tar_target(
#     plot_pesticide_maps,
#     plot_outcome_map(sf_plot_outcome_maps),
#     pattern = map(sf_plot_outcome_maps),
#     iteration = "list"
#   )
# ),
# tar_target( # This target creates 10-fold CV using Spatialsample
#   name = spatial_kfold,
#   command = spatial_block_cv(sf_pesticide_partition_cleaned[[1]])
# ),
# tar_target( # This target plots the CV folds
#   name = plot_spatial_kfolds,
#   command = autoplot(spatial_kfold)
# ),
# tar_target( # This target creates leave-one-year-out cross-validatoin
#   name = temporal_kfold,
#   command = group_vfold_cv(sf_pesticide_partition_cleaned[[1]], group = "Year")
# ),
# list( # Dynamic branching with tar_group_by and fitting lasso model to each pesticide group
#   tar_group_by(
#     sf_lasso_mvn,
#     sf_pesticide_for_fit,
#     kfolds
#   ),
#   tar_target(
#     lasso_fit_by_kfold,
#     fit_lasso(sf_lasso_mvn),
#     pattern = map(sf_lasso_mvn),
#     iteration = "list"
#   )
# ),
# tar_target(
#   kfolds_iter,
#   1:10
# ),
# tar_target(
#   get_spatial_kfolds,
#   get_rsplit(spatial_kfold, kfolds_iter),
#   pattern = map(kfolds_iter),
# ),
# tar_target(
#   lasso_fit_spatial_kfold,
#   lasso_spatial_kfold_fit(get_spatial_kfolds, as.formula(log(cncntrt) ~ . -id - ChmclNm - Year -lft_cns)),
#   pattern = map(get_spatial_kfolds),
#   iteration = "list"
# ),
# tar_target(# This target will create a small subsample of the data for testing the PrestoGP model
#   name = sub_sample_data,
#   command = dplyr::filter(sf_pesticide_partition_cleaned[[1]],Year >= 2003 & Year <= 2004)
#
# ),
# tar_target( # This target creates 10-fold CV using Spatialsample
#   name = sub_sp_kfold,
#   command = spatial_block_cv(sub_sample_data, v = 3)
# ),
# tar_target( # This target creates leave-one-year-out cross-validatoin
#   name = sub_time_kfold,
#   command = group_vfold_cv(sub_sample_data, group = "Year")
# ),
# tar_target(
#   kfolds_iter3,
#   1:3
# ),
# tar_target(
#   get_sub_sp_kfolds,
#   get_rsplit(sub_sp_kfold, kfolds_iter3),
#   pattern = map(kfolds_iter3),
# ),
# tar_target(
#   PrestoGP_sub_fit,
#   fit_MV_Vecchia(get_sub_sp_kfolds),
#   pattern = map(get_sub_sp_kfolds),
#   iteration = "list"
# )

# PRISM Data
# We have downloaded the 30-year normals for the PRISM data

# EPA EnviroAtlas data to add
# https://www.epa.gov/enviroatlas/data-download-step-2?token=bDeOAOosrqLOGojIi5gLwGwHA-atUJ5GZQ2o4ZhZNBQ
# by File Name 
# 1. Ag_On_Slopes
# 2. AgW_Demand (Agricultural Water Demand)
# 3. AgW_Supply (Agricultural Water Supply)
# 4. BNF4 (Biological NItrogen Fixation)
# 5. cbnf4 (Cultivated biological nitrogen fixation (kg N/ha/yr))
# 6. Estimated_floodplain_CONUS (Estimated Floodplain)
# 7. GroundwaterAndWaterBudgets (All 4 metrics)
# 8. N_P_H2O_Loss (Nitrogen and Phosphorus Losses to Water)
# 9.  RiparianCanopy (Riparian Canopy)

# Created by use_targets().
# 3. Run PrestoGP model on local machine for small test case
# 3a. Target for the model results 3b. Target for the model metrics
# 4. Run PrestoGP on local machine
# 5. Run PrestoGP on HPC-GEO
# 6. Compare with penalized Tobit regression for single variables (https://github.com/TateJacobson/tobitnet)
