# Created by use_targets().
# Main targets file for the project.
# Created by Kyle P Messier
# options(
#   clustermq.scheduler = "local",
#   clustermq.template = "code/02A_SLURM_submission/template_slurm.tmpl"
# )
Sys.setenv(
  "LD_LIBRARY_PATH" =
    paste0(
      "/ddn/gs1/biotools/R/lib64/R/customlib:",
      Sys.getenv("LD_LIBRARY_PATH")
    )
)

libpaths_in <-
  c(
    "/ddn/gs1/biotools/R/lib64/R/custompkg",
    "/ddn/gs1/home/songi2/r-libs",
    .libPaths()
  )

.libPaths(libpaths_in)
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
# library(clustermq)
library(crew.cluster)
library(chopin)


sf::sf_use_s2(FALSE)


tar_config_set(
  store = "/ddn/gs1/group/set/pipeline/Pesticides"
)

# Set target options:
sbatch_add_lines <-
  c("#SBATCH --mail-user=isong@nih.gov",
    "#SBATCH --mail-type=END,FAIL",
    "export LD_LIBRARY_PATH=/ddn/gs1/biotools/R/lib64/R/customlib:$LD_LIBRARY_PATH"
  )

crew_default <-
  crew.cluster::crew_controller_slurm(
    name = "controller_default",
    tls = crew::crew_tls(mode = "automatic"),
    launch_max = 10L,
    host = Sys.info()["nodename"],
    slurm_partition = "geo",
    slurm_log_output = "output/crew_log_slurm_default.log",
    slurm_log_error = "output/crew_error_slurm_default.error",
    script_lines = sbatch_add_lines,
    slurm_memory_gigabytes_per_cpu = 4,
    slurm_cpus_per_task = 1,
    workers = 12L
  )


# mq_twi <-
#   targets::tar_resources(
#     clustermq =
#     targets::tar_resources_clustermq(
#       template =
#       list(
#         memory = 8,
#         email = "messierkp@nih.gov",
#         log_file = "output/clustermq_log.log",
#         error_file = "output/clustermq_error.error",
#         partition = "geo",
#         cores = 10
#       )
#     )
#   )
# mq_nass <-
#   targets::tar_resources(
#     clustermq =
#     targets::tar_resources_clustermq(
#       template =
#       list(
#         memory = 8,
#         email = "messierkp@nih.gov",
#         log_file = "output/clustermq_log.log",
#         error_file = "output/clustermq_error.error",
#         partition = "geo",
#         cores = 15L
#       )
#     )
#   )

# crew_default <-
#   crew.cluster::crew_controller_slurm(
#     name = "controller_default",
#     workers = 12L,
#     seconds_idle = 10,
#     launch_max = 5L,
#     slurm_partition = "geo",
#     slurm_log_output = "output/crew_log.log",
#     slurm_log_error = "output/crew_error.error",
#     slurm_memory_gigabytes_per_cpu = 8,
#     slurm_cpus_per_task = 1
#   )

controller_geo1 <- crew.cluster::crew_controller_slurm(
  name = "controller_geo1",
  workers = 4,
  # seconds_idle = 15,
  # seconds_timeout = 86400,
  # seconds_launch= 7200,
  launch_max = 10L,
  slurm_partition = "geo",
  slurm_log_output = "output/crew_log_slurm1.log",
  slurm_log_error = "output/crew_error_slurm1.error",
  script_lines = sbatch_add_lines,
  slurm_memory_gigabytes_per_cpu = 8,
  slurm_cpus_per_task = 15
)
controller_geo2 <- crew.cluster::crew_controller_slurm(
  name = "controller_geo2",
  workers = 3L,
  # seconds_timeout = 7200,
  # seconds_launch= 7200,
  launch_max = 5L,
  slurm_partition = "geo",
  slurm_log_output = "output/crew_log_slurm2.log",
  slurm_log_error = "output/crew_error_slurm2.error",
  script_lines = sbatch_add_lines,
  slurm_memory_gigabytes_per_cpu = 64,
  slurm_cpus_per_task = 1
)

tar_option_set(
  packages = c(
    "PrestoGP", "tibble", "sf", "terra", "qs", "tidyverse", "skimr",
    "rsample", "stats", "ggplot2", "geotargets", "tarchetypes", "parsnip", "fastDummies",
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
  garbage_collection = TRUE,
  error = "stop",
  library = libpaths_in,
  # debug = "olm_huc12_9dae2790e8379df8",
  # cue = tar_cue(mode = "never")
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  # controller = crew::crew_controller_local(workers = 8)
  #
  #
  # add the slurm username to the crew controller
  #  controller = crew.cluster::crew_controller_slurm(
  #    name = "pipeline_kpm",
  #    workers = 12,
  #    slurm_log_output="/slurm_messages/pipeline_kpm.out",
  #    slurm_log_error="/slurm_messages/pipeline_kpm.err",
  # script_lines = "module load R",
  #    slurm_partition = "triton"
  #  )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# options(clustermq.scheduler = "multicore")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(c(
  "code/03_Pesticide_Analysis/Target_Helpers.R",
  "code/01_Create_Dataset/Target_Pesticide_Data.R",
  "code/02_Geographic_Covariates/Calc_OLM.R",
  "code/02_Geographic_Covariates/Calc_terraClimate.R"
))

# data_AZO_covariates_cleaned_03032024
#  The TARGET LIST
list(
  tar_target( # This target is the WBD database
    name = wbd_data,
    command = "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/WBD-National/WBD_National_GDB.gdb",
    format = "file"
  ),
  list( # Dynamic branch of the states for pesticide data from NWIS
    tar_target(
      state_list,
      state.abb[c(-2, -11)], # Remove Alaska and Hawaii
    ),
    tar_target( # This target creates the state-level Pesticide data from NWIS
      name = state_pesticide,
      command = state_fun_AZO(state_list),
      pattern = map(state_list)
    )
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
        "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/OpenLandMapData/%s", olm_names
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
        "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/terraClimate/NetCDF/%s", terra_climate_names
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
    command = list.files("/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/TWI/", full.names = T, pattern = "*.tif"),
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
    command = list.files("/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/USDA_NASS", "*.tif$", full.names = TRUE)
  ),
  tar_target(
    name = huc_nass,
    command = calc_nass(
      base_path = "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/",
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
  # tar_target(
  #   name = twi_file,
  #   command = "../../../../input/TWI/CONUS_TWI_epsg5072_30m_unmasked.tif",
  #   format = "file"
  # ),
  tar_target(
    name = huc_twi,
    command = calc_twi(twi_file = twi_path, wbd_path = wbd_data, huc_level = huc_levels),
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

# Created by use_targets().
# 3. Run PrestoGP model on local machine for small test case
# 3a. Target for the model results 3b. Target for the model metrics
# 4. Run PrestoGP on local machine
# 5. Run PrestoGP on HPC-GEO
# 6. Compare with penalized Tobit regression for single variables (https://github.com/TateJacobson/tobitnet)
