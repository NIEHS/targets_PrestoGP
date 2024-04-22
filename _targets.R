# Created by use_targets().
# Main targets file for the project.
# Created by Kyle P Messier

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(PrestoGP)
library(tibble)
library(sf)
library(terra)
library(qs)
library(tidyverse)
library(skimr)
library(rsample)
library(stats)
library(ggplot2)
library(parsnip)
library(fastDummies)
library(scales)
library(ggridges)
library(spatialsample)
library(broom)
library(yardstick)
library(data.table)
# Set target options:
tar_option_set(
  packages = c("PrestoGP","tibble","sf","terra","qs","tidyverse","skimr",
               "rsample","stats","ggplot2","tarchetypes","parsnip","fastDummies",
               "scales","ggridges","spatialsample","broom","yardstick","data.table",
               "nhdplusTools"),
  format = "qs",
  sf_use_s2(FALSE),
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #controller = crew::crew_controller_local(workers = 2)
  #
  # 
    controller = crew.cluster::crew_controller_slurm(
      workers = 12,
      # Many clusters install R as an environment module, and you can load it
      # with the script_lines argument. To select a specific verison of R,
      # you may need to include a version string, e.g. "module load R/4.3.0".
      # Check with your system administrator if you are unsure.
      script_lines = "module load R",
      slurm_partition = "geo"
    )
    # 
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
tar_source(c("code/03_Pesticide_Analysis/Target_Helpers.R",
             "code/01_Create_Dataset/Target_Pesticide_Data.R",
             "code/02_Geographic_Covariates/Calc_OLM.R")
)

# data_AZO_covariates_cleaned_03032024
#  The TARGET LIST
list( 
  tar_target(# This target downloads the WBD database (WARNING LARGE)
    name = wbd_data,
    command = download_wbd(outdir = "input/WBD-National/")
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
  tar_target(# filter concentrations with vals <= 0
    name = pesticide_yearly_filtered,
    command = filter(pesticide_yearly, concentration > 0) 
  ),
  tar_target( # This target re-projects the data into an sf object
    name = sf_pesticide,
    # use st_sf to create an sf object with the Albers Equal Area projection
    command = st_as_sf(pesticide_yearly_filtered, coords = c("Longitude","Latitude"), crs = 5070)
  ),  
  tar_target( # This target joins the point pesticide data with HUC data
    name = sf_pesticide_huc,
    command = join_pesticide_huc(sf_pesticide)
  ),
  tar_target( # This target separates the data into (1)outcome (2) covariates/features (3) ancillary info
    name = sf_pesticide_partition,
    command = partition_datasets(sf_pesticide)
  ),  
  tar_target( # This target runs skimr::skim to look at the summary stats 
    name = explore_skim, 
    command = skim(sf_pesticide_partition[[1]]) #List element 2 is the covariates
  ),
  tar_target( # This target makes a ridge density plot of the outcomes 
    name = plot_outcome_ridgelines, 
    command = plot_pesticide_ridges(sf_pesticide_partition[[1]]) #List element 1 is the Pesticide data
  ),  
  tar_target(# This target prepares the numeric and factor covariates for analysis
    name = sf_pesticide_partition_cleaned,
    command = covariate_prep(sf_pesticide_partition, 0.00001)
  ),
  tar_target( # This target pivots the covariates
    name = sf_covariates_pivot, 
    command = pivot_covariates(sf_pesticide_partition_cleaned[[1]])
  ),
  list( # Dynamic branching with tar_group_by and plotting covariates
    tar_group_by(
      sf_explore_cov_maps,
      sf_covariates_pivot,
      covariate
    ),
    tar_target(
      plot_covariate_maps,
      plot_exploratory_covariates(sf_explore_cov_maps),
      pattern = map(sf_explore_cov_maps),
      iteration = "list"
    )
  ),
  list( # Dynamic branching with tar_group_by and plotting maps of pesticide outcome
    tar_group_by(
      sf_plot_outcome_maps,
      sf_pesticide_partition_cleaned[[1]],
      ChmclNm
    ),
    tar_target(
      plot_pesticide_maps,
      plot_outcome_map(sf_plot_outcome_maps),
      pattern = map(sf_plot_outcome_maps),
      iteration = "list"
    )
  ),  
  tar_target( # This target creates 10-fold CV using Spatialsample
    name = spatial_kfold,
    command = spatial_block_cv(sf_pesticide_partition_cleaned[[1]])
  ),
  tar_target( # This target plots the CV folds 
    name = plot_spatial_kfolds,
    command = autoplot(spatial_kfold)
  ), 
  tar_target( # This target creates leave-one-year-out cross-validatoin
    name = temporal_kfold,
    command = group_vfold_cv(sf_pesticide_partition_cleaned[[1]], group = "Year")
  ),
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
  tar_target(
    kfolds_iter,
    1:10
  ),
  tar_target(
    get_spatial_kfolds,
    get_rsplit(spatial_kfold, kfolds_iter),
    pattern = map(kfolds_iter),
  ),
  tar_target(
    lasso_fit_spatial_kfold,
    lasso_spatial_kfold_fit(get_spatial_kfolds, as.formula(log(cncntrt) ~ . -id - ChmclNm - Year -lft_cns)),
    pattern = map(get_spatial_kfolds),
    iteration = "list"
  ),
  tar_target(# This target will create a small subsample of the data for testing the PrestoGP model
    name = sub_sample_data,
    command = dplyr::filter(sf_pesticide_partition_cleaned[[1]],Year >= 2003 & Year <= 2004)

  ),
  tar_target( # This target creates 10-fold CV using Spatialsample
    name = sub_sp_kfold,
    command = spatial_block_cv(sub_sample_data, v = 3)
  ),
  tar_target( # This target creates leave-one-year-out cross-validatoin
    name = sub_time_kfold,
    command = group_vfold_cv(sub_sample_data, group = "Year")
  ),
  tar_target(
    kfolds_iter3,
    1:3
  ),
  tar_target(
    get_sub_sp_kfolds,
    get_rsplit(sub_sp_kfold, kfolds_iter3),
    pattern = map(kfolds_iter3),
  ),
  tar_target(
    PrestoGP_sub_fit,
    fit_MV_Vecchia(get_sub_sp_kfolds),
    pattern = map(get_sub_sp_kfolds),
    iteration = "list"
  )  
)
# Created by use_targets().
# 3. Run PrestoGP model on local machine for small test case 
# 3a. Target for the model results 3b. Target for the model metrics
# 4. Run PrestoGP on local machine
# 5. Run PrestoGP on HPC-GEO 
# 6. Compare with penalized Tobit regression for single variables (https://github.com/TateJacobson/tobitnet)
