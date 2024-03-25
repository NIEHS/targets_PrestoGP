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
# Set target options:
tar_option_set(
  packages = c("PrestoGP","tibble","sf","terra","qs","tidyverse","skimr",
               "rsample","stats","ggplot2","tarchetypes","parsnip","fastDummies"),
  format = "qs"
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
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
tar_source("code/03_Pesticide_Analysis/Target_Helpers.R")

#  The TARGET LIST
list(
  tar_target(
    name = set_path,
    command = set_local_data_path(COMPUTE_MODE = 1)
  ),
  tar_target(
    name = readQS,
    command = read_data(set_path,"data_AZO_covariates_cleaned_03032024")
  ),
  tar_target( # This target filters out the NA values in the COVARIATES
    name = filterNA_Covariates,
    command = filter_NA(readQS)
  ),
  tar_target( # This target runs skimr::skim to look at the summary stats of COVARIATES
    name = explore_skim, # Covariates start at column 41
    command = skim(filterNA_Covariates[,41:ncol(filterNA_Covariates)])
  ),
  tar_target(# This target calculates the percent of unique values in each of the the COVARIATES
    name = explore_unique,
    command = unique_vals(filterNA_Covariates)
  ),
  tar_target( # This target drops columns with less than 0.0001 unique values (i.e. keeps them all)
    name = drop_cols,
    command = drop_bad_cols(filterNA_Covariates, explore_unique, 0.0001)
  ),  
  tar_target( # This target creates k-1 indicator variables for each categorical variable
    name = create_dummies,
    command = create_dummy_vars(drop_cols)
  ),    
  tar_target( # This target re-projects the combined, filtered data into an sf object
    name = sf_pesticide,
    # use st_sf to create an sf object with the Albers Equal Area projection
    command = st_as_sf(create_dummies, coords = c("X","Y"), crs = 5070)
  ),
  tar_target( # This target extracts coordinates for CV input
    name = coords_mat,
    command = st_coordinates(sf_pesticide) 
  ),
  tar_target( # This target creates 10-fold CV using RSAMPLE
    name = kfold_cv,
    command = kmeans(coords_mat, centers = 10)$cluster
  ),
  tar_target( # This target joins the CV folds with the data
    name = sf_pesticide_cv,
    command = add_column(sf_pesticide, kfolds = as.factor(kfold_cv))
  ),
  tar_target( # This target plots the CV folds 
    name = plot_kfolds,
    command = plot_cv_map(sf_pesticide_cv)
  ),  
  list( # Dynamic branching with tar_group_by and plotting kfolds
    tar_group_by(
      sf_pesticide_cv_group,
      sf_pesticide_cv,
      kfolds
    ),
    tar_target(
      plot_by_kfold,
      plot_single_map(sf_pesticide_cv_group),
      pattern = map(sf_pesticide_cv_group),
      iteration = "list"
    )
  )
  # list( # Dynamic branching with tar_group_by and fitting lasso model to each pesticide group
  #   tar_group_by(
  #     sf_pesticide_individual,
  #     sf_pesticide,
  #     ChmclNm
  #   ),
  #   tar_target(
  #     lasso_fit_by_chem,
  #     fit_lasso(sf_pesticide_individual),
  #     pattern = map(sf_pesticide_individual),
  #     iteration = "list"
  #   )
  # )  
)
# Created by use_targets().
# 1. Create dummy variables before exploratory analysis 
# 2. drop the aquifer_ROCKNAME, keep other factors, including NA covariates (ie. unknown geology/aquifer)
# 3. Make exploratory analysis and drop_vars not hard coded (unique_vals uses 41 as covariate- make a target that defines covariates)
# Variables that need to be converted to indicator vars
# geology_unit_type, aquifer_AQ_NAME, aquifer_ROCK_NAME
# 3. Setup PrestoGP with LBLO Cross-Validation
# 4. Run analysis on local machine - assume all values are observed
# 5. Run analysis on HPC-GEO - assume all values are observed
