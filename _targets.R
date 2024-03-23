# Created by use_targets().
# Main targets file for the project.
# Created by Kyle P Messier

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("PrestoGP","tibble","sf","terra","qs","tidyverse","skimr"),
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
  tar_target(
    name = read_pesticide,
    command = read_pesticide_data(COMPUTE_MODE = 1)
  ),
  tar_target( # This target runs skimr::skim to look at the summary stats of COVARIATES
    name = explore_skim, # Covariates start at column 41
    command = skim(filterNA_Covariates[,41:ncol(filterNA_Covariates)])
  ),
  tar_target(# This target calculates the percent of unique values in each of the the COVARIATES
    name = explore_unique,
    command = unique_vals(filterNA_Covariates)
  ),
  tar_target(
    name = drop_cols,
    command = drop_bad_cols(filterNA_Covariates, explore_unique, 0.0001)
  )
)
# Created by use_targets().