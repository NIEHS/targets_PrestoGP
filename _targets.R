# Created by use_targets().
# Main targets file for the project.
# Created by Kyle P Messier

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("PrestoGP", "dplyr", "sf", "terra") # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
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
tar_source("code/00_Load_Packages/Load_Packages.r") # Source other scripts as needed.
tar_source("code/03_Pesticide_Analysis/Curate_Data.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = data,
    command = fst::read_fst(file.path(path_base, "output/Covariates_Calculated/data_AZO_covariates_zerofill.fst"))
  ),
  tar_target(
    name = data_sub,
    command = subsetting(data)
  ),
  tar_target(
    data_list,
    preppresto(data_sub[1:2000, ])
  ),
  tar_target(
    prestomodel,
    PrestoGP::VecchiaModel()
  ),
  tar_target(
    prestomodelfit,
    PrestoGP::prestogp_fit(
      model = prestomodel,
      locs = data_list$locs,
      Y = data_list$Y,
      X = data_list$X,
      parallel = FALSE
    )
  ),
  tar_target(
    name = presto_simazine,
    command = summary(prestomodelfit)
  )
)
