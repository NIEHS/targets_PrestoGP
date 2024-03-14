# Created by use_targets().
# Main targets file for the project.
# Created by Kyle P Messier

# Load packages required to define the pipeline:
library(targets)
library(crew.cluster)

# Set target options:
tar_option_set(
  packages = c("PrestoGP", "dplyr", "sf", "terra", "exactextractr",
               "data.table", "fst", "crew", "crew.cluster"),
  # packages that your targets need to run
  format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  #
  # controller = crew.cluster::crew_controller_slurm(
  #   workers = 48L,
  #   slurm_partition = "geo",
  #   garbage_collection = TRUE,
  #   slurm_memory_gigabytes_per_cpu = 12L,
  #   slurm_cpus_per_task = 8L
  # )
  # Many clusters install R as an environment module, and you can load it
  # with the script_lines argument. To select a specific verison of R,
  # you may need to include a version string, e.g. "module load R/4.3.0".
  # Check with your system administrator if you are unsure.
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# options(clustermq.scheduler = "multicore")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("code/00_Load_Packages/Load_Packages.r")
tar_source("code/03_Pesticide_Analysis/Curate_Data.R")


# Replace the target list below with your own:
list(
  tar_target(
    name = data,
    command = readin()
  ),
  tar_target(
    name = data_sub,
    command = subsetting(data)
  ),
  tar_target(
    data_list,
    preppresto(data_sub[1:20000, ])
  ),
  # tar_target(
  #   prestomodel,
  #   PrestoGP::VecchiaModel()
  # ),
  tar_target(
    prestomodelfit,
    init_presto(
      modeltype = PrestoGP::VecchiaModel(),
      dat = data_list
    )
  ),
  tar_target(
    name = presto_simazine,
    command = summary(prestomodelfit)
  ),
  tar_target(
    name = summary_exported,
    command = writeLines(presto_simazine, file.path(path_base, "output/Model_Summary/Model_Summary.txt"))
  )
)
