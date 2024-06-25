# target-PrestoGP A targets pipeline for demonstrating the use of PrestoGP

### General Steps

0. Prepare Apptainer image
  - Replace the export directory path in the command below. This directory should be set correctly in `run_apptainer.sh` as well.

```shell
. ./build_apptainer_image.sh /ddn/gs1/group/set/pipeline
```

  - The command above will build an Apptainer image named pipeline_image.sif with all required packages installed
  - To run the pipeline with the built image, run in SSH to NIEHS HPC
    - All paths are adjusted to point the internal paths in the Apptainer image
    - Progress is recorded in SLURM log file named `pesticides_pipeline_appt.log` in the working directory.

```shell
sbatch run_apptainer.sh
```

1. Merge Calculated Covariates
2. Exploratory Analysis of Covariates
  - Maps of Covariates
  - Histograms
  - Correlation Matrix
3. Merge Covariates 
4. Save covariates as `qs` or `fst` file 
5. Run `PrestoGP`
  - Subset Test Locally
  - `geo` cluster setup
  - Run `PrestoGP` on `geo` cluster
  - Spatiotemporal Cross-Validation (branches)
  - Model Selection
6. Save `PrestoGP` Results
7. Exploratory Analysis of `PrestoGP` Results
  - Maps of `PrestoGP` Results
  - Histograms
  - Correlation Matrix
8. Post-Processing of `PrestoGP` Results
  - Data Formats for sharing
  - Areal Summarization
  - Visualization

### Side Option
1. Make `PrestoGP` into a `parsnip` model

