#!/bin/bash

#SBATCH --job-name=huc10_nass
#SBATCH --error=huc10_calc_nass.error
#SBATCH --mail-user=songi2@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=15
#SBATCH --mem=450g
#SBATCH --partition=highmem

## apptainer
# apptainer exec --writable-tmpfs --mount='type=bind,source=/opt,destination=/ddn/gs1/home/songi2/projects/PrestoGP_Pesticides/code' /ddn/gs1/home/songi2/images/rocker_base_computation.sif Rscript /ddn/gs1/home/songi2/projects/PrestoGP_Pesticides/Calc_WBD.r

apptainer exec \
    --writable-tmpfs \
    --mount type=bind,source=/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input,destination=/opt \
    --mount type=bind,source=/ddn/gs1/home/songi2/projects/PrestoGP_Pesticides/output,destination=/mnt \
    /ddn/gs1/home/songi2/images/rocker_base_computation2.sif Rscript /ddn/gs1/home/songi2/projects/PrestoGP_Pesticides/code/02_Geographic_Covariates/HUC10_extract_test.r
