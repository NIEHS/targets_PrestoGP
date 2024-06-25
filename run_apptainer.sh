#!/bin/bash

#SBATCH --job-name=pesticides_pipeline_appt
#SBATCH --output=pesticides_pipeline_appt.log
#SBATCH --error=pesticides_pipeline_appt.error
#SBATCH --mail-user=songi2@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem=768g
#SBATCH --partition=geo

# assign assets properly -- double check SBATCH command
apptainer exec \
  # raw data path binding
  --mount type=bind,src=/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input,dst=/pipeline/input \
  # _targets object path binding
  --mount type=bind,src=/ddn/gs1/group/set/pipeline/Pesticides,dst=/opt/_targets \
  # main code path binding
  --mount type=bind,src=/ddn/gs1/home/songi2/projects/targets_PrestoGP,dst=/mnt \
  # disabling default path binding
  --no-mount bind-paths \
  # resource allocation
  --memory 768G \
  # max memory if there is contention
  --memory-reservation 640G \
  # max threads used
  --cpus 64 \
  # apptainer image
  pipeline_image.sif \
  # command
  Rscript /mnt/tar_run.R # use the host path?
