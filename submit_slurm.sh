#!/bin/bash

#SBATCH --job-name=pesticides_pipeline
#SBATCH --output=pesticides_pipeline.log
#SBATCH --error=pesticides_pipeline.error
#SBATCH --mail-user=songi2@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16g
#SBATCH --partition=geo


# change PROJECT_DIR according to your system environment
# $HOME is user home
PROJECT_DIR=$HOME/projects/targets_PrestoGP

export PATH=$PATH:/ddn/gs1/tools/cuda11.8/bin
export LD_LIBRARY_PATH=/ddn/gs1/biotools/R/lib64/R/customlib:$LD_LIBRARY_PATH:/ddn/gs1/tools/cuda11.8/lib64
# here the second directory is explicitly set to a specific user directory.
# Please make sure that you have access to the directory.
# You may want to change the path into your user library path.
export R_LIBS_USER=/ddn/gs1/biotools/R/lib64/R/custompkg:/ddn/gs1/home/songi2/r-libs:$R_LIBS_USER

# module load R # Uncomment if R is an environment module.
Rscript $PROJECT_DIR/run.R
