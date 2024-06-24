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


export PATH=$PATH:/ddn/gs1/tools/cuda11.8/bin
export LD_LIBRARY_PATH=/ddn/gs1/biotools/R/lib64/R/customlib:$LD_LIBRARY_PATH:/ddn/gs1/tools/cuda11.8/lib64
# The second part $HOME/libs should be changed accordingly
export R_LIBS_USER=/ddn/gs1/biotools/R/lib64/R/custompkg:$HOME/r-libs:$R_LIBS_USER

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.
Rscript /ddn/gs1/home/songi2/projects/targets_PrestoGP/run.R
