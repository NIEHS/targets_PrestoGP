#!/bin/bash

#SBATCH --job-name=pgp_targets_test
#SBATCH --error=pgp_targets_error.error
#SBATCH --mail-user=songi2@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH --partition=geo

Rscript -e "targets::tar_make()"