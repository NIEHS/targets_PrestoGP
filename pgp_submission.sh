#!/bin/bash

#SBATCH --job-name=pgp_kpm
#SBATCH --error=pgp_kpm_error.error
#SBATCH --mail-user=messierkp@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH --partition=geo

Rscript -e "targets::tar_make()"