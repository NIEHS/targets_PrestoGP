#!/bin/bash

#SBATCH --job-name=pgp_kpm
#SBATCH --output=/ddn/gs1/home/messierkp/group_prj_local/targets_PrestoGP/slurm_messages/crew_kpm.out  
#SBATCH --error=/ddn/gs1/home/messierkp/group_prj_local/targets_PrestoGP/slurm_messages/crew_kpm.error
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=8g
#SBATCH --partition=geo
#SBATCH --mail-user=messierkp@nih.gov


export PATH=$PATH:/ddn/gs1/tools/cuda11.8/bin
export LD_LIBRARY_PATH=/ddn/gs1/biotools/R/lib64/R/customlib:$LD_LIBRARY_PATH:/ddn/gs1/tools/cuda11.8/lib64
# The second part $HOME/libs should be changed accordingly
export R_LIBS_USER=/ddn/gs1/biotools/R/lib64/R/custompkg:$HOME/r-libs:$R_LIBS_USER

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.
Rscript /ddn/gs1/home/messierkp/group_prj_local/targets_PrestoGP/run.R