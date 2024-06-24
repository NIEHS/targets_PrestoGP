#!/bin/bash

export LD_LIBRARY_PATH=/ddn/gs1/biotools/R/lib64/R/customlib:$LD_LIBRARY_PATH
export R_LIBS_USER="/ddn/gs1/biotools/R/lib64/R/custompkg:~/r-libs:${R_LIBS_USER}"
# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.
nohup nice -4 R CMD BATCH run.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
# rm -f .RData
