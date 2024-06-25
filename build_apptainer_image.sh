#!/bin/bash

# usage: build_apptainer_image.sh [full file path]
# where full file path ends with .sif, with full directory path to save the image
# after the image is built, group write/execution privileges are given
apptainer build $1/pipeline_image.sif pipeline_image.def
chmod 774 $1/pipeline_image.sif
