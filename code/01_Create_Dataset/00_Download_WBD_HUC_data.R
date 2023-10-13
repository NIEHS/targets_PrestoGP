# Simple script
# Download the WBD National File

library(beepr)
library(nhdplusTools)

outdir <- "input/WBD-National/"

download_wbd(
  outdir,
  url = paste0(
    "https://prd-tnm.s3.amazonaws.com/StagedProducts/",
    "Hydrography/WBD/National/GDB/WBD_National_GDB.zip"
  ),
  progress = TRUE
)
beep(sound = 2)
