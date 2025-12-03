###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Climate data: download data from NAS
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
PCAR <- args[1]
output <- args[2] 

#download/install packages
#pkg <- c("ijtiff", "terra", "ijtiff", "raster", "dplyr", "tidyr", "ade4", "sp")
pkg<-c("raster" )
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################
#load data

load(PCAR) # summary of princomp PCA
climate_data@layers[[1]]

climate_data2 <- climate_data

save(climate_data2, file=output)