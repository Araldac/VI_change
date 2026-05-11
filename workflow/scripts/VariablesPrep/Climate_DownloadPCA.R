###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Climate data: download data from NAS
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
ax1 <- args[1]
ax2 <- args[2]
output <- args[3] 

#download/install packages
#pkg <- c("ijtiff", "terra", "ijtiff", "raster", "dplyr", "tidyr", "ade4", "sp")
pkg<-c( "terra" )
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################

# Create the raster stack
r_a1 <- terra::rast(ax1)
r_a2 <- terra::rast(ax2)

climate_data2 <- c(r_a1, r_a2)
names(climate_data2) <- c("PCA_1", "PCA_2")


# Save

terra::writeRaster(climate_data2, filename = output, filetype = "GTiff", overwrite = TRUE)