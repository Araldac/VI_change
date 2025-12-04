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
pkg<-c("raster", "terra" )
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################
#load data

#load(paste0(path_OB_NAS, "/Soil_Data/WorldclimPCA_25.RData")) # summary of princomp PCA

##Load the first 2 axis 
#datafiles <- Sys.glob(paste0(path_OB_NAS,"*.tif")) # download all .tif files
#print(datafiles)
#pca_world_2.5 <- stack()

#for(i in 1:2){ ### just select firast 2 axis
  
#  raster <- raster(datafiles[i])
#  names(raster) <- paste0('PCA_', i)
#  pca_world_2.5 <- stack(pca_world_2.5,raster)
  
#}

# Create the raster stack
r_a1 <- terra::rast(ax1)
r_a2 <- terra::rast(ax2)

pca_world_2.5 <- c(r_a1, r_a2)
names(pca_world_2.5) <- c("PCA_1", "PCA_2")

# Wrap directly (this handles memory internally)
climate_data2 <- terra::wrap(pca_world_2.5)

# Save
save(climate_data2, file = output)