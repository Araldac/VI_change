###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Climate data: download data from NAS
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
path_OB_NAS <- args[1]
output <- args[2] 

#download/install packages
#pkg <- c("ijtiff", "terra", "ijtiff", "raster", "dplyr", "tidyr", "ade4", "sp")
pkg<-c("raster" )
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################
#load data
print(path_OB_NAS)

#load(paste0(path_OB_NAS, "/Soil_Data/WorldclimPCA_25.RData")) # summary of princomp PCA

##Load the first 2 axis 
datafiles <- Sys.glob(paste0(path_OB_NAS,"/worldclim_pca_2_5/*.tif")) # download all .tif files
print(datafiles)
pca_world_2.5 <- stack()

for(i in 1:2){ ### just select firast 2 axis
  
  raster <- raster(datafiles[i])
  names(raster) <- paste0('PCA_', i)
  pca_world_2.5 <- stack(pca_world_2.5,raster)
  
}

crs(pca_world_2.5)
climate_data <- pca_world_2.5

save(climate_data, output)