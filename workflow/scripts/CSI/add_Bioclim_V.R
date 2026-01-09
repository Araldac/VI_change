###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Add Climate and PA 
# Date: 26/11/2025
###############################################################################

options(warn = 1)

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
path_WorldClim <- args[2]
v_selected <- args[3]
output <- args[4]


#download/install packages
pkg <- c("terra", "dplyr", "tidyr", "tiff", "sf")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################

#### #### load data #### ####
lthin <- read.csv(occurrences, sep = ",")

bioclim<-list.files(path_WorldClim, full.names = T)
bio<-terra::rast(bioclim)
names(bio)=sub("wc2.1_2.5m_","",names(bio))
cat("Names bio cleaned: ", names(bio), "\n")

vars <- strsplit(v_selected, ",")[[1]]
cat("Varables selected: ", vars, "\n")

#transform data
lthin <- st_as_sf(lthin, coords=c("decimallongitude", "decimallatitude"))

#extract cliamtic data for each P/PA
cat("Extracting variables for each coord ...\n")

envtcel<-terra::extract(bio, lthin)
envtcel_filtered <- envtcel[ ,vars]

cat("Variables extracted! \n")

envtcel_thin<-cbind(lthin, envtcel_filtered)

occ_Bioclim <- envtcel_thin %>% dplyr::select(-c(PCA_1, PCA_2))

write.csv(st_drop_geometry(occ_Bioclim), file= output)