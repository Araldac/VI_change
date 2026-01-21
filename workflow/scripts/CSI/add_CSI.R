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
mix_CSI_S1 <- args[2]
mix_CSI_S2 <- args[3]
output_dataset <- args[4]

#download/install packages
pkg <- c("terra", "dplyr", "tidyr", "tiff", "sf")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Function
################################################################################

extract_CSI <-function (data, raster){

data_CSI <- data

cat("Extracting variables for each coord ...\n")
envtcel<-terra::extract(raster, data)
cat("Variables extracted! \n")

data_CSI$CSI <- envtcel[,2]

return(st_drop_geometry(data_CSI))
}
################################################################################
#Code
################################################################################

#### #### load data #### ####
all_v <- read.csv(occurrences, sep = ",")
all_v<- all_v %>% dplyr::select(-c(PCA_1, PCA_2))

raster_s1 <- terra::rast(mix_CSI_S1)
raster_s2 <- terra::rast(mix_CSI_S2)

#setwd("//wsl.localhost/Ubuntu/")

#raster_s1 <- terra::unwrap(raster_s1)
#raster_s2 <- terra::unwrap(raster_s2)
#### #### Group evaluation CSIs  #### ####
#df_eval_s1 <- result_S1[[2]]
#df_eval_s2 <- result_S2[[2]]

#df_eval <- rbind(df_eval_s1,df_eval_s2)

#write.csv(df_eval, file = output_df_eval)

#### #### Apply CSI  #### ####
CSI_S1<-mean(raster_s1)
CSI_S2<-mean(raster_s2)

#transform data
all_v <- st_as_sf(all_v, coords=c("decimallongitude", "decimallatitude"))
csi_s1 <- extract_CSI(all_v[all_v$S == "S1", ], CSI_S1)
csi_s2 <- extract_CSI(all_v[all_v$S == "S2", ], CSI_S2)

final_csi <- rbind(csi_s1, csi_s2)

head(final_csi)

#setwd("//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/")
write.csv(final_csi, file = output_dataset)