
###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: HM Data: load the HM per pixel
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
hm1970 <- args[2]
hm2025 <- args[3]
output <- args[4]

#download/install packages
pkg <- c("dplyr", "terra", "raster", "sp")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

load_HM<- function(data){
  # Convert to SpatRaster
  d_s1<-data %>% filter(S=="S1") %>% dplyr::select(decimallongitude, decimallatitude)
  d_s2<-data %>% filter(S=="S2") %>% dplyr::select(decimallongitude, decimallatitude)
  
  
  #hm_1970 <- terra::rast(hm_1970)
  #hm_2025 <- terra::rast(hm_2025)

  cat(sprintf("  HM 1970: %s\n", paste(dim(hm_1970), collapse=" x ")))
  cat(sprintf("  HM 2025: %s\n", paste(dim(hm_2025), collapse=" x ")))

  # Extract values all at once
  values_S1 <- terra::extract(hm_1970, d_s1, cells = TRUE)
  values_S2 <- terra::extract(hm_2025, d_s2, cells = TRUE)
  
  cat("Extracted values of S1 : ", length(values_S1[,2]), "\n")
  cat("Rows of S1 : ", nrow(data[data$S =="S1",]), "\n \n")
  cat("Extracted values of S2 : ", length(values_S2[,2]), "\n")
  cat("Rows of S2: ", nrow( data[data$S =="S2",]), "\n")


  # Join results
  data_hm<-data
  data_hm$HM[data_hm$S =="S1"] <- values_S1[,2]
  data_hm$HM[data_hm$S =="S2"] <- values_S2[,2]
  
  # Remove the NAs
  #data_pca <- data_pca %>% drop_na(PCA_1, PCA_2) # from 408158 to 407807 rows
  cat(sprintf("Valores HM extraídos: %d\n", sum(!is.na(data_hm$HM))))
  cat(sprintf("Valores NA: %d\n", sum(is.na(data_hm$HM))))
  
  return(data_hm)
}

################################################################################
#code
################################################################################

#### #### load data #### ####
cat("Loading data \n")
#load(hm1970)
#load(hm2025)
hm_1970 <- terra::rast(hm1970)      # should define climate_data2
hm_2025 <- terra::rast(hm2025)      # should define climate_data2


LH_C_B_PP <- read.csv(occurrences)

cat("Loading data completed\n")

cat("loading HM ...  \n")
LH_C_B_PP_HM <- load_HM(LH_C_B_PP)
cat("loading completed ! \n")


cat("Occurrences without HM : \n")
LH_C_B_PP_HM[is.na(LH_C_B_PP_HM$HM), ]

write.csv(LH_C_B_PP_HM, output)