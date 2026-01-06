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
PCA_climate <- args[2]
antmaps_pol <- args[3]
sp_name <-args[4]
speciesDistribution <- args[5]
output <- args[6]

#download/install packages
pkg <- c("terra", "dplyr", "tidyr", "raster", "tiff", "sf")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Functions
################################################################################


#### #### load PCA #### ####
# Extract the PCA scores for each occurrence with terra (optimized approach):
#load_PCA <- function(data, climate_data){
#  cat("Loading PCA data...\n")
  
  # Convert to SpatRaster
#  climate_data_terra <- terra::rast(climate_data)
  
#    cat("Loaextract PCA data...\n")
  
  # Extract values all at once
#  coords <- data[, c("decimallongitude", "decimallatitude")]
#  values <- terra::extract(climate_data_terra, coords, cells = TRUE)
  
#     cat("join PCA data...\n")
  
  # Join results using base R (avoid dplyr)
#  data_pca <- cbind(data, values[, -1])  # drop ID column
  
#       cat("remove NA...\n")
  
  # Remove NAs using base R
#  complete_rows <- complete.cases(data_pca[, c("PCA_1", "PCA_2")])
#  data_pca <- data_pca[complete_rows, ]
  
#  cat(sprintf("Kept %d rows with complete PCA data\n", nrow(data_pca)))
  
#  return(data_pca)
#}
load_PCA<- function(data, climate_data){
  # Convert to SpatRaster
  cat("Loading PCA data...\n")
  #climate_data_terra <- terra::rast(climate_data)
  climate_data_terra<-terra::unwrap(climate_data)
  # Extract values all at once
  coords <- data[, c("decimallongitude", "decimallatitude")]

  cat("Extract PCA data...\n")
  values <- terra::extract(climate_data_terra, coords, cells = TRUE)
  
  cat("join PCA data...\n")
  # Join results
  data_pca <- cbind(data, values[, -1])  # drop ID column
  
  cat("remove NA...\n")
  
  # Remove the NAs
  data_pca <- data_pca %>% drop_na(PCA_1, PCA_2) # from 408158 to 407807 rows
  
  cat(sprintf("Kept %d rows with complete PCA data\n", nrow(data_pca)))

  return(data_pca)
}

#### #### load PA #### ####
PA_byS <- function(data, climate_data, antmaps_pol, antmaps_dis, species_name){
  
  set.seed(3)
  
  cat("Loading climatic data...\n")
  rastclim <- rast(climate_data)  
  
  # Use base R instead of dplyr pipes to avoid node stack overflow
  cat("Preparing S1 and S2 datasets...\n")
  
  # S1 data - using base R
  s1_idx <- data$S == "S1"
  input_biomodS1 <- data[s1_idx, c("decimallatitude", "decimallongitude", "PCA_1", "PCA_2")]
  input_biomodS1$Occurrence <- 1
  
  # S2 data - using base R
  s2_idx <- data$S == "S2"
  input_biomodS2 <- data[s2_idx, c("decimallatitude", "decimallongitude", "PCA_1", "PCA_2")]
  input_biomodS2$Occurrence <- 1
  
  # Get native zone info
  cat("Processing native zone...\n")
  info_sp <- antmaps_dis[[species_name]]
  native_ben <- info_sp$gid[info_sp$category == "N"]
  
  # Convert native polygons
  native_subset <- antmaps_pol[antmaps_pol$BEN %in% native_ben, ]
  native_pol_vect <- vect(native_subset)
  
  # Project if needed
  if (!terra::same.crs(native_pol_vect, rastclim)) {
    cat("Reprojecting native polygons...\n")
    native_pol_vect <- terra::project(native_pol_vect, rastclim)
  }
  
  # Create coordinate vectors
  cat("Creating coordinate vectors...\n")
  coord_biomodS1 <- vect(input_biomodS1[, c("decimallongitude", "decimallatitude")],
                         geom = c("decimallongitude", "decimallatitude"),
                         crs = crs(rastclim))
                         
  coord_biomodS2 <- vect(input_biomodS2[, c("decimallongitude", "decimallatitude")],
                         geom = c("decimallongitude", "decimallatitude"),
                         crs = crs(rastclim))
  
  # Buffer points (200 km)
  #cat("Creating buffers (200 km)...\n")
  #buf_points_S1 <- terra::buffer(coord_biomodS1, width = 200000)
  #buf_points_S2 <- terra::buffer(coord_biomodS2, width = 200000)
  
  # Combine features
  #cat("Combining buffers with native zone...\n")
  #buf_1 <- rbind(buf_points_S1, native_pol_vect)
  #buf_2 <- rbind(buf_points_S2, native_pol_vect)
  
  # CRITICAL: Simplify geometries if too complex
  #buf_1 <- terra::simplifyGeom(buf_1, tolerance = 1000)
  #buf_2 <- terra::simplifyGeom(buf_2, tolerance = 1000)
  
  # Crop first to reduce raster size
  #cat("Cropping rasters...\n")
  #buf_1_extent <- ext(buf_1)
  #buf_2_extent <- ext(buf_2)
  
  rastclim_cropped1 <- terra::crop(rastclim, native_pol_vect, snap = "out")
  rastclim_cropped2 <- terra::crop(rastclim, native_pol_vect, snap = "out")
  
  # Mask (on smaller rasters)
  cat("Masking regions...\n")
  region_outbuf1 <- terra::mask(rastclim_cropped1, native_pol_vect, inverse = TRUE)
  region_outbuf2 <- terra::mask(rastclim_cropped2, native_pol_vect, inverse = TRUE)
  
  # Clean up immediately
  rm(rastclim, rastclim_cropped1, rastclim_cropped2, native_pol_vect)
  #rm(buf_points_S1, buf_points_S2, native_pol_vect)
  gc(verbose = FALSE)
  
  # Sample PA for S2 (1 round)
  cat("Sampling PA for S2 (1 round)...\n")
  n_s2 <- nrow(input_biomodS2)
  
  PA_2_list <- vector("list", 10)
  
  for (i in 1:10) {
      bg_rand_outbuf2 <- terra::spatSample(region_outbuf2, n_s2, 
                                       method = "random", 
                                       na.rm = TRUE, 
                                       as.points = TRUE)
    
    pa2_i <- terra::as.data.frame(bg_rand_outbuf1, geom = "XY")
    pa2_i$PA_N <- i
    pa2_i$S <- "S2"
    
    PA_2_list[[i]] <- pa2_i
    
    cat(sprintf("  For S2 Round %d/10 completed\n", i))
    
    # Clean up
    rm(bg_rand_outbuf2, pa2_i)
  }

  PA_2 <- do.call(rbind, PA_2_list)
  rm(PA_1_list)
  gc(verbose = FALSE)
  
  # Sample PA for S1 (10 rounds)
  cat("Sampling PA for S1 (10 rounds)...\n")
  n_s1 <- nrow(input_biomodS1)
  
  PA_1_list <- vector("list", 10)
  
  for (i in 1:10) {
    bg_rand_outbuf1 <- terra::spatSample(region_outbuf1, n_s1, 
                                         method = "random",
                                         na.rm = TRUE, 
                                         as.points = TRUE)
    
    pa1_i <- terra::as.data.frame(bg_rand_outbuf1, geom = "XY")
    pa1_i$PA_N <- i
    pa1_i$S <- "S1"
    
    PA_1_list[[i]] <- pa1_i
    
    cat(sprintf("  For S1 Round %d/10 completed\n", i))
    
    # Clean up
    rm(bg_rand_outbuf1, pa1_i)
    if (i %% 3 == 0) gc(verbose = FALSE)
  }
  
  # Combine list elements (more efficient than repeated rbind)
  PA_1 <- do.call(rbind, PA_1_list)
  rm(PA_1_list)
  gc(verbose = FALSE)
  
  # Clean up large objects
  rm(region_outbuf1, region_outbuf2)
  gc(verbose = FALSE)
  
  # Combine PA datasets
  cat("Combining PA datasets...\n")
  PA_global <- rbind(PA_1, PA_2)
  colnames(PA_global)[1:4] <- c("PCA_1", "PCA_2", "decimallongitude", "decimallatitude")
  PA_global$Occurrence <- 0
  
  # Reorder columns using base R
  PA_global <- PA_global[, c("Occurrence", "decimallatitude", "decimallongitude", 
                             "PCA_1", "PCA_2", "S", "PA_N")]
  
  # Add S labels to presence data
  input_biomodS1$S <- "S1"
  input_biomodS2$S <- "S2"
  
  # Combine presence data
  cat("Combining presence data...\n")
  P_global <- rbind(input_biomodS1, input_biomodS2)
  P_global$PA_N <- NA
  
  # Reorder columns
  P_global <- P_global[, c("Occurrence", "decimallatitude", "decimallongitude", 
                           "PCA_1", "PCA_2", "S", "PA_N")]
  
  # Final combination
  cat("Creating final dataset...\n")
  P_PA_global <- rbind(P_global, PA_global)
  
  cat("PA_byS completed successfully!\n")
  return(P_PA_global)
}


################################################################################
#Code
################################################################################

#### #### load data #### ####
occ_E_cols <- read.csv(occurrences, sep = ",")

#portable_obj <- readRDS(PCA_climate)
#climate_data2 <- rebuild_spatraster(portable_obj)

#print(climate_data2)
#climate_data2 <- readRDS(PCA_climate)
load(PCA_climate)      # should define climate_data2

if (!exists("climate_data2")) stop("Object 'climate_data2' not found in PCA_climate RData")

load(antmaps_pol)      # should define antmaps_pol
if (!exists("antmaps_pol")) stop("Object 'antmaps_pol' not found in antmaps_pol RData")

load(speciesDistribution)  # should define antmaps_dis
if (!exists("antmaps_dis")) stop("Object 'antmaps_dis' not found in speciesDistribution RData")


#### #### load PCA #### ####
occ_PCA <- load_PCA(occ_E_cols, climate_data2)

#### #### assign PA #### ####
occ_PA <- PA_byS(occ_PCA, climate_data2, antmaps_pol, antmaps_dis, sp_name)
#plot PA and P
class(occ_PA)

write.csv(occ_PA, file= output)




