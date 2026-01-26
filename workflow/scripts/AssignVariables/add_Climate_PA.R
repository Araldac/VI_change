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
#imageS1 <- args[7]
#imageS2 <- args[8]

#download/install packages
pkg <- c("terra", "dplyr", "tidyr", "raster", "tiff", "sf")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Functions
################################################################################


#### #### load PCA #### ####
load_PCA<- function(data, climate_data){
  # Convert to SpatRaster
  cat("Loading PCA data...\n")
  #climate_data_terra <- terra::rast(climate_data)
  #climate_data_terra<-terra::unwrap(climate_data)
  # Extract values all at once
  coords <- data[, c("decimallongitude", "decimallatitude")]

  cat("Extract PCA data...\n")
  values <- terra::extract(climate_data, coords, cells = TRUE)
  
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
sample_PA <- function(data, S, n_rounds, region_outbuf){
  
  # Sample all rounds at once
  total_pts <- nrow(data) * n_rounds
  bg_rand_all <- terra::spatSample(region_outbuf, total_pts, 
                                   "random", na.rm=TRUE, as.points=TRUE)
  pa_all <- terra::as.data.frame(bg_rand_all, geom="XY")
  
  # Add PA_N and S columns
  pa_all$PA_N <- rep(1:n_rounds, each=nrow(data))
  pa_all$S <- S
  
  return(pa_all)
}

# data<- occ_PCA
# species_name <- "Linepithema humile"
# climate_data <- climate_data2
PA_byS <- function(data, climate_data, antmaps_pol, antmaps_dis, species_name){

    set.seed(3)

    cat("Loading climatic data...\n")
    rastclim <- climate_data
    
  #separate two S datasets
  input_biomodS1<-data  %>%filter (S=="S1") %>% dplyr::select(decimallatitude, decimallongitude, PCA_1, PCA_2) %>% mutate(Occurrence = 1)
  input_biomodS2<-data  %>% filter (S=="S2") %>% dplyr::select(decimallatitude, decimallongitude, PCA_1, PCA_2)%>% mutate(Occurrence = 1) 

  #obtain coordinates in same crs as raster clim
  coord_biomodS1 <- vect(input_biomodS1[, c("decimallongitude", "decimallatitude")],
                       geom = c("decimallongitude", "decimallatitude"),
                       crs = crs(rastclim))  # WGS84
  coord_biomodS2 <- vect(input_biomodS2[, c("decimallongitude", "decimallatitude")],
                       geom = c("decimallongitude", "decimallatitude"),
                       crs = crs(rastclim))  # WGS84
  
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
    
    # Set all raster cells inside the buffer to NA using the argument inverse=T
    region_outbuf <- terra::mask(rastclim, native_pol_vect, inverse=T)
    
    #sample PA for S1 with 10 rounds 
     cat("Sampling PA for S1...\n")
     system.time(
       S1_PA<- sample_PA(input_biomodS1, "S1", 10, region_outbuf)
     )
     
     cat("Sampling PA for S2...\n")
     system.time(
     S2_PA<- sample_PA(input_biomodS2, "S2", 10, region_outbuf))
     PA_global<-dplyr::bind_rows(S1_PA, S2_PA)     
    
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
      return(P_PA_global)}



################################################################################
#Code
################################################################################

#### #### load data #### ####
occ_E_cols <- read.csv(occurrences, sep = ",")

#portable_obj <- readRDS(PCA_climate)
#climate_data2 <- rebuild_spatraster(portable_obj)

#print(climate_data2)
#climate_data2 <- readRDS(PCA_climate)
climate_data2 <- terra::rast(PCA_climate)      # should define climate_data2

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

cat("Final datasets nrows:", nrow(occ_PA), "\n")
cat("S1 \n")
cat("   -Presences:", nrow(occ_PA[occ_PA$S == "S1" & occ_PA$Occurrence == 1,]), "\n")
cat("   -Absences:",  nrow(occ_PA[occ_PA$S == "S1" & occ_PA$Occurrence == 0,]), "\n \n")
cat("S2 \n")
cat("   -Presences:", nrow(occ_PA[occ_PA$S == "S2" & occ_PA$Occurrence == 1,]), "\n")
cat("   -Absences:",  nrow(occ_PA[occ_PA$S == "S2" & occ_PA$Occurrence == 0,]), "\n")

 cat("Levels of PA_N for S2", levels(as.factor(occ_PA$PA_N[occ_PA$S == "S2"])), "\n")
  


write.csv(occ_PA, file= output)




