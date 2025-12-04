###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Add Climate and PA 
# Date: 26/11/2025
###############################################################################

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
PCA_climate <- args[2]
antmaps_pol <- args[3]
sp_name <-args[4]
speciesDistribution <- args[5]
output <- args[6]

#download/install packages
pkg <- c("terra", "dplyr", "tidyr", "raster")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

Cstack_info()
options(expressions = 5e5)              # allow deeper R recursion if any
Sys.setenv(R_CStackLimit = 5e8)         # relax R's C stack check (use with care)
Cstack_info()
################################################################################
#Functions
################################################################################

#### #### load PCA #### ####
# Extract the PCA scores for each occurrence with terra (optimized approach):
load_PCA<- function(data, climate_data){
  # Convert to SpatRaster
climate_data_terra <- terra::rast(climate_data)
# Extract values all at once
coords <- data[, c("decimallongitude", "decimallatitude")]
values <- terra::extract(climate_data_terra, coords, cells = TRUE)

# Join results
data_pca <- cbind(data, values[, -1])  # drop ID column

# Remove the NAs
data_pca <- data_pca %>% drop_na(PCA_1, PCA_2) # from 408158 to 407807 rows

return(data_pca)
}


#### #### load PA #### ####

PA_byS<-function(data, climate_data, antmaps_pol, antmaps_dis, species_name){
  
  set.seed(3)
  ##load climatic data as raster
  rastclim<-rast(climate_data)  
  
  #separate two S datasets
  input_biomodS1<-data  %>% filter (S=="S1") %>% dplyr::select(decimallatitude, decimallongitude, PCA_1, PCA_2) %>% mutate(Occurrence = 1)
  input_biomodS2<-data  %>% filter (S=="S2") %>% dplyr::select(decimallatitude, decimallongitude, PCA_1, PCA_2)%>% mutate(Occurrence = 1) 
  
  #obtain coordinates in same crs as raster clim
  
  coord_biomodS1 <- vect(input_biomodS1[, c("decimallongitude", "decimallatitude")],
                         geom = c("decimallongitude", "decimallatitude"),
                         crs = crs(rastclim))  # WGS84
  coord_biomodS2 <- vect(input_biomodS2[, c("decimallongitude", "decimallatitude")],
                         geom = c("decimallongitude", "decimallatitude"),
                         crs = crs(rastclim))  # WGS84
  
  
    
    if (!terra::same.crs(native_pol_vect, rastclim)) {
    native_pol_vect <- terra::project(native_pol_vect, rastclim)
    }


    # Place a buffer of 200 km radius around our presence points adn native zone
    #native zone
    info_sp <- antmaps_dis[[species_name]]
    native_ben<-info_sp$gid[info_sp$category == "N"]

    # Convert native polygons to SpatVector
    native_pol_vect <- vect(antmaps_pol[antmaps_pol$BEN %in% native_ben, ])


    # Buffer points (200 km) (keep native polygons as-is; avoid buffer(0))
    buf_points_S1 <- terra::buffer(coord_biomodS1, width = 200000)
    buf_points_S2 <- terra::buffer(coord_biomodS2, width = 200000)

    # Just append features instead of union (much cheaper)
    buf_1 <- rbind(buf_points_S1, native_pol_vect)
    buf_2 <- rbind(buf_points_S2, native_pol_vect)

    buf_1_extent <- ext(buf_1)
    buf_2_extent <- ext(buf_2)
  
    rastclim_cropped1 <- terra::crop(rastclim, buf_1_extent, snap = "out")
    rastclim_cropped2 <- terra::crop(rastclim, buf_2_extent, snap = "out")
  
    # Then mask (on smaller rasters)
    region_outbuf1 <- terra::mask(rastclim_cropped1, buf_1, inverse = TRUE)
    region_outbuf2 <- terra::mask(rastclim_cropped2, buf_2, inverse = TRUE)

    # Mask outside the appended polygons
    #region_outbuf1 <- terra::mask(rastclim, buf_1, inverse = TRUE)
    #region_outbuf2 <- terra::mask(rastclim, buf_2, inverse=T)
  
  PA_1<-data.frame(x=numeric(), 
                   y=numeric(),
                   PCA_1=numeric(),
                   PCA_2=numeric(),
                   Occurrence=numeric(),
                   S=character(),
                   PA_N=numeric()) 
  
  PA_2<-data.frame(x=numeric(), 
                   y=numeric(),
                   PCA_1=numeric(),
                   PCA_2=numeric(),
                   Occurrence=numeric(),
                   S=character(),
                   PA_N=numeric()) 
  
  #sample PA for S2 with 1 round
  bg_rand_outbuf2 <- terra::spatSample(region_outbuf2, nrow(input_biomodS2), "random", na.rm=T, as.points=TRUE)
  pa2_i<-terra::as.data.frame(bg_rand_outbuf2, geom="XY")
  pa2_i$PA_N<-1
  pa2_i$S<-"S2"
  PA_2<-rbind(PA_2, pa2_i)
  
  #sample PA for S1 with 10 rounds
  for (i in 1:10){
    
    bg_rand_outbuf1 <- terra::spatSample(region_outbuf1, nrow(input_biomodS1), "random", na.rm=T, as.points=TRUE)
    bg_rand_outbuf2 <- terra::spatSample(region_outbuf2, nrow(input_biomodS2), "random", na.rm=T, as.points=TRUE)
    
    pa1_i<-terra::as.data.frame(bg_rand_outbuf1, geom="XY")
    pa1_i$PA_N<-i
    pa1_i$S<-"S1"
    
    
    PA_1<-rbind(PA_1, pa1_i)
    
    
    print(paste0("PA round ", i, "/10"))
  }
  
  PA_global<-rbind(PA_1, PA_2)
  colnames(PA_global)[1:4] <- c("PCA_1", "PCA_2", "decimallongitude", "decimallatitude")
  PA_global$Occurrence<-0
  
  PA_global<-PA_global %>% dplyr::select(Occurrence, decimallatitude, decimallongitude, everything())
  input_biomodS1$S<- "S1"
  input_biomodS2$S<- "S2"
  
  P_global<-rbind(input_biomodS1, input_biomodS2)
  P_global$PA_N<-NA
  P_global<-P_global %>% dplyr::select(Occurrence, everything())
  
  P_PA_global<-rbind(P_global, PA_global)
  
  return(P_PA_global)
  
} 


################################################################################
#Code
################################################################################

#### #### load data #### ####
occ_E_cols <- read.csv(occurrences, sep = ",")

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

write.csv(occ_PA, output)