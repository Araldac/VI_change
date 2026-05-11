###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: add MPD data to each occurrence/background point
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
sp_MPD <- args[2]
antmaps_pol <-args[3]
our_sp_name <- args[4]
output <- args[5]

#download/install packages
pkg <- c("dplyr", "sp", "rworldmap", "stringr", "tidyverse", "tidyr", "sf", "rworldxtra")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

#Assign BEN and country to each occurence

coords2admin2 <- function(data, admin = antmaps_pol) {
  cat("Assign BEN and ISO to occurrences ... \n")
  points <- data %>% dplyr::select(decimallongitude, decimallatitude)
  
  # Convert to spatial points
  pointsSP <- SpatialPoints(points, proj4string = CRS("EPSG:4326"))
  pointsSF <- st_as_sf(pointsSP)
  
  # Ensure CRS is set
  if (is.na(st_crs(admin))) {
    st_crs(admin) <- 4326
  }

  # Try intersection first
  indices <- st_intersects(pointsSF, admin, sparse = TRUE)
  
  # Fallback: assign nearest polygon if no intersection
  result <- sapply(seq_along(indices), function(i) {
    if (length(indices[[i]]) == 0) {
      # Compute distances to all polygons
      distances <- st_distance(pointsSF[i, ], admin)
      nearest_idx <- which.min(distances)
      return(nearest_idx)
    } else {
      return(indices[[i]][1])
    }
  })
  
  data$BEN <- admin$BEN[result]
  data$ISO <- admin$ISO[result]
  cat("BEN and ISO Assigned! \n")
  return(data)
}


################################################################################
#Code
################################################################################

#### #### Load the necessary data #### #### 
load(antmaps_pol)
if (!exists("antmaps_pol")) stop("antmaps_pol not found")

occ_PA <- read.csv(occurrences)
MPD_SP <- read.csv(sp_MPD)

head(MPD_SP)
head(MPD_SP[, 2])

#### Fix missing data ####
antmaps_pol$NAME[antmaps_pol$BEN=="BEN20610"] <- "Italy" #complete data with BEN20610 Tuscan Archipelago

#### Assign bentities to occurrences, if not in an exact poygon finds the nearest ####
cat("\nAssigning bentities to occurrences (this may take a few minutes)...\n")
#LH_C_B<-coords2admin2(occ_PA)
LH_C_B <- occ_PA

#LH_C_B$MPD <- NA
for (ben in unique(LH_C_B$BEN)){
  #print(ben)
  mpd_ben <- MPD_SP[MPD_SP$X == ben, 2]
  #print(mpd_ben)
  LH_C_B$MPD[LH_C_B$BEN == ben] <- mpd_ben 
  
}

head(LH_C_B)

cat("\nSaving results...\n")
write.csv(LH_C_B, output)