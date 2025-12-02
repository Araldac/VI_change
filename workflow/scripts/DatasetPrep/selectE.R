###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Sleect only the exotic occurrencesd
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
thin_data <- args[1]
distributionData <- args[2]
antmaps_pol <-args[3]
sp_name<-args[4]
output <- args[5]

pkg <- c("spacesXYZ", "geosphere", "parallel", "raster", "readr", "colorspace", "rgl", "ade4", "farver")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

pkg <- c("magrittr", "CoordinateCleaner", "countrycode", "rnaturalearth", "sf", "sp", "sfheaders", "dplyr", "tidyr")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Functions
################################################################################
coords2admin2 <- function(data, admin = antmaps_pol) {

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
  
  return(data)
}

"select_E"<-function(data, species_name) {
  #assign('sp_disData', get(load(distributionData)))
  info_sp <- antmaps_dis[[species_name]]
  exotic_ben <- info_sp$gid[info_sp$category == "E"]
  data_clean<-data[data$BEN %in% exotic_ben,]
  
  return(data_clean)
}


################################################################################
#Code
################################################################################

# Just load the files; objects will appear in the environment
load(antmaps_pol)
load(distributionData)

# Load RData files correctly
#load(antmaps_pol)
#load(distributionData)
#antmaps_dis <- (load(distributionData))

## Loading occurrences of sp from GBIF in 2025
occ_thin20 <- read.csv(thin_data, sep = ",") #

### Filter only E ####----------------------------------------
#Filter occurences that are Exotic (remove indoor and native)
#load(antmaps_pol)
#load(distributionData)

occ_ben<-coords2admin2(occ_thin20)

occ_E<-select_E(occ_ben, sp_name)

occ_E_cols<- occ_E %>% dplyr::select(decimallatitude, decimallongitude, year, S, BEN, countrycode)

write.csv(occ_E_cols, output)
