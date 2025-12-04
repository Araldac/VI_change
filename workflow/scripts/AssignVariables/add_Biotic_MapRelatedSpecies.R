
###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Biotic Data:obtain the number of relted genus for each close tribe per bentity
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
relatedSpecies <- args[2]
distributionData <- args[3]
antmaps_pol <-args[4]
our_sp_name <- args[5]
tribe_list <-args[6]
output <- args[7]


#download/install packages
pkg <- c("dplyr", "sp", "rworldmap", "stringr", "tidyverse", "tidyr", "sf", "rworldxtra")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

#Assign BEN and country to each occurence

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


##returns the number of genus for each tribe per bentity
sametrib_extraction<- function(data_species, tribe_genus_data){ 
  
  
  #data_species=antmaps_dis
  #tribe_genus_data=tribe_genus
  
  
  ##Part 1- extract all the specie s in the same family per tribe 
  c_name<-c("Species", "Genus", "Tribe", "Ben", "Country")
  sp_fam<-data.frame(matrix(nrow=0, ncol=length(c_name)))
  colnames(sp_fam)<-c_name
  fam_list <- list()  # Store results in a list first
  
  
  for (i in 1:length(data_species)) {
    sp <- data_species[i]
    name_sp <- names(sp)
    gen <- str_split(name_sp, " ")[[1]][1]
    
    if(gen %in% tribe_genus_data$Genus) {
      tribe_name <- tribe_genus_data$Tribe[tribe_genus_data$Genus == gen]
      n_rows <- nrow(sp[[1]])
      
      if(n_rows > 0) {
        # Match each Ben code to get the corresponding country name
        ben_codes <- sp[[1]]$gid
        country_names <- sapply(ben_codes, function(ben) {
          match_idx <- which(antmaps_pol$BEN == ben)
          if(length(match_idx) > 0) {
            return(antmaps_pol$NAME[match_idx[1]])
          } else {
            return(NA)
          }
        })
        
        fam_list[[i]] <- data.frame(
          Species = rep(name_sp, n_rows),
          Genus = rep(gen, n_rows),
          Tribe = rep(tribe_name, n_rows),
          Ben = ben_codes,
          Country = country_names,
          stringsAsFactors = FALSE
        )
      }
    }
    if(i %% 100 == 0) print(paste0("Done species nb ", i, "/", length(data_species)))
  }
  
  sp_fam <- do.call(rbind, fam_list)
  
  ##Part 2- Obtain the number of genus per family by Bentity
  ngen <- sp_fam %>%
    group_by(Ben, Country, Tribe) %>%
    summarise(n_genera = n_distinct(Genus), .groups = "drop") %>%
    pivot_wider(
      names_from = Tribe, 
      values_from = n_genera, 
      values_fill = 0
    ) %>%
    as.data.frame()
  
  return(ngen)
  
}

################################################################################
#Code
################################################################################

#### #### Load the necessary data #### #### 
load(antmaps_pol)
load(distributionData)
list_relsp <- read.csv(relatedSpecies)
occ_PA <- read.csv(occurrences)

tribe_list <- strsplit(tribe_list, " +")
tribe_list<-unlist(tribe_list)

antmaps_pol$NAME[antmaps_pol$BEN=="BEN20610"]<-"Italy" #complete data with BEN20610 Tuscan Archipelago

##assigns each observation the benityt if it intersects with it's polygon. If ther eis no intersection itis assigned to the nearest polygon
#takes 3 minutes
LH_C_B<-coords2admin2(occ_PA)
##check if all the points have countries assigned
LH_C_B[!complete.cases(LH_C_B$BEN), ]

#Extract the species of the family per bentity using the list of species per bentity
antmaps_dis_woLH<-antmaps_dis[names(antmaps_dis) != our_sp_name]
ngen<-sametrib_extraction(antmaps_dis_woLH, tribe_genus)

#Obtain the number of genus for each tribe in the Doloichootea family per Bentity

# Ensure all expected tribe columns exist
for (tribe in tribe_list) {
  if (!tribe %in% colnames(ngen)) {
    ngen[[tribe]] <- 0
  }
  
  paste0("Max Bentity of ", tribe, " ", max(ngen[tribe]))
  
  #create columns for each tribe
  LH_C_B<-cbind(LH_C_B, tribe = NA)
  }

##Assign each observation the number of genus per family
#assign number of genera per tribe per country
LH_C_B[,10:13] <- ngen[match(LH_C_B$BEN, ngen$Ben), 3:6]

#replace NA with 0 
LH_C_B[, 10:13][is.na(LH_C_B[, 10:13])] <- 0

#check that the values ar ein the right columns (we should expect the same resuts as above)
for (tribe in tribe_list) {
  paste0("Max Bentity of ", tribe, " ", max(LH_C_B[tribe]))
  
  }


write.csv(LH_C_B, output)