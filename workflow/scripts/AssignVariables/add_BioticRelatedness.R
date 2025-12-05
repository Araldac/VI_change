
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


##returns the number of genus for each tribe per bentity
sametrib_extraction<- function(data_species, tribe_genus_data){ 
    cat("Start extraction of the number of genus per tribe... \n")
  
  # Validate inputs
  if (!is.data.frame(tribe_genus_data) || 
      !all(c("Genus", "Tribe") %in% colnames(tribe_genus_data))) {
    stop("tribe_genus_data must be a data frame with 'Genus' and 'Tribe' columns")
  }

  
  ##Part 1- extract all the specie s in the same family per tribe 
  c_name<-c("Species", "Genus", "Tribe", "Ben", "Country")
  sp_fam<-data.frame(matrix(nrow=0, ncol=length(c_name)))
  colnames(sp_fam)<-c_name
  fam_list <- list()  # Store results in a list first
  
  

  # for (i in 1:length(data_species)) {
#   sp <- data_species[i]
#   name_sp <- names(sp)
#   gen <- str_split(name_sp, " ")[[1]][1]
#   
#   # Check if genus is in tribe data
#   if(gen %in% tribe_genus_data$Genus) {
#     tribe_name <- tribe_genus_data$Tribe[tribe_genus_data$Genus == gen]
#     n_rows <- nrow(sp[[1]])
#     
#     if( nrow(sp) > 0) {
#       # Match each Ben code to get the corresponding country name
#       ben_codes <- sp[[1]]$gid
#       country_names <- sapply(ben_codes, function(ben) {
#         match_idx <- which(antmaps_pol$BEN == ben)
#         if(length(match_idx) > 0) {
#           return(antmaps_pol$NAME[match_idx[1]])
#         } else {
#           return(NA_character_)
#         }
#       })
#       
#       fam_list[[i]] <- data.frame(
#         Species = rep(name_sp, n_rows),
#         Genus = rep(gen, n_rows),
#         Tribe = rep(tribe_name, n_rows),
#         Ben = ben_codes,
#         Country = country_names,
#         stringsAsFactors = FALSE
#       )
#     }
#   }
#   if(i %% 100 == 0) print(paste0("Done species nb ", i, "/", length(data_species)))
# }

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
  
  cat("obtainng the number of genus per family ... \n")
  
  ngen <- sp_fam %>%
    group_by(Ben, Country, Tribe) %>%
    summarise(n_genera = n_distinct(Genus), .groups = "drop") %>%
    pivot_wider(
      names_from = Tribe, 
      values_from = n_genera, 
      values_fill = 0
    ) %>%
    as.data.frame()
  
 cat("✓ Tribe extraction completed!\n")
  return(ngen)
  
}

################################################################################
#Code
################################################################################

#### #### Load the necessary data #### #### 
load(antmaps_pol)
if (!exists("antmaps_pol")) stop("antmaps_pol not found")

load(distributionData)
if (!exists("antmaps_dis")) stop("antmaps_dis not found")

realtedSp <- read.csv(relatedSpecies, sep= ";")
head(realtedSp)
occ_PA <- read.csv(occurrences)

#### Process tribe list ####
tribe_list <- strsplit(tribe_list, " +")
tribe_list<-unlist(tribe_list)
cat(sprintf("  Tribes to analyze: %s\n", paste(tribe_list, collapse = ", ")))

#### Fix missing data ####
antmaps_pol$NAME[antmaps_pol$BEN=="BEN20610"]<-"Italy" #complete data with BEN20610 Tuscan Archipelago

#### Assign bentities to occurrences, if not in an exact poygon finds the nearest ####
cat("\nAssigning bentities to occurrences (this may take a few minutes)...\n")
LH_C_B<-coords2admin2(occ_PA)
# Check for unassigned points
unassigned <- LH_C_B[!complete.cases(LH_C_B$BEN), ]
if (nrow(unassigned) > 0) {
  cat(sprintf("⚠ Warning: %d points could not be assigned to a bentity\n", nrow(unassigned)))
} else {
  cat("✓ All points assigned to bentities\n")
}

#Extract the species of the family per bentity using the list of species per bentity
antmaps_dis_woLH<-antmaps_dis[names(antmaps_dis) != our_sp_name]
ngen<-sametrib_extraction(antmaps_dis_woLH, realtedSp)
cat(sprintf("\n✓ Genus counts extracted for %d bentities\n", nrow(ngen)))



#Obtain the number of genus for each tribe in the Doloichootea family per Bentity
 cat("Obtaining the number of genus for each tribe  ! \n")
# Ensure all expected tribe columns exist
for (tribe in tribe_list) {

  # Ensure all expected tribe columns exist in ngen
  if (!tribe %in% colnames(ngen)) {
    ngen[[tribe]] <- 0
  }
  
  #show max values per tribe
  if (tribe %in% colnames(ngen)) {
    max_val <- max(ngen[[tribe]], na.rm = TRUE)
    cat(sprintf("  %s: max = %d genera\n", tribe, max_val))
  }

  #create columns for each tribe
  LH_C_B[[tribe]] <- NA_real_  # ✓ Creates separate column for each tribe
  }

cat("\nAssigning genus counts to each occurrence...\n")

# Find rows in ngen that match BEN in LH_C_B
match_indices <- match(LH_C_B$BEN, ngen$Ben)

# Get the new column indices in LH_C_B (tribes are at the end)
tribe_cols_LH <- which(colnames(LH_C_B) %in% tribe_list)

##Assign each observation the number of genus per family
#assign number of genera per tribe per country
# Assign values
for (i in seq_along(tribe_list)) {
  tribe <- tribe_list[i]
  
  if (tribe %in% colnames(ngen)) {
    # Get values from ngen for matched rows
    values_to_assign <- ngen[[tribe]][match_indices]
    
    # Assign to LH_C_B
    LH_C_B[[tribe]] <- values_to_assign
  }
}

# Replace NA with 0 for tribe columns
LH_C_B[tribe_cols_LH] <- lapply(LH_C_B[tribe_cols_LH], function(x) {
  x[is.na(x)] <- 0
  return(x)
})

#Verify assignment worked
cat("\nVerifying assignment:\n")
for (tribe in tribe_list) {
  if (tribe %in% colnames(LH_C_B)) {
    max_val <- max(LH_C_B[[tribe]], na.rm = TRUE)
    cat(sprintf("  %s: max = %d genera per occurrence\n", tribe, max_val))
  }}

cat("\nSaving results...\n")
cat(sprintf("  Final dataset: %d rows × %d columns\n", nrow(LH_C_B), ncol(LH_C_B)))
write.csv(LH_C_B, output)