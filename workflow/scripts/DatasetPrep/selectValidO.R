###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Thinning
# Date: 26/11/2025
###############################################################################

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
data_thinned <- args[1]
first_record <- args[2]
output <- args[3]

 pkg <- c("dplyr")
 lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
# Code
################################################################################

occ_thin20_colsR <- read.csv(data_thinned)
head(occ_thin20_colsR)
##filter occurrences that shouldn't have been there with webber data
first_records_df <- read.csv(first_record)
head(first_records_df)

occ_valid <- occ_thin20_colsR
for (i in seq_len(nrow(first_records_df))) {


    country <- first_records_df$Region[i]
    if (! country %in% occ_valid$countrycode) next
    #country <- countrycode::countrycode(country, origin="iso2c", destination="iso3c")
    date <- first_records_df$FirstRecord[i]

    
    occ_valid <- occ_valid %>% filter(! (countrycode == country &  year < date))
    
    
    

  }

  cat("s1: ", nrow(occ_thin20_colsR[occ_thin20_colsR$S == "S1",]), "--> ", nrow(occ_valid[occ_valid$S == "S1",]), "\n     s2: ", nrow(occ_thin20_colsR[occ_thin20_colsR$S == "S2",]), "--> ", nrow(occ_valid[occ_valid$S == "S2",]),"\n")
  

write.csv(occ_valid, output)

