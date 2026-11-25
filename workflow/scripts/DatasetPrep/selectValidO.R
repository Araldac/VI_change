###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Filter using dta from Bertelsmeier et al (2017) on first alien occurrences
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
# Namibia's ISO2 code "NA" is read as missing; GBIF leaves the field empty when the country
# is unknown, so a missing code here is Namibia
occ_thin20_colsR$countrycode[is.na(occ_thin20_colsR$countrycode)] <- "NA"
# Remote islands have their own first records (e.g. "Azores"), but GBIF gives their occurrences
# the country code of the mainland (e.g. "PT"). Compare island occurrences with the island's
# first record instead, using their Antmaps region (BEN). An island without a first record
# for the species is not filtered, like a country without one.
island_bens <- c("Azores" = "BEN20537", "Madeira" = "BEN20575", "Canary Islands" = "BEN20541",
                 "Galapagos Islands" = "BEN20555", "Easter Island" = "BEN20552",
                 "Juan Fernandez" = "BEN20564", "US-Hawaii" = "BEN20171")
occ_thin20_colsR$fr_region <- occ_thin20_colsR$countrycode
on_island <- occ_thin20_colsR$BEN %in% island_bens
occ_thin20_colsR$fr_region[on_island] <- names(island_bens)[match(occ_thin20_colsR$BEN[on_island], island_bens)]
head(occ_thin20_colsR)
##filter occurrences with published dataset
first_records_df <- read.csv(first_record, na.strings = "") # keep Namibia's ISO2 code "NA"
head(first_records_df)

occ_valid <- occ_thin20_colsR
for (i in seq_len(nrow(first_records_df))) {


    country <- first_records_df$Region[i]
    if (! country %in% occ_valid$fr_region) next
    #country <- countrycode::countrycode(country, origin="iso2c", destination="iso3c")
    date <- first_records_df$FirstRecord[i]
    
    occ_valid <- occ_valid %>% filter(! (fr_region == country &  year < date))

    

  }

  cat("s1: ", nrow(occ_thin20_colsR[occ_thin20_colsR$S == "S1",]), "--> ", nrow(occ_valid[occ_valid$S == "S1",]), "\n     s2: ", nrow(occ_thin20_colsR[occ_thin20_colsR$S == "S2",]), "--> ", nrow(occ_valid[occ_valid$S == "S2",]),"\n")
  

occ_valid$fr_region <- NULL # helper column only
write.csv(occ_valid, output)

