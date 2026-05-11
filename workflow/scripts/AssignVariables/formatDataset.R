###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Order columns final dataset
# Date: 03/12/2025
###############################################################################

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
sp_name <- args[2]
biotic <- args[3]
output <- args[4]

library(dplyr)
################################################################################
#Code
################################################################################

###extracte biotic variables
#biotic_var <- strsplit(biotic, " +")
#biotic_var<-unlist(biotic_var)
#cat(sprintf("  Tribes to analyze: %s\n", paste(biotic_var, collapse = ", ")))

data <- read.csv(occurrences)

data$Taxa <- sp_name

data_ref <- data %>% dplyr::select(Taxa,
                                    Occurrence, 
                                    decimallatitude,
                                    decimallongitude,
                                    S,
                                    PA_N,
                                    BEN,
                                    ISO,
                                    PCA_1,
                                    PCA_2,
                                    trade,
                                    HM,
                                    MPD)

write.csv(data_ref, output)