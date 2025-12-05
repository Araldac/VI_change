###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Order columns final dataset
# Date: 03/12/2025
###############################################################################

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
output <- args[2]

library(dplyr)
################################################################################
#Code
################################################################################

data <- read.csv(occurrences)

data_ref <- data %>% dplyr::select(Occurrence, 
                                    decimallatitude,
                                    decimallongitude,
                                    S,
                                    PA_N,
                                    BEN,
                                    ISO,
                                    PCA_1,
                                    PCA_2,
                                    Leptomyrmecini,
                                    Tapinomini,
                                    Bothriomyrmecini,
                                    Dolichoderini,
                                    trade,
                                    HM)

write.csv(data_ref, output)