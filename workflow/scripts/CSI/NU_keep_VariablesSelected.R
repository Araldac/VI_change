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
v_selected <- args[2]
output <- args[3]


#download/install packages
pkg <- c("terra", "dplyr", "tidyr", "tiff", "sf")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################

#### #### load data #### ####
lthin <- read.csv(occurrences, sep = ",")

vars <- strsplit(v_selected, ",")[[1]]
cat("Varables selected: ", vars, "\n")

#keep only the bios in here
all_columns <- colnames(lthin)

bio_columns <- all_columns[grep("bio_", all_columns)]

bio_not_selected <- bio_columns[! bio_columns %in%  vars]

occ_bio_selected <- envtcel_thin %>% dplyr::select(-bio_not_selected)

write.csv(occ_bio_selected, file= output)