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
output_all <- args[2]


#download/install packages
pkg <- c("terra", "dplyr", "tidyr", "tiff", "sf")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################

#### #### load data #### ####

library(dplyr)

all_occ <- lapply(occurrences, function(f) {
  df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
  return(df)
}) %>% bind_rows()


write.csv(all_occ, row.names = FALSE, file= output)