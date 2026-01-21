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
mix_CSI_S1 <- args[2]
mix_CSI_S2 <- args[3]
output_dataset <- args[4]

#download/install packages
pkg <- c("terra", "dplyr", "tidyr", "tiff", "sf")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))