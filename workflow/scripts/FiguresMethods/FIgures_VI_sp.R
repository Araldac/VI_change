###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Create a figure for each species witht the variable importance 
# Date: 26/11/2025
###############################################################################

options(warn = 1)

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
Raw = args[1]
PCA = args[2]
Sp_name = args[3]




################################################################################
#Code
################################################################################

dataRF_Raw <- read.csv(Raw)
dataRF_PCA <- read.csv(PCA)

##load sp name

dataRF_PCA$Model <- "PCA"
dataRF_Raw$Model <- "Raw"

dataRF_PCA$Species <- Sp_name
dataRF_Raw$Species <- Sp_name

