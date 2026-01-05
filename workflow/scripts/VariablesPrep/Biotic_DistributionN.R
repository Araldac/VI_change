###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Biotic keep only native occurences in Antmaps_dis
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
distributionData <- args[1]
output <- args[2]

#download/install packages
pkg <- c("dplyr")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Code
################################################################################
load(distributionData)
if (!exists("antmaps_dis")) stop("antmaps_dis not found")


N_antmaps_dis <- antmaps_dis
for (i in 1:length(N_antmaps_dis)){
  sp<-N_antmaps_dis[[i]]
  sp_N <- sp %>% filter(category == "N")
  N_antmaps_dis[[i]]<-sp_N
  cat(i, "/15556 done \n")
  
}

save(N_antmaps_dis, file= output)