###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Obtain optimla parameters for BRT
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
input <-args[1]
table_top5 <-args[2]
output <- args[3]

#download/install packages
pkg <- c("dplyr", "gbm", "dismo", "tidyr", "formattable")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Code
################################################################################

paramS2<-read.csv(input)

##do a mean x repetition and select 
means_paramS5<-paramS2 %>%  group_by(nTrees,treeComplexity, learningRate, bagFraction) %>% summarise(avAUC= mean(meanAUC), avseauc=mean(seAUC)) #

top5_param<-means_paramS5 %>%  ungroup() %>%  slice_max(avAUC, n = 5, with_ties = FALSE)

top5_param
write.csv(top5_param, table_top5)
write.csv(top5_param[1,], output)
