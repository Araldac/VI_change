###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Obtain optimla parameters for RF
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
input <-args[1]
table_top5 <-args[2]
output <- args[3]

#download/install packages
pkg <- c("dplyr", "tidyr")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Code
################################################################################

paramS2<-read.csv(input, sep =",")

##do a mean x repetition and select 
means_paramS2<-paramS2 %>%  group_by(ntree, mtry) %>% 
  summarise(meanOOB= mean(oob_error), seOOB=sd(oob_error)) #

top5_param <- means_paramS2 %>%  ungroup() %>%  
  slice_min(meanOOB, n = 5, with_ties = FALSE)

top5_param

write.csv(top5_param, table_top5)
print("file1")
write.csv(top5_param[1,], output)