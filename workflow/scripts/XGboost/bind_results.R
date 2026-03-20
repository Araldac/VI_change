###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Bind datasets
# Date: 26/11/2025
###############################################################################

## Load arguments and packages####

args <- commandArgs(trailingOnly = TRUE)
list_files <- args[1]
output <- args[2]


################################################################################
#Code
################################################################################

library(dplyr)

list_files <- strsplit(list_files, " +")
list_files<-unlist(list_files)

print(list_files)
all_oc <- lapply(list_files, function(f) {{
df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
return(df)
}}) %>% dplyr::bind_rows()  

head(all_oc)
write.csv(all_oc, output)
