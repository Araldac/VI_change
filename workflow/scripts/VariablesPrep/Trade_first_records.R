###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Historical data: convert to current countries and interpolates NA
# Date: 26/11/2025
###############################################################################


args <- commandArgs(trailingOnly = TRUE)
path_first_rec <- args[1]
antmaps_pol <-args[2]
output <- args[5]


FR_NISO_forSMK<- function(first_records, species_name, acronym){
  
  #save first records
  FR_SP <- first_records[first_records$Taxon == species_name, ]
  file_path <- paste0("//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/data/variables/Trade/", acronym, "_first_records.csv" )
  write.csv(FR_SP, file= file_path)
  cat("first record", min(FR_SP$FirstRecord), "\n")
  #give list of names
  
  ts_dis <- antmaps_dis[[species_name]]
  N_gid <- ts_dis %>% filter(category == "N") %>% select(gid)
  iso_df <- st_drop_geometry(antmaps_pol) %>% filter(BEN %in% N_gid$gid) %>% select(ISO)
  iso_list <- (as.vector(unique(iso_df)))
  cat(paste(as.vector(iso_list), collapse = ", ")) 
  return(as.vector(iso_list))}


#load data
first_records <- read.csv(path_first_rec, sep = "\t")



FR_NISO_forSMK(first_records, "Linepithema humile", "LH")
FR_NISO_forSMK(first_records, "Paratrechina longicornis", "PL")
FR_NISO_forSMK(first_records, "Monomorium pharaonis", "MP")
FR_NISO_forSMK(first_records, "Tapinoma melanocephalum", "TM")
FR_NISO_forSMK(first_records, "Pheidole megacephala", "PM")
FR_NISO_forSMK(first_records, "Monomorium floricola", "MF")
FR_NISO_forSMK(first_records, "Hypoponera punctatissima", "HP")
FR_NISO_forSMK(first_records, "Hypoponera opaciceps", "HO")
FR_NISO_forSMK(first_records, "Anoplolepis gracilipes", "AG")
FR_NISO_forSMK(first_records, "Hypoponera eduardi", "HE")
FR_NISO_forSMK(first_records, "Tetramorium bicarinatum", "TB")
FR_NISO_forSMK(first_records, "Technomyrmex albipes", "TA")
FR_NISO_forSMK(first_records, "Wasmannia auropunctata", "WA")