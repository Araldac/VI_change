###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Preparation of the variables for snakemake
# Date: 26/11/2025
###############################################################################


## Select input and output data ####

# setting name and path for the species
input<-args[1]
functionDataprep<-args[2]
disData<-args[3]
output<-args[4]

# Loading occurrences of sp from GBIF in 2025
sp <- read.csv(input, sep = "\t") #

# rename some columns
sp <- sp %>%
  rename(
    decimallatitude = decimalLatitude,
    decimallongitude = decimalLongitude,
    countrycode = countryCode
  )

source(functionDataprep)
load(disData)

### Cleaning------------------------------------------------------------------

# We are going to clean the data using Olivia bates and Sebatien Ollier to
# clean data removing Unvalid and duplicated, occurences, occurrences in lakes,
# sea etc. Information in cleaning.docx

res <- occ_cleaning(sp, details = FALSE, lon = "decimallongitude", lat = "decimallatitude", sea = FALSE, lak = FALSE, map = FALSE)
occ_cle <- sp[res, ]
occ_cle <- occ_cle %>% drop_na(year) # remove data without a year
occ_cle$S[occ_cle$year <= 1970] <- "S1" # assign the dataset 1 or 2 depending on year
occ_cle$S[occ_cle$year > 1970] <- "S2"
occ_cle$S <- as.factor(occ_cle$S)
#table(occ_cle$S)

### Thinning--------------------------------------------------------------------

# Thinning of the data using code from Olivia bates and Sebatien Ollier. Using a
# new function which applies thinning for data every 20 years. Which allows to
# account for spatial autocorrelation of observation while keeping the temporal
# dimension. More information in thnning.docx

occ_thin20 <- thin_breaks(occ_cle, 20)

#table(r$S) # new 197 1742 vs old w same method #195 1747

### Filter only E ####----------------------------------------
#Filter occurences that are Exotic (remove indoor and native)
occ_ben<-coords2admin2(occ_cle_thin20)

occ_E<-select_E(occ_ben)
#head(occ_E)
occ_E_cols<- occ_E %>% dplyr::select(decimallatitude, decimallongitude, year, S, BEN, countrycode)

#nrow(occ_E_cols[occ_E_cols$S == "S1",]) # 178
#nrow(occ_E_cols[occ_E_cols$S == "S2",]) # 1620

save(occ_E_cols, file = output)
