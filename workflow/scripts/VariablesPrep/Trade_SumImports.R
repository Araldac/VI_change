
###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Sum imports from countries with species present
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
historicalTrade <- args[1]
firstRecord_Countries <- args[2]
firstRecord_date <- args[3] # not used any more: native countries count from first_trade_year
nativeCountries <- args[4]
output <- args[5] 

first_trade_year <- 1827 # trade data are available from 1827

library(devtools)
#install.packages("MazamaSpatialUtils", repos = "http://cran.us.r-project.org")
#devtools::install_github('mazamascience/MazamaSpatialUtils') #, build_vignettes=TRUE)
pkg <- c("countrycode", "dplyr", "stringr", "pbapply", "tidyr")#, "tidyverse")#, "sf", "dplyr", "tidyr", "purrr", "pbapply", "zoo")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Functions
################################################################################

#### #### Sum imports from countries where the species is present  #### ####

sum_import<-function(data, iso3_first_records, ISOcountry, min_year=first_trade_year, max_year=2014){
  year_sumimport <- data.frame(ISO3= character(), year=numeric(), Sum=numeric(),stringsAsFactors = FALSE )

  #countries with the species present: start with the native range and all records
  #dated up to min_year (records before the first year of trade data count from that year)
  c_w_lh <- iso3_first_records$iso3[as.numeric(iso3_first_records$FirstRecord) <= min_year]
  
  for (y in min_year:max_year){
    
    if (y > min_year && y %in% iso3_first_records$FirstRecord) {
      new_iso3 <- iso3_first_records$iso3[iso3_first_records$FirstRecord == y]
      c_w_lh <- c(c_w_lh, new_iso3)
      #print(paste("Year:", y, "Added:", paste(new_iso3, collapse = ", ")))
    }
    
  
 
    # imports of year y only, from countries where the species is present
    all_t <- sum(data %>%
                filter(year == y,
                      iso3_origin %in% c_w_lh,
                      iso3_destination == ISOcountry,
                      !is.na(FLOW_split)) %>%
                pull(FLOW_split),
              na.rm = TRUE)

    if(y == min_year){
      # Create the new row as a data frame
      new_row <- data.frame(
        ISO3 = ISOcountry,
        year = as.numeric(y),
        Sum = as.numeric(all_t),
        stringsAsFactors = FALSE
      )
      
      # Append it to the existing data frame
      year_sumimport <- rbind(year_sumimport, new_row)
    }
    else{
      cum_t <- sum(year_sumimport$Sum[year_sumimport$ISO3==ISOcountry & year_sumimport$year== y-1 ], all_t)
      
      # Create the new row as a data frame
      new_row <- data.frame(
        ISO3 = ISOcountry,
        year = as.numeric(y),
        Sum = as.numeric(cum_t),
        stringsAsFactors = FALSE
      )
      
      # Append it to the existing data frame
      year_sumimport <- rbind(year_sumimport, new_row)
      
    }
  }
  
  return(year_sumimport)
  
  }


################################################################################
#Code
################################################################################


#### #### load dataset #### ####
cat("loading datasets \n")
first_records <- read.csv(firstRecord_Countries, sep=",", na.strings = "") # keep Namibia's ISO2 code "NA"
head(first_records)
HistTrade <- read.csv(historicalTrade)

if( !is.null(first_records$Region) == TRUE  ){
first_records$ISO2 <- first_records$Region
}
#### #### prepare first occurrences document #### ####

#Namibia ("NA") is kept as text by na.strings = "" above; only truly missing codes are dropped
first_records <- first_records %>% tidyr::drop_na(ISO2)

cat("select only firsts records in mainland not in islands \n ")
#select only firsts records in mainland not in islands
iso2_first_records<-first_records[nchar(first_records$ISO2) == 2,]
iso2_first_records$iso3 <- countrycode::countrycode(iso2_first_records$ISO2, origin="iso2c", destination="iso3c")
#The trade table merges these islands into "LA" (Lesser Antilles, as in Antmaps; see
#Trade_HistoricalInterpolated.R), so a first record on any of them makes "LA" a source
lesser_antilles <- c("BRB", "GRD", "VCT", "LCA", "DMA", "KNA", "ATG", "MTQ", "GLP")
iso2_first_records$iso3[iso2_first_records$iso3 %in% lesser_antilles] <- "LA"
#iso2_first_records$iso3<-iso2ToIso3(iso2_first_records$ISO2) # add iso 2 columns
#iso3_first_records<-iso2_first_records[3:4]#just select year and country code
iso3_first_records <- iso2_first_records %>% select(iso3, FirstRecord)

head(iso3_first_records)

nativeCountries <- strsplit(nativeCountries, " +")
nativeCountries<-unlist(nativeCountries)

cat("Native countries : ", nativeCountries, " counted from : ", first_trade_year, "\n")
#Native range counts from the first year of trade data, for all species
for (i in 1:length(nativeCountries)){
  print(c(first_trade_year, nativeCountries[i]))
  iso3_first_records[nrow(iso3_first_records) + 1, ] <- c(nativeCountries[i], first_trade_year)

}



cat(" Sum over each year when the species is present in that country \n")
#### #### Sum over each year when the species is present in that country #### ####

#get unique names
u_o <- unique(HistTrade$iso3_origin)

#1st records as list 
list1rec <- as.list(first_records)

#Run function 
# 100% elapsed=51m 57s
all_c_year_sumimport <- list()
print("starting function")
pboptions(type = "timer")  # or "txt"
all_c_year_sumimport <- pbapply::pblapply(1:length(u_o), function(i) {
 sum_import(HistTrade, iso3_first_records, as.character(u_o[i]))
})

#merge the lists
all_c_year_sumimport <- dplyr::bind_rows(all_c_year_sumimport)

head(all_c_year_sumimport)

write.csv(all_c_year_sumimport, file= output)