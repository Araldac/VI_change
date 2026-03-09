
###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Historical data: convert to current countries and interpolates NA
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
historicalTrade <- args[1]
firstRecord_Countries <- args[2]
firstRecord_date <- args[3]
nativeCountries <- args[4]
output <- args[5] 

library(devtools)
#install.packages("MazamaSpatialUtils", repos = "http://cran.us.r-project.org")
#devtools::install_github('mazamascience/MazamaSpatialUtils') #, build_vignettes=TRUE)
pkg <- c("countrycode", "dplyr", "stringr", "pbapply", "tidyr")#, "tidyverse")#, "sf", "dplyr", "tidyr", "purrr", "pbapply", "zoo")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Functions
################################################################################

#### #### Sum imports from countries where the species is present  #### ####

sum_import<-function(data, iso3_first_records, ISOcountry, min_year=max(firstRecord_date,1827), max_year=2014){
  year_sumimport<-data.frame(ISO3= character(), year=numeric(), Sum=numeric(),stringsAsFactors = FALSE )

  c_w_lh<-c() 
  
  for (y in min_year:max_year){
    
    if (y %in% iso3_first_records$FirstRecord) {
      new_iso3 <- iso3_first_records$iso3[iso3_first_records$FirstRecord == y]
      c_w_lh <- c(c_w_lh, new_iso3)
      #print(paste("Year:", y, "Added:", paste(new_iso3, collapse = ", ")))
    }
    
  
    #all_t<-sum(data %>% filter(iso3_origin %in% c_w_lh, iso3_destination==ISOcountry, !is.na(FLOW_split)) %>% dplyr::select(FLOW_split))
   
    all_t <- sum(data %>%
                filter(iso3_origin %in% c_w_lh,
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
      cum_t<-sum(year_sumimport$Sum[year_sumimport$ISO3==ISOcountry & year_sumimport$year== y-1 ], all_t)
      
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
first_records <- read.csv(firstRecord_Countries, sep=",")
head(first_records)
HistTrade <- read.csv(historicalTrade)

if( !is.null(first_records$Region) == TRUE  ){
first_records$ISO2 <- first_records$Region
}
#### #### prepare first occurrences document #### ####

#If Namibia is present count it as NA not <NA> 
#first_records$ISO2[is.na(first_records$ISO2)] <- "NA"
first_records <- first_records %>% tidyr::drop_na(ISO2)

cat("select only firsts records in mainland not in islands \n ")
#select only firsts records in mainland not in islands
iso2_first_records<-first_records[nchar(first_records$ISO2) == 2,]
iso2_first_records$iso3 <- countrycode::countrycode(iso2_first_records$ISO2, origin="iso2c", destination="iso3c")
#iso2_first_records$iso3<-iso2ToIso3(iso2_first_records$ISO2) # add iso 2 columns
#iso3_first_records<-iso2_first_records[3:4]#just select year and country code
iso3_first_records<-iso2_first_records %>% select(iso3, FirstRecord)

head(iso3_first_records)

nativeCountries <- strsplit(nativeCountries, " +")
nativeCountries<-unlist(nativeCountries)

cat("Native countries : ", nativeCountries, " first date : ", firstRecord_date, "\n")
#Set Native area as frist record as the first record date  
for (i in 1:length(nativeCountries)){
  print(c(firstRecord_date, nativeCountries[i]))
    iso3_first_records[nrow(iso3_first_records) + 1, ] <- c(firstRecord_date, nativeCountries[i])

}



cat(" Sum over each year when the species is present in that country \n")
#### #### Sum over each year when the species is present in that country #### ####

#get unique names
u_o<-unique(HistTrade$iso3_origin)

#1st records as list 
list1rec<-as.list(first_records)

#Run function 
# 100% elapsed=51m 57s
all_c_year_sumimport<-list()
print("starting function")
pboptions(type = "timer")  # or "txt"
all_c_year_sumimport <- pbapply::pblapply(1:length(u_o), function(i) {
 sum_import(HistTrade, iso3_first_records, as.character(u_o[i]))
})

#merge the lists
all_c_year_sumimport <- dplyr::bind_rows(all_c_year_sumimport)

head(all_c_year_sumimport)

write.csv(all_c_year_sumimport, file= output)