###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Trade Data add the trade information for each observation
# Date: 03/12/2025
###############################################################################

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
sumimport <- args[2]
output <- args[3]


################################################################################
#Functions
################################################################################

"assign_trade"<-function(data, sum_import){
  country_data<-unique(data$ISO)

  for (c in country_data){
    if (c %in% sum_import$ISO3){
    data$trade[data$ISO == c & data$S =="S1"]<-sum_import$Sum[sum_import$year == 1970 & sum_import$ISO3 ==c]
    data$trade[data$ISO == c & data$S =="S2"]<-sum_import$Sum[sum_import$year == 2014 & sum_import$ISO3 ==c]
    }
    else {
      data$trade[data$ISO == c & data$S =="S1"]<-NA
      data$trade[data$ISO == c & data$S =="S2"]<-NA 
      
    }
  }
  
  return(data)
}

################################################################################
#Code
################################################################################

#### #### Read data #### ####
LH_C_B <- read.csv(occurrences)
all_c_year_sumimport <- read.csv(sumimport)

#### #### Re-assign ISo and BEN #### #### 
# Re-assign some of the countries name
LH_C_B$ISO[LH_C_B$ISO == "TUSC"] <- "ITA"
LH_C_B$ISO[LH_C_B$ISO == "NA"] <- NA

 LH_C_B$ISO[LH_C_B$BEN== "BEN20190"]<-"IDN" #Malaku Islands as Indonesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20594"]<-"IDN" #Riau Islands as Indonesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20570"]<-"IDN" #Lesser Sunda Islands as Indonesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20568"]<-"RUS" #Kuril islands as Russian Empire USSR
 LH_C_B$ISO[LH_C_B$BEN== "BEN20560"]<-"GRC" #ionian silands as greece
 LH_C_B$ISO[LH_C_B$BEN== "BEN20561"]<-"MEX" #ISlas Marias as mexico
 LH_C_B$ISO[LH_C_B$BEN== "BEN20345"]<-"PNG" #Bismark archipelago as papua new guinea
 LH_C_B$ISO[LH_C_B$BEN== "BEN20607"]<-"PNG" #Trobriand islands aS papua new guinea
 LH_C_B$ISO[LH_C_B$BEN== "BEN20569"]<-"LA" #Lesser Antilles islands
 LH_C_B$ISO[LH_C_B$BEN== "BEN20126"]<- NA #Europa Island (uninhabited )


# check the data
cat("For S1 :\n")
cat("       -Presences", sum(nrow(LH_C_B[LH_C_B$S == "S1" & LH_C_B$Occurrence == 1, ])), "\n")
cat("       -Absences", sum(nrow(LH_C_B[LH_C_B$S == "S1" & LH_C_B$Occurrence == 0, ])), "\n" )
cat("\n")
cat("For S2 :\n")
cat("       -Presences", sum(nrow(LH_C_B[LH_C_B$S == "S2" & LH_C_B$Occurrence == 1, ])), "\n")
cat("       -Absences", sum(nrow(LH_C_B[LH_C_B$S == "S2" & LH_C_B$Occurrence == 0, ])), "\n" )


# apply trade information
LH_C_B_PP <- assign_trade(LH_C_B, all_c_year_sumimport)


write.csv(LH_C_B_PP,  output)