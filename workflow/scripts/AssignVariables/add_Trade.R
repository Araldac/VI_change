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

 # Other island regions with no country code in Antmaps (ISO "NA"), assigned to their country as above.
 # Without this they get no trade value, and since most of their points are presences,
 # the missing value itself would become a signal for presence
 LH_C_B$ISO[LH_C_B$BEN== "BEN20555"]<-"ECU" #Galapagos Islands as Ecuador
 LH_C_B$ISO[LH_C_B$BEN== "BEN20011"]<-"IND" #Andaman and Nicobar Islands as India
 LH_C_B$ISO[LH_C_B$BEN== "BEN20586"]<-"JPN" #Ogasawara Islands as Japan
 LH_C_B$ISO[LH_C_B$BEN== "BEN20587"]<-"JPN" #Oki Islands as Japan
 LH_C_B$ISO[LH_C_B$BEN== "BEN20597"]<-"JPN" #Satsunan Islands as Japan
 LH_C_B$ISO[LH_C_B$BEN== "BEN20606"]<-"AUS" #Torres Strait Islands as Australia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20588"]<-"ITA" #Pantelleria Island as Italy
 LH_C_B$ISO[LH_C_B$BEN== "BEN20589"]<-"ITA" #Pelagie Islands as Italy
 LH_C_B$ISO[LH_C_B$BEN== "BEN20591"]<-"ITA" #Pontine Islands as Italy
 LH_C_B$ISO[LH_C_B$BEN== "BEN20610"]<-"ITA" #Tuscan Archipelago as Italy
 LH_C_B$ISO[LH_C_B$BEN== "BEN20556"]<-"TUN" #Galite Islands as Tunisia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20553"]<-"IDN" #Enggano Island as Indonesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20567"]<-"IDN" #Krakatau Islands as Indonesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20605"]<-"IDN" #Thousand Islands (north of Jakarta) as Indonesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20573"]<-"PNG" #Louisiade Archipelago as Papua New Guinea
 LH_C_B$ISO[LH_C_B$BEN== "BEN20546"]<-"RUS" #Commander Islands as Russia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20593"]<-"MEX" #Revillagigedo Islands as Mexico
 LH_C_B$ISO[LH_C_B$BEN== "BEN20571"]<-"KIR" #Line Islands as Kiribati
 LH_C_B$ISO[LH_C_B$BEN== "BEN20590"]<-"KIR" #Phoenix Islands as Kiribati
 LH_C_B$ISO[LH_C_B$BEN== "BEN20536"]<-"PYF" #Austral Islands as French Polynesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20557"]<-"PYF" #Gambier Islands as French Polynesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20577"]<-"PYF" #Marquesas Islands as French Polynesia
 LH_C_B$ISO[LH_C_B$BEN== "BEN20609"]<-"PYF" #Tuamotu Islands as French Polynesia
 # Mascarene Islands: Reunion lies west of 56.5 E, Mauritius and Rodrigues east of it
 # (as for French Guiana, the trade data have no separate values for Reunion after 1996)
 LH_C_B$ISO[LH_C_B$BEN== "BEN20578" & LH_C_B$decimallongitude <  56.5]<-"REU" #Reunion
 LH_C_B$ISO[LH_C_B$BEN== "BEN20578" & LH_C_B$decimallongitude >= 56.5]<-"MUS" #Mauritius and Rodrigues
 # No usable trade data: left without a trade value, like Europa Island
 LH_C_B$ISO[LH_C_B$BEN== "BEN20273"]<- NA #Micronesia (Yap): almost no FSM data in the trade data
 LH_C_B$ISO[LH_C_B$BEN== "BEN20542"]<- NA #Caroline Islands: almost no FSM data in the trade data
 LH_C_B$ISO[LH_C_B$BEN== "BEN20543"]<- NA #Chagos Archipelago: no trade data
 LH_C_B$ISO[LH_C_B$BEN== "BEN20599"]<- NA #Senkaku Islands (uninhabited)


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