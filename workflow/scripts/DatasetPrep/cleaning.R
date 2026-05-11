###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Cleaning occurrences
# Date: 26/11/2025
###############################################################################

## Load arguments and packages####

args <- commandArgs(trailingOnly = TRUE)
occurrencesE <- args[1]
sf_lak <- args[2]
sf_land <- args[3]
sf_sea <- args[4]
sf_unland <- args[5]
output <- args[6]

pkg <- c("spacesXYZ", "geosphere", "parallel", "raster", "readr", "colorspace", "rgl", "ade4", "farver", "dplyr", "tidyr")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

pkg <- c("magrittr", "CoordinateCleaner", "countrycode", "rnaturalearth", "sf", "sp", "sfheaders")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Functions
################################################################################

####	####	Cleaning ####	####

load(sf_lak)	# oject w_gadm (in fact w_gadm_41): used to find points in the sea
load(sf_land)	# object l_esri (in fact l_esri_100): used to find points in the lakes
load(sf_sea)	# object land (in fact land f naturalearth): used to draw sea and land when mapping results
load(sf_unland)	# object unland (in fact l_esri_100): used to draw lakes when mapping results

"occ_cleaning" <- function(occ, lon = "decimallongitude", lat = "decimallatitude",
                           GBIF = TRUE, occ_precision = 0.01, occ_uncertainity1 = 50*1000,
                           occ_uncertainity2 = c(301,3036,999,9999), occ_count = 0,
                           occ_status = c("PRESENT"), occ_record = c("FOSSIL_SPECIMEN"),
                           occ_year = 1700, sea = NULL, lak = NULL, map = TRUE, details = TRUE){
  ## pipeline to clean occurrences of one species - the file could come from GBIF or not
  # occ: data.frame with two columns called "decimallongitude" and "decimallatitude" for coordinates in geographical units ; if no species field, a column will be added with an arbitrary species name
  # lon: name of the longitude column
  # lat: name of latitude column
  # GBIF: FALSE when occurrences do not come from GBIF and all the arguments are facultative
  # occ_precision: threshold linked to "coordinatePrecision" field ; if the field is absent, specify NULL
  # occ_uncertainity1: threshold linked to "coordinateUncertaintyInMeters" field ; if the field is absent, specify NULL
  # occ_uncertainity2: false values in "coordinateUncertaintyInMeters" field ; if the field is absent, specify NULL
  # occ_count: threshold linked to "individualCount" field : eliminate occurences if count is equal or under threshold ; in the field is absent, specify NULL
  # occ_status: attributes kept in the "occurrenceStatus" field ; in the field is absent, specify NULL
  # occ_record: attributes kept in the "basisOfRecord" field ; in the field is absent, specify NULL
  # occ_year: threshold linked to "year" field : eliminate occurences if year is under threshold ; in the field is absent, specify NULL
  # sea: if NULL, do not check if points fall in the sea or not; if TRUE, keep points in the sea using w_gam (in fact w_gadm_41); if FALSE, remove points in the sea using w_gadm (in fact w_gadm_41)
  # lak: if NULL, do not check if points fall in lakes or not; if TRUE, keep points in lakes using l_esri (in fact l_esri_100); if FALSE, remove points in lakes using l_esri (in fact l_esri_100)
  # map: if TRUE, map deleted occurences by filter
  # details: if TRUE, return rem, kip and ind ; else return directly index (ind) of points to keep
  
  #### Prepare and check dataset
  ### occ must be a data.frame
  if(!(is.data.frame(occ) | is.matrix(occ)))
    return(NULL)
  
  if(!is.matrix(occ))
    occ <- as.data.frame(occ)
  nr <- nrow(occ)
  nam <- names(occ)	
  
  ### occ must have two columns named "decimallongitude", "decimallatitude"
  i1 <- which(names(occ) == lon)
  i2 <- which(names(occ) == lat)
  names(occ)[c(i1, i2)] <- c("decimallongitude", "decimallatitude")
  
  ### occ must have a species column with only one species name
  test <- c("species") %in% nam
  if(test){
    l <- unique(occ$species)
    if(length(l) > 1){
      print("occ must concern only one species: it is probably not the case for your data, you should check")
      species <- rep("no species name", nr)
      occ$species <- species
    }
  }	else{
    species <- rep("no species name", nr)
    occ <- cbind.data.frame(occ, species)
    nam <- c(nam, "species")
  }
  
  ### Assign facultative argument if GBIF == FALSE
  if(!GBIF)
    occ_precision <- occ_uncertainity1 <- occ_uncertainity2 <- occ_count <- occ_status <- occ_record <- occ_year <- NULL
  
  
  ### Check other column names according to argument
  ## All facultative arguments
  if(is.null(occ_uncertainity1) & is.null(occ_uncertainity2)){
    local1 <- NULL
  }	else{
    local1 <- "UNNULL"
  }
  arg <- list(occ_precision, local1, occ_count, occ_status, occ_record, occ_year)
  colnam <- c("coordinatePrecision", "coordinateUncertaintyInMeters", "individualCount", "occurrenceStatus", "basisOfRecord", "year")
  null <- unlist(lapply(arg, is.null))
  test <- colnam[!null] %in% nam
  if(any(!test)){
    txt <- paste(colnam[!null][!test], collapse = ", ")
    txt <- paste0("According to the arguments, you need to have column(s) named ", txt, "; be careful to letter case")
    stop(txt)
  }
  
  #### Unvalid and duplicated
  ### Index of points
  ind <- 1:nr
  
  ### Unvalid
  val <- CoordinateCleaner::cc_val(occ, lon = lon, lat = lat, value = "flagged", verbose = FALSE)
  ind <- ind[val]
  occ <- occ[val,]
  
  ### Duplicated
  dup <- CoordinateCleaner::cc_dupl(occ, lon = lon, lat = lat, value = "flagged", verbose = FALSE)
  ind <- ind[dup]
  occ <- occ[dup,]
  
  nr_new <- nrow(occ)
  
  #--> to be tested
  if(nr_new == 0){
    res <- NULL
    print("No more occurences after cleaning unvalid and duplicated points")
    return(res)
  }
  
  
  #### Intrinsic
  ### Zero coordinates
  nul <- suppressWarnings(cc_zero(occ, lon = lon, lat = lat, buffer = 0.001, value = "flagged", verbose = FALSE))
  
  ### Equal coordinates
  equ <- cc_equ(occ, lon = lon, lat = lat, value = "flagged", verbose = FALSE)
  
  ### Precision
  if(!is.null(occ_precision))
    pre <- (occ$coordinatePrecision < occ_precision) | (is.na(occ$coordinatePrecision))
  
  ### Uncertainity
  if(!is.null(occ_uncertainity1)){
    unc1 <- (occ$coordinateUncertaintyInMeters < occ_uncertainity1) | (is.na(occ$coordinateUncertaintyInMeters))
  }	else{
    unc1 <- rep(TRUE, nr_new)
  }
  
  if(!is.null(occ_uncertainity1)){
    unc2 <- !(occ$coordinateUncertaintyInMeters %in% occ_uncertainity2)
  }	else{
    unc2 <- rep(TRUE, nr_new)
  }
  
  if(!is.null(occ_uncertainity1) | !is.null(occ_uncertainity2))
    unc <- unc1 & unc2
  
  
  #### Extrinsic
  ### Occurrences that fall in the sea
  if(is.null(sea)){
    wat1 <- rep(TRUE, nr_new)
    names(wat1) <- row.names(occ)
  }	else{
    lonlat <- sf_point(occ[,c("decimallongitude", "decimallatitude")])
    lonlat <- lonlat$geometry
    st_crs(lonlat) <- st_crs(w_gadm)
    
    local <- suppressMessages(st_intersects(lonlat, w_gadm))
    local <- unclass(local)				
    wat1 <- (unlist(lapply(local, length)) == 0)
    names(wat1) <- row.names(occ)
    
    if(!sea)	wat1 <- !wat1		
  }
  
  ### Occurrences that fall in lakes
  if(is.null(lak)){
    wat2 <- rep(TRUE, nr_new)
    names(wat2) <- row.names(occ)
  }	else{
    lonlat <- sf_point(occ[,c("decimallongitude", "decimallatitude")])
    lonlat <- lonlat$geometry
    st_crs(lonlat) <- st_crs(l_esri)
    
    local <- suppressMessages(st_intersects(lonlat, l_esri))
    local <- unclass(local)				
    wat2 <- !(unlist(lapply(local, length)) == 0)
    names(wat2) <- row.names(occ)
    
    if(!lak)	wat2 <- !wat2		
  }
  
  ### Vincinity of country capitals: error in the code (limits <- raster::extent(dat) + buffer #--> buffer is is meters and not in lonlat: what is the meaning of the crop step? buffer has been changed in 250 (it was 100 before the 22/03/2023)
  cap <- suppressWarnings(cc_cap(occ, lon = lon, lat = lat, buffer = 250, value = "flagged", verbose = FALSE))
  
  ### Vincinity of country and province centroids
  cen <- suppressWarnings(cc_cen(occ, lon = lon, lat = lat, buffer = 250, value = "flagged", verbose = FALSE))
  
  ### Vincinity of biodiversity institutions
  ins <- suppressWarnings(cc_inst(occ, lon = lon, lat = lat, buffer = 250, value = "flagged", verbose = FALSE))
  
  ### GBIF headquarters
  suppressWarnings(gbi <- cc_gbif(occ, lon = lon, lat = lat, buffer = 250, value = "flagged", verbose = FALSE))
  
  
  #### Attributes
  ### Status
  if(!is.null(occ_count)){
    sta1 <- (occ$individualCount > occ_count) | is.na(occ$individualCount)
  }	else{
    sta1 <- rep(TRUE, nr_new)
  }
  
  if(!is.null(occ_status)){
    sta2 <- (occ$occurrenceStatus %in% occ_status) | is.na(occ$occurrenceStatus)
  }	else{
    sta2 <- rep(TRUE, nr_new)
  }
  
  if(!is.null(occ_count) | !is.null(occ_status)){
    sta <- sta1 & sta2
  }
  
  ### Record
  if(!is.null(occ_record))
    rec <- !(occ$basisOfRecord %in% occ_record) | is.na(occ$basisOfRecord)
  
  
  ### Year
  if(!is.null(occ_year))
    dat <- (occ$year >= occ_year) | is.na(occ$year)
  
  
  #### Prepare results
  kip <- cbind.data.frame(nul, equ, cap, cen, gbi, wat1, wat2, ins)
  
  if(is.null(occ_uncertainity1) & is.null(occ_uncertainity2)){
    local1 <- NULL
  }	else{
    local1 <- "UNNULL"
  }
  
  if(is.null(occ_count) & is.null(occ_status)){
    local2 <- NULL
  }	else{
    local2 <- "UNNULL"
  }
  
  arg <- list(occ_precision, local1, local2, occ_record, occ_year)
  null <- unlist(lapply(arg, is.null))
  if(any(!null)){
    colnam <- c("pre", "unc", "sta", "rec", "dat")[!null]
    txt <- paste(colnam, collapse = ", ")
    exp <- paste0("cbind.data.frame(", txt, ")")
    df <- eval(parse(text = exp))
    kip <- cbind.data.frame(kip, df)
  }
  
  rem <- apply(!kip, 2, sum)
  rem <- c(sum(!val), sum(!dup), rem)
  names(rem)[1:2] <- c("val", "dup")
  
  
  #### Map results
  if(map){
    index <- apply(!kip, 2, sum)
    index <- which(index > 0)
    nc <- length(index) + 1
    namcol <- names(index)
    par(mfrow = n2mfrow(nc))
    par(mar = c(2.5, 0.5, 2.5, 0.5))
    
    ### Map kept points
    # plot(w_gadm, col = "grey", border = "grey")
    plot(land, col = "grey", border = "grey", bg = "cyan3")
    # plot(l_esri, col = "cyan1", border = "cyan1", add = T)
    plot(unland, col = "cyan1", border = "cyan1", add = T)
    poi <- which(apply(kip, 1, sum) == ncol(kip))
    points(occ[poi,c("decimallongitude", "decimallatitude")], col = "black", pch = "+", cex = 0.1)
    
    ### Map deleted points by filter
    for(i in namcol){
      # plot(w_gadm, col = "grey", border = "grey")
      plot(land, col = "grey", border = "grey", bg = "cyan3")
      # plot(l_esri, col = "cyan1", border = "cyan1", add = T)
      plot(unland, col = "cyan1", border = "cyan1", add = T)
      points(occ[!kip[,i],c("decimallongitude", "decimallatitude")], col = "red", pch = "+", cex = 0.5)
      txt <- paste(i, rem[i], sep = " - ")
      title(txt)
    }
  }
  
  
  #### Return
  if(details){
    res <- list(rem = rem, kip = kip, ind = ind)
  }	else{
    test <- apply(!kip, 1, sum)
    index <- which(test == 0)
    res <- ind[index]
  }
  return(res)
}

################################################################################
#Code
################################################################################

## Loading occurrences of sp from GBIF in 2025
sp <- read.csv(occurrencesE, sep = ",") #

### Cleaning------------------------------------------------------------------

# We are going to clean the data using Olivia bates and Sebatien Ollier to
# clean data removing Unvalid and duplicated, occurences, occurrences in lakes,
# sea etc. Information in cleaning.docx

cat("Cleaning S1 \n")
sp_s1 <- sp %>% filter(S =="S1")
res1 <- occ_cleaning(sp_s1, details = FALSE, lon = "decimallongitude", lat = "decimallatitude", sea = FALSE, lak = FALSE, map = FALSE)

#occ_cleS1 <- sp_s1[as.vector(res1), ]
occ_cleS1 <- sp_s1[res1, , drop = FALSE]

cat("Cleaning S1 Ended ! \n Startign Cleaning S2 \n")
sp_s2 <- sp %>% filter(S =="S2")
res2 <- occ_cleaning(sp_s2, details = FALSE, lon = "decimallongitude", lat = "decimallatitude", sea = FALSE, lak = FALSE, map = FALSE)
#occ_cleS2 <- sp_s2[as.vector(res2), ]
occ_cleS2 <- sp_s2[res2, , drop = FALSE]


cat("Clenaing ended, join rows \n")

occ_cle <- rbind(occ_cleS1, occ_cleS2)


cat("Total rows before Cleaning", nrow(sp), "\n")
cat("S1 Presences before Cleaning: ", nrow(sp[sp$S == "S1", ]), "\n")
cat("S2 Presences before Cleaning: ", nrow(sp[sp$S == "S2", ]), "\n \n")

cat("Total rows after Cleaning ", nrow(occ_cle), "\n")
cat("S1 Presences after Cleanign: ", nrow(occ_cle[occ_cle$S == "S1", ]), "\n")
cat("S2 Presences after Cleaning ", nrow(occ_cle[occ_cle$S == "S2", ]), "\n")



write.csv(occ_cle, output)
