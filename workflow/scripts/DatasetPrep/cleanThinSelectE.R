###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Preparation of the variables for snakemake
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
occurrencesGBIF <- args[1]
distributionData <- args[2]
sf_lak <- args[3]
sf_land <- args[4]
sf_sea <- args[5]
sf_unland <- args[6]
antmaps_pol <-args[7]
output <- args[8]

 pkg <- c("spacesXYZ", "geosphere", "parallel", "raster", "readr", "colorspace", "rgl", "ade4", "farver")
 lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

 pkg <- c("magrittr", "CoordinateCleaner", "countrycode", "rnaturalearth", "sf", "sp", "sfheaders", "dplyr", "tidyr")
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

####	#### Thinning	####	#### 
#"splitcloud: see thinning.docx 
#thin split : see thinning.docx
#Thin breaks: apply thinning every X years to avoid thinning different temporal data


"thin_algo" <- function(cloud, mat = NULL, method = "euclidean", threshold, rep = 1, print = FALSE){
  #--> derived from thin.algorithm from spThin
  #--> internal function to run spatial thinning algorithm rep times (by default rep = 1) on a cloud named cloud (not limited to 2D) with a matrix of distance (not limited to euclidean or geographical distance)
  #--> to be used on not to big cloud - not too big means that we can calculated and stored the matrix of distance mat
  
  #--> cloud : coordinates of points cloud (not restricted to 2D)
  #--> mat : a matrix of distance between points. If NULL, euclidean distances are calculated
  #--> method : when mat = NULL, dist function is used with method argument. By default, it uses euclidean distance
  #--> threshold : distance threshold for thinning
  #--> rep : repeat the algorithm rep times
  
  # Check if cloud has only one line
  if(is.vector(cloud)){
    cloud <- t(cloud)
  }
  
  # Prepare results
  reduced <- vector("list", rep)
  
  # Define matrix of distance
  if(is.null(mat)){
    # By default, create euclidean distance function
    mat <- as.matrix(dist(cloud, method = method))
  }
  
  # Prepare algorithm
  DistMat.save <- (mat < threshold)
  diag(DistMat.save) <- FALSE
  DistMat.save[is.na(DistMat.save)] <- FALSE
  SumVec.save <- rowSums(DistMat.save)
  df.keep.save <- rep(TRUE, length(SumVec.save))
  
  for (Rep in seq_len(rep)) {
    DistMat <- DistMat.save
    SumVec <- SumVec.save
    df.keep <- df.keep.save
    k <- 0
    while (any(DistMat) && sum(df.keep) > 1) {
      RemoveRec <- which(SumVec == max(SumVec))
      if (length(RemoveRec) > 1) {
        RemoveRec <- sample(RemoveRec, 1)
      }
      SumVec <- SumVec - DistMat[, RemoveRec]
      SumVec[RemoveRec] <- 0L
      DistMat[RemoveRec, ] <- FALSE
      DistMat[, RemoveRec] <- FALSE
      df.keep[RemoveRec] <- FALSE
      k <- k + 1
      if(print){
        print(k)
      }
    }
    rec.df <- cloud[df.keep, , drop = FALSE]
    colnames(rec.df) <- colnames(cloud)
    reduced[[Rep]] <- rec.df
    # print(Rep)
  }
  
  reduced.rec.order <- unlist(lapply(reduced, nrow))
  reduced.rec.order <- order(reduced.rec.order, decreasing = TRUE)
  reduced <- reduced[reduced.rec.order]
  return(reduced)
}
"split_cloud" <- function(cloud, threshold, option = 1){
  #--> split a cloud of points according to a threshold around median
  
  #--> cloud : coordinates of points cloud (not restricted to 2D)
  #--> threshold : threshold for splitting (maximum number of points by subclouds)
  #--> method : 1 for splitting recursively around medians, axe by axe ; 2 for splitting recursively in 2**D parts around the median centroid
  
  nc <- ncol(cloud)
  nr <- nrow(cloud)
  test <- (nr >= threshold)
  w <- 0
  subcloud <- list(cloud)
  
  if(option == 1){
    while(any(test)){
      tosplit <- subcloud[test]
      subcloud <- subcloud[!test]
      local <- list()
      d <- 1
      while((d <= nc) & (any(test))){
        for(i in 1:length(tosplit)){
          me <- median(tosplit[[i]][,d])
          t <- (tosplit[[i]][,d] > me)
          local[[i*2 - 1]] <- tosplit[[i]][t,]
          local[[i*2]] <- tosplit[[i]][!t,]
        }
        tosplit <- local	
        d <- d + 1	
        subcloud <- c(subcloud, tosplit)
        nr <- unlist(lapply(subcloud, nrow))
        test <- (nr >= threshold)
        tosplit <- subcloud[test]
        subcloud <- subcloud[!test]
      }
      subcloud <- c(subcloud, tosplit)
      nr <- unlist(lapply(subcloud, nrow))
      test <- (nr >= threshold)
      w <- w + 1
      # print(w)
    }
  }
  
  if(option == 2){
    while(any(test)){
      tosplit <- subcloud[test]
      subcloud <- subcloud[!test]
      
      for(i in 1:length(tosplit)){
        me <- apply(tosplit[[i]], 2, median)
        #--> could use means or half of ranges
        
        for(j in 1:nc){
          assign(paste0("t", j), (tosplit[[i]][,j] < me[j]))
          assign(paste0("t_", j), !(tosplit[[i]][,j] < me[j]))
          assign(paste0("fac", j), as.factor(c(paste0("t", j), paste0("t_", j))))
        }
        
        exp <- paste(paste0("fac", 1:nc), collapse = ":")
        t <- levels(eval(parse(text = exp)))
        t <- strsplit(t, ":")
        
        for(k in 1:length(t)){
          exp <- paste0("get(t[[", k, "]][", 1:nc, "])")
          exp <- paste(exp, collapse = "&")
          t[[k]] <- eval(parse(text = exp))
          t[[k]] <- tosplit[[i]][t[[k]],]
        }
        
        subcloud <- c(subcloud, t)
      }
      
      nr <- unlist(lapply(subcloud, nrow))
      test <- (nr >= threshold)
      w <- w + 1
      # print(w)
    }
  }
  
  subcloud <- subcloud[nr > 0]
  return(subcloud)
} 

"thin_split" <- function(cloud, threshold_s = Inf, fun = "dist", method = "euclidean", threshold_d, ncores = 1){
  #--> thinning (rep = 1) big clouds of points by splitting it in many subclouds according to threshold_s
  #--> allows to use other distances than the euclidean ones (actually, spacesXYZ::DeltaE for color, geosphere::distGeo for geographic distance
  
  #--> cloud : points cloud to split and thin (not restricted to 2D)
  #--> threshold_s : threshold for splitting (by default, Inf, no splitting)
  #--> fun : distance function, by default stats::dist
  #--> method : distance method associated to fun function (by default, euclidean function associated to dist but could be metric associated to spacesXYZ::DeltaE() or farver:compare_colour() [faster than DeltaE, do not need parallelization])
  #--> threshold_d : threshold distance for thinning
  #--> ncores : if you want to parrallelize your code on more cores
  
  ## Dimensions
  nc <- ncol(cloud)
  nr <- nrow(cloud)
  
  ## Memory limit size
  #--> help(Memory)
  mem_max <- Inf	#--> mem_max <- memory.size(max = NA) : deprecated
  if(!is.infinite(threshold_s)){
    mat_size <- unclass(object.size(matrix(1, threshold_s, threshold_s))) / 10**6
    if(mat_size > (mem_max / ncores)){
      print("Your threshold is too big regarding your memory, you should choose a lower one")
      break
    }	
  }	else{
    possibleError <- tryCatch(
      mat_size <- format(object.size(matrix(NA, nr, nr)), "Mb"
      ), error = function(e) e)
    if(inherits(possibleError, "error")){
      print("You should define a threshold, your cloud is too big to store a distance matrix")
    }			
  }
  
  ## Split the cloud in subclouds according to threshold
  subclouds <- split_cloud(cloud, threshold_s)
  
  ## Spatial thining on subclouds
  k <- 1
  nsub <- length(subclouds)
  if(nsub == 1){
    print("Your cloud has not been splitted")
  }	else{
    print(paste0("Your cloud has been splitted in ", nsub, " subclouds"))
  }
  res <- NULL
  
  repeat{
    # Calculate matrix of distances until memory size of submats is reached
    if(fun == "dist"){
      submats <- vector("list", nsub)
      k <- s <- nsub + 1
    }	else{
      gc()
      submats <- as.list(NULL)
      s <- 1
      mem <- unclass(object.size(submats)) / 10**6
      
      while((mem < (mem_max * 4 / 5 / ncores)) & (k <= nsub)){
        subcloud <- subclouds[[k]]
        cl <- makeCluster(ncores)
        
        if(fun == "DeltaE"){
          clusterExport(cl, varlist = c("DeltaE", "method"), envir = environment())
          submats[[s]] <- parApply(cl, subcloud, 1, function(x) DeltaE(x, subcloud, metric = method))
          
        }
        
        if(fun == "compare_colour"){
          submats[[s]] <- compare_colour(subcloud, subcloud, "luv", "luv", method = method)
          
        }
        
        if(fun == "distGeo"){
          clusterExport(cl, varlist = c("distGeo"))
          submats[[s]] <- parApply(cl, subcloud, 1, function(x) distGeo(x, subcloud))
        }
        
        stopCluster(cl)
        mem <- unclass(object.size(submats)) / 10**6
        # print(format(mem, "Mb"))
        k <- k + 1
        print(k)
        s <- s + 1
      }
    }
    
    # Execute spatial thinning
    ij <- mapply(function(x,y) c(x,y), as.list((k-(s-1)):(k-1)), as.list(1:(s-1)), SIMPLIFY = F)
    cl <- makeCluster(ncores)
    clusterExport(cl, varlist = c("thin_algo", "subclouds", "submats", "method", "threshold_d"), envir = environment())
    if(fun == "dist"){
      subres <- parLapply(cl, ij, function(x) thin_algo(subclouds[[x[1]]], submats[[x[2]]], method = method, threshold = threshold_d)[[1]])
    }	else{
      subres <- parLapply(cl, ij, function(x) thin_algo(subclouds[[x[1]]], submats[[x[2]]], threshold = threshold_d)[[1]])
    }
    stopCluster(cl)
    
    res <- rbind(res, do.call(rbind, subres))
    # save(k, res, file = "test.RData")
    
    if(k >= nsub){
      break
    }
  }
  return(res)
}

"thin_breaks"<- function(data, timestep=20 ){
  set.seed(1)
  list_index<-c()
  breaks<-seq(min(data$year), 2030, by=timestep)
  for (i in 1:length(breaks)){
    if (i < length(breaks)){
      d<- data[between (data$year, breaks[i], breaks[i+1]), ]
      system.time(
        occ_t <- thin_split(d[,c("decimallongitude", "decimallatitude")], 500, fun = "distGeo", threshold_d = 10*1000, ncores = 12)
      )
      list_index<-c(list_index, rownames(occ_t))
      
    }
  }
  return(data[list_index,])}


####	#### Filter E only ####
#needs to be modified bc it's specific to linepithema humile
coords2admin2 <- function(data, admin = antmaps_pol) {

  points <- data %>% dplyr::select(decimallongitude, decimallatitude)
  
  # Convert to spatial points
  pointsSP <- SpatialPoints(points, proj4string = CRS("EPSG:4326"))
  pointsSF <- st_as_sf(pointsSP)
  
  # Ensure CRS is set
  if (is.na(st_crs(admin))) {
    st_crs(admin) <- 4326
  }
  
  # Try intersection first
  indices <- st_intersects(pointsSF, admin, sparse = TRUE)
  
  # Fallback: assign nearest polygon if no intersection
  result <- sapply(seq_along(indices), function(i) {
    if (length(indices[[i]]) == 0) {
      # Compute distances to all polygons
      distances <- st_distance(pointsSF[i, ], admin)
      nearest_idx <- which.min(distances)
      return(nearest_idx)
    } else {
      return(indices[[i]][1])
    }
  })
  
  data$BEN <- admin$BEN[result]
  data$ISO <- admin$ISO[result]
  
  return(data)
}

"select_E"<-function(data) {
  assign('sp_disData', get(load(distributionData)))
  info_sp <- antmaps_dis[["Linepithema humile"]]
  exotic_ben <- info_sp$gid[info_sp$category == "E"]
  data_clean<-data[data$BEN %in% exotic_ben,]
  
  return(data_clean)
}




################################################################################
#Code
################################################################################

## Loading occurrences of sp from GBIF in 2025
sp <- read.csv(occurrencesGBIF, sep = "\t") #

# rename some columns
sp <- sp %>%
  rename(
    decimallatitude = decimalLatitude,
    decimallongitude = decimalLongitude,
    countrycode = countryCode
  )


load(distributionData)

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


### Filter only E ####----------------------------------------
#Filter occurences that are Exotic (remove indoor and native)
load(antmaps_pol)

occ_ben<-coords2admin2(occ_thin20)

occ_E<-select_E(occ_ben)

occ_E_cols<- occ_E %>% dplyr::select(decimallatitude, decimallongitude, year, S, BEN, countrycode)

write.csv(occ_E_cols, output)
