###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Thinning
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
data_cleaned <- args[1]
#distributionData <- args[2]
#sf_lak <- args[3]
#sf_land <- args[4]
#sf_sea <- args[5]
#sf_unland <- args[6]
#antmaps_pol <-args[7]
raster <- args[2]
output <- args[3]



 pkg <- c("spacesXYZ", "geosphere", "parallel", "raster", "readr", "colorspace", "rgl", "ade4", "farver",  "devtools")
 lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

 pkg <- c("magrittr", "CoordinateCleaner", "countrycode", "rnaturalearth", "sf", "sp", "sfheaders", "dplyr", "tidyr", "terra")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))
# install.packages("devtools")
devtools::install_github("jmestret/GeoThinneR")
################################################################################
#Functions
################################################################################
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

#"thin_breaks"<- function(data, timestep=20 ){
#  set.seed(1)
#  list_index<-c()
#  breaks<-seq(min(data$year), 2030, by=timestep)
#  for (i in 1:length(breaks)){
#    if (i < length(breaks)){
#      d<- data[between (data$year, breaks[i], breaks[i+1]), ]
#      system.time(
#        occ_t <- thin_split(d[,c("decimallongitude", "decimallatitude")], 500, fun = "distGeo", threshold_d = 10*1000, ncores = 12)
#      )
#      list_index<-c(list_index, rownames(occ_t))
      
#    }
#  }
#  return(data[list_index,])}

"thin_period"<- function(data, S_n){
  set.seed(1)
    print (S_n)
     dataS <- data %>% filter(S == S_n)
      system.time(
        occ_t <- thin_split(dataS[,c("decimallongitude", "decimallatitude")], 500, fun = "distGeo", threshold_d = 10*1000, ncores = 12)
      )
      list_index<-rownames(occ_t)
      
  return(data[list_index,])}


  "thin_terra" <- function(data, S_n){
    set.seed(1)
    print (S_n)
     dataS <- data %>% filter(S == S_n)
      system.time(
      grid_raster_thin <- GeoThinneR ::thin_points(
        data = dataS,
        lon_col = "decimallongitude",     # Longitude column name
        lat_col = "decimallatitude",     # Latitude column name
        method = "grid", 
        raster_obj = r_a1,
        trials = 10,
        n = 1,
        all_trials = FALSE,
        seed = 123
      ))

      summary(grid_raster_thin)
      print(grid_raster_thin$retained)
      list_index <- unlist(grid_raster_thin$retained)
      print(list_index)
      print("retained)")
      head(dataS[list_index,])
      
  return(dataS[list_index,])}

################################################################################
#Code
################################################################################

## Loading occurrences of sp from GBIF in 2025
occ_cle <- read.csv(data_cleaned, sep = ",") #
#laod raster 
r_a1 <- terra::rast(raster)

head(occ_cle)
### Thinning

# Thinning of the data using code from Olivia bates and Sebatien Ollier. More information in thnning.docx

set.seed(1)
cat("Thinning S1 \n")
occ_thin1 <- thin_period(occ_cle, "S1")
occ_thin1R <- thin_terra(occ_cle, "S1")
cat("Thinning S2 \n")
occ_thin2 <- thin_period(occ_cle, "S2")
occ_thin2R <- thin_terra(occ_cle, "S2")

occ_thin20 <- rbind(occ_thin1, occ_thin2)

occ_thin20_cols <- occ_thin20 %>% dplyr::select(decimallatitude, decimallongitude, year, S, BEN, countrycode)

occ_thin20R <- rbind(occ_thin1R, occ_thin2R)
occ_thin20_colsR <- occ_thin20R %>% dplyr::select(decimallatitude, decimallongitude, year, S, BEN, countrycode)


cat("Total rows before Thinning", nrow(occ_cle), "\n")
cat("S1 Presences before Thinning: ", nrow(occ_cle[occ_cle$S == "S1", ]), "\n")
cat("S2 Presences before Thinning: ", nrow(occ_cle[occ_cle$S == "S2", ]), "\n \n")

cat("Total rows after Thinning", nrow(occ_thin20_cols), "\n")
cat("S1 Presences after Thinning: ", nrow(occ_thin20_cols[occ_thin20_cols$S == "S1", ]), "\n")
cat("S2 Presences after Thinning: ", nrow(occ_thin20_cols[occ_thin20_cols$S == "S2", ]), "\n")

cat("Total rows after Thinning raster ", nrow(occ_thin1R)+nrow(occ_thin2R), "\n")
cat("S1 Presences after Thinning raster: ", nrow(occ_thin1R[occ_thin1R$S == "S1", ]), "\n")
cat("S2 Presences after Thinning raster: ", nrow(occ_thin2R[occ_thin2R$S == "S2", ]), "\n")

write.csv(occ_thin20_colsR, output)

