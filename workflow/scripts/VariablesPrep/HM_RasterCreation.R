###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Create HM raster with GDP, Pop desnity and anthromes
# Date: 26/11/2025
###############################################################################

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
pop_1970 <- args[1] 
pop_2025 <- args[2] 
anth_1970 <- args[3] 
anth_2025 <- args[4] 
output_1970 <- args[5] 
output_2025<- args[6] 

devtools::install_github("Nowosad/popgrids")
#download/install packages
pkg <- c( "countrycode", "zoo", "dplyr", "tidyr", "purrr", "pbapply", "terra","raster", "popgrids", "sp", "scales", "RColorBrewer")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

#### #### Function to rearrange population density using the metjods from HP #### ####

"rearrange_popdensity"<-function(raster_data){
  values<-raster::getValues(raster_data)
  # Identify the 99.9th percentile threshold
  cutoff <- quantile(values, 0.999, na.rm = TRUE)
  
  # Cap values above the 99.9th percentile
  gpw_values_capped <- ifelse(values > cutoff, cutoff, values)
  
  #Log-transform the capped values using log(X + 1)
  gpw_log_transformed <- log1p(gpw_values_capped)
  
  #Max-normalize the log-transformed values
  gpw_normalized <- gpw_log_transformed / max(gpw_log_transformed, na.rm = TRUE)
  
  #Create a new raster with the normalized values
  gpw_raster_normalized <- terra::setValues(raster_data, gpw_normalized)
  
  #transform to 10
  gpw_raster_normalized <- gpw_raster_normalized*10
  
  return(gpw_raster_normalized)
}

"create_HM_raster" <- function(population, gdp, anthromes, plot=FALSE){

#### #### Anthromes #### ####
cat("change anthromes  \n")

##change anthromes to specific value
anth_values <- c("11", "12", "21", "22", "23", "24", "31", "32", "33", "34", "41", "42", "43", "51", "52", "53", "54", "61", "62", "63", "70")
anth_values <- as.numeric(anth_values)
lof_values <- c(rep(10,2), 7, 10, 7, 7, 7, 7, 4, 4, 4, 4, 0, 4, 4, 0, 2, 0, 0, 0, 0)
anth_to_lw <- data.frame(anth_values, lof_values)

cat("substitue values   \n")
#substitue values 
anthromes_subs <- subs(anthromes, anth_to_lw)

#### #### Population density 
upd_pop <- rearrange_popdensity(population)

##change resolution of GDP raster to match the other two
gdp_resized <- resample(gdp, upd_pop)

#transformt them to values between 0-10
gdp_10 = calc(gdp_resized, fun=function(x, max=gdp_resized@data@max) { (x/max)*10 })

##Sum all the three values
hm <- overlay(gdp_10, upd_pop, anthromes_subs, fun=function(x,y,z){x+y+z})

if (plot== TRUE){
    my.palette <- rev(brewer.pal(n = 30, name = "Spectral"))
    par(mfrow=c(1,1))
    hm_crop <- crop(hm, extent(-180, 180, -90, 90))
    plot(hm_crop, col = my.palette, main = "Human footprint map")
}

  return(hm)
}

################################################################################
#Code
################################################################################

#### #### load data #### ####
cat("load data \n")
d_pop1970 <- raster::raster(pop_1970)
d_pop2025 <- raster::raster(pop_2025)

d_ant1970 <- raster::raster(anth_1970)
d_ant2025 <- raster::raster(anth_2025)

g_1980<-subset(gdp_grid, match("g_1980", names(gdp_grid)))
g3_2020<-subset(gdp_grid, match("g3_2020", names(gdp_grid)))


#### #### create rasters #### ####
cat("create raster \n")
hm_1970<-create_HM_raster(d_pop1970, g_1980, d_ant1970)
hm_2025<-create_HM_raster(d_pop2025, g3_2020, d_ant2025)

#### #### save output #### ####

cat("Save rasters \n")
terra::writeRaster(hm_1970, filename = output_1970, filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(hm_2025, filename = output_2025, filetype = "GTiff", overwrite = TRUE)



