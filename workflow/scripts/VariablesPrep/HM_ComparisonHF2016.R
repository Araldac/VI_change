###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Compare HM rasters method with the Human Footprint map 2016
# Date: 26/11/2025
###############################################################################

## Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
pop_2016 <- args[1] 
anth_2016 <- args[2] 
HF_2016 <- args[3] 
output_2016<- args[4] 

devtools::install_github("Nowosad/popgrids")
#download/install packages
pkg <- c( "countrycode", "zoo", "dplyr", "tidyr", "purrr", "pbapply", "terra","raster", "popgrids") #, "", "", "")
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
  
  gpw_raster_normalized<-gpw_raster_normalized*10
  
  return(gpw_raster_normalized)
}

"create_HM_raster" <- function(population, gdp, anthromes, plot=FALSE){

    #### #### Anthromes #### ####

    ##change anthromes to specific value
    anth_values <- c("11", "12", "21", "22", "23", "24", "31", "32", "33", "34", "41", "42", "43", "51", "52", "53", "54", "61", "62", "63", "70")
    anth_values <- as.numeric(anth_values)
    lof_values <- c(rep(10,2), 7, 10, 7, 7, 7, 7, 4, 4, 4, 4, 0, 4, 4, 0, 2, 0, 0, 0, 0)
    anth_to_lw <- data.frame(anth_values, lof_values)

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
d_pop2016 <- raster(pop_2016)
d_ant2016 <- raster(anth_2016)
g3_2020 <- subset(gdp_grid, match("g3_2020", names(gdp_grid)))

hf_2016 <- raster(HF_2016)


#### #### create rasters #### ####
hm_2016 <- create_HM_raster(d_pop2016, g3_2020, d_ant2016)

#### #### Compare rasters #### ####
index.sample <- sample(x = which(!is.na(hm_2016@data@values)), size = 50000)
index.sample <- xyFromCell(object=hm_2016, index.sample)

in_hm <- extract(rast(hm_2016), index.sample )
in_hf <- extract(rast(hf_2016), index.sample )

correlation <- cor(in_hm, in_hf, use="complete.obs") ## 0.796935
print(paste0("The correlation between HM and HF is:", correlation))

data <- data.frame(hf=in_hf, hm=in_hm)
colnames(data) <- c("hf", "hm")

lm_rg <- lm(data=data, hf~hm) 
summary(lm_rg)# Multiple R-squared:  0.6351



#### #### save output #### ####
save(hm_2016, file=output_2016)


