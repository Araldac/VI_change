###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Compute CSI 
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
data_all_v <-args[1]
data_bioV <-args[2]
path_WorldClim <- args[3]
v_names <- args[4]
#output_df_eval <- args[5]
output_s1 <- args[5]
output_s2 <- args[6]
output_df_eval <- args[7]


#download/install packages
pkg <- c("dplyr", "magrittr","gbm", "dismo", "pROC","foreach" , "doParallel", "terra")#,,"parallel") "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################


"CSI_df_map_A"<-function(data, env_spat, s, V_csi, n_cores = 10, s_n=1){
  
  terraOptions(progress = 0, threads = 8)
  
  # -------------------------------------------------
  # SANITY CHECKS
  # -------------------------------------------------
  
  # 1. V_csi must exist in the data
  if (!all(V_csi %in% colnames(data))) {
    stop("ERROR: Some predictor names in V_csi do not exist in `data`.")
  }
  
  # 2. Raster names must match data variable names
  if (!all(names(env_spat) %in% colnames(data))) {
    warning("Some raster layer names do not exist in data columns.\n",
            "terra::predict will try to match by name; ensure alignment.")
  }
  
  # -------------------------------------------------
  # PARALLEL CLUSTER FOR MODEL TRAINING ONLY
  # -------------------------------------------------
  
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  stack_pred <- NULL
  df_eval <- NULL

  message("\n Starting PARALLEL model training...\n")
  
  start_time <- Sys.time()

  models_and_Eval <- foreach(i = 1:10,  ##change if just want to do PAn=1
                             .packages = c("dplyr","magrittr","dismo","gbm","pROC")) %dopar% {

  seed <- 12*i*s_n
  set.seed(seed)
    
    #in case it's not clean
    data_clean <- data %>%
         dplyr::filter(
           S == s,
           is.na(PA_N) | PA_N == i
         )
  
  # Train/test split
  ind=sample(2,nrow(data_clean),replace=TRUE,prob=c(0.80,0.20))
  
  data.training=data_clean[ind==1,]
  data.test=data_clean[ind==2,]
  
  # -----------------------
  # Train model
  # -----------------------
  
  model_i <- gbm.fixed(
    data = data.training, 
    gbm.x = V_csi, ###
    gbm.y = "Occurrence",
    family = "bernoulli", 
    tree.complexity = 8,
    learning.rate = 0.01,
    bag.fraction = 0.7,
    n.trees = 2900)
  
  
  #evaluate the CSI
  predictions <- predict.gbm(model_i, data.test[, V_csi], n.trees = model_i$nTrees, type = "response")
  auc_S2 <- auc(roc(as.factor(data.test$Occurrence), predictions))
  
  # Valores reales
  actuals <- data.test$Occurrence
  
  # Convertir probabilidades a clases (0 o 1) usando umbral 0.5
  threshold <- 0.5
  predictions_class <- ifelse(predictions >= threshold, 1, 0)
  confusion.matrix.training <- table(actuals,predictions_class)
  
  #Prepare where to store the results
  colnames_results<-c("AUC", "TP", "TN", "FP", "FN", "S", "PA_N", "Seed", "Model")
  df_results_i<-data.frame(matrix(ncol= length(colnames_results), nrow=1))
  colnames(df_results_i)<- colnames_results
  
  #Store the results for this run
  df_results_i$PA_N <- i
  df_results_i$Model <- "CSI"
  df_results_i$Seed <- seed
  df_results_i$S <- s
  df_results_i$AUC <- as.numeric(auc_S2)
  df_results_i$TP <- confusion.matrix.training[2,2]
  df_results_i$TN <- confusion.matrix.training[1,1]
  df_results_i$FP <- confusion.matrix.training[1,2]
  df_results_i$FN <- confusion.matrix.training[2,1]
  
  return(list(model=model_i, eval=df_results_i))
  
  }
  
#return(models_and_Eval)

training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
message(sprintf("  ✓ Training completed in %.1f seconds", training_time))

# -------------------------------------------------
# EXTRACT MODELS AND EVALUATION
# -------------------------------------------------
models  <- lapply(models_and_Eval, `[[`, "model")
eval_df <- dplyr::bind_rows(lapply(models_and_Eval, `[[`, "eval"))

#cat("Head of the eval \n" )
#head(eval_df)

# -------------------------------------------------
# SEQUENTIAL RASTER PREDICTION (SAFE FOR TERRA)
# -------------------------------------------------
message("\n Starting SEQUENTIAL raster predictions...\n")

suit_list <- vector("list", length(models))


#if (1 == 0){
for (i in seq_along(models)){
#for (i in 1){

  terra::terraOptions(threads = parallel::detectCores() - 1, progress = 10)

  start_time_i<- Sys.time()
  mod <- models[[i]]

  #Predict the suitability to world
  suit <- terra::predict(env_spat, mod, type = "response", na.rm = TRUE)

  names(suit) <- paste0("CSI_", i)
  suit_list[[i]] <- suit

  training_time <- as.numeric(difftime(Sys.time(), start_time_i, units = "secs"))
  message("✔ Completed raster projection: ", i, "/10" )
  message(sprintf("in %.1f minutes", training_time/60))

}

message("✔ Completed all raster projections")


unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list = ls(name = env), pos = env)
}
unregister_dopar()

message("Stacking rasters \n")
suit_stack <- terra::rast(suit_list)


# -------------------------------------------------
# RETURN OUTPUT
# -------------------------------------------------
message("Returning output ")
return(list(
  stack_pred = suit_stack,
  df_eval    = eval_df
))
}

#return(df_eval  = eval_df)

#}




################################################################################
#Code
################################################################################

#### #### load data #### ####
#load dataset and remove the PCA columns
all_v <- read.csv(data_all_v)
all_v <- all_v %>% dplyr::select(-PCA_1, -PCA_2)

#load names of climtic variables
V_csi <- strsplit(v_names, ",")[[1]]
cat("Varables selected: ", V_csi, "\n")
#V_csi <- c("bio_2",  "bio_9",  "bio_3" , "bio_1" , "bio_8" , "bio_12", "bio_15")

C_v <- read.csv(data_bioV)
C_v <- C_v %>% dplyr::select(all_of(V_csi))

#join the datasets
join_d <- cbind(all_v, C_v)


#load cliamtic raster
cat("load climatic raster \n")
bioclim<-list.files(path_WorldClim,  pattern = "^[^:]*$|[^:]*[^:]*", full.names = T)
bio<-terra::rast(bioclim)
names(bio)=sub("wc2.1_2.5m_","",names(bio))
bio_sel <- bio[[V_csi]]


##Apply the function

#output will be a list of the CSI for each PA_N and the df with the evaluation of each CSI of each S

system.time({
result_S1<- CSI_df_map_A(join_d, bio_sel, "S1", V_csi, n_cores = 10)
terra::writeRaster(result_S1[[1]], output_s1, filetype = "GTiff", overwrite = TRUE)
#raster_s1 <- terra::wrap(result_S1[[1]])
#save(raster_s1, file= output_s1)
})

system.time({
  result_S2<- CSI_df_map_A(join_d, bio_sel, "S2", V_csi, n_cores = 10, s_n=2)
  terra::writeRaster(result_S2[[1]], output_s2, filetype = "GTiff", overwrite = TRUE)
  #raster_s2 <- terra::wrap(result_S2[[1]])  
  #save(raster_s2, file= output_s2)
})


#### #### Group evaluation CSIs  #### ####
df_eval_s1 <- result_S1[[2]]
df_eval_s2 <- result_S2[[2]]

#df_eval_s1 <- result_S1
#df_eval_s2 <- result_S2

df_eval <- rbind(df_eval_s1,df_eval_s2)

head(df_eval)

cat("Output file should go here ", output_df_eval, "\n")
write.csv(df_eval, file= output_df_eval)