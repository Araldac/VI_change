###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Compute CSI 
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
data_all_v <-args[1]
#data_bioV <-args[2]
path_WorldClim <- args[2]
v_names <- args[3]
#output_df_eval <- args[5]
output_s1 <- args[4]
output_s2 <- args[5]
output_df_eval <- args[6]


#download/install packages
pkg <- c("dplyr", "magrittr","gbm", "dismo", "pROC","foreach" , "doParallel", "terra")#,,"parallel") "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))


################################################################################
#Code
################################################################################

CSI_df_map_A <- function(data, env_spat, s, V_csi, n_cores = 10, s_n = 1, n_replicates = 1) {
  
  terraOptions(progress = 0, threads = 8)
  
  # -------------------------------------------------
  # SANITY CHECKS
  # -------------------------------------------------
  
  if (!all(V_csi %in% colnames(data))) {
    stop("ERROR: Some predictor names in V_csi do not exist in `data`.")
  }
  
  if (!all(names(env_spat) %in% colnames(data))) {
    warning("Some raster layer names do not exist in data columns.\n",
            "terra::predict will try to match by name; ensure alignment.")
  }
  
  # -------------------------------------------------
  # INITIALIZE STORAGE FOR ALL REPLICATES
  # -------------------------------------------------
  
  all_stacks <- vector("list", n_replicates)
  all_eval <- vector("list", n_replicates)
  
  message(sprintf("\n========================================"))
  message(sprintf("Running %d replicate(s) with 10 PA iterations each", n_replicates))
  message(sprintf("========================================\n"))
  
  # -------------------------------------------------
  # LOOP THROUGH REPLICATES
  # -------------------------------------------------
  
  for (rep in 1:n_replicates) {
    
    message(sprintf("\n--- REPLICATE %d/%d ---\n", rep, n_replicates))
    
    # -------------------------------------------------
    # PARALLEL CLUSTER FOR MODEL TRAINING
    # -------------------------------------------------
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    message("Starting PARALLEL model training...")
    
    start_time <- Sys.time()
    
    models_and_Eval <- foreach(i = 1:10,
                               .packages = c("dplyr", "magrittr", "dismo", "gbm", "pROC")) %dopar% {
      
      # Unique seed incorporating replicate number
      seed <- 12 * i * s_n * rep
      set.seed(seed)
      
      # Clean data
      data_clean <- data %>%
        dplyr::filter(
          S == s,
          is.na(PA_N) | PA_N == i
        )
      
      # Train/test split
      ind <- sample(2, nrow(data_clean), replace = TRUE, prob = c(0.80, 0.20))
      
      data.training <- data_clean[ind == 1, ]
      data.test <- data_clean[ind == 2, ]
      
      # Train model
      model_i <- gbm.fixed(
        data = data.training, 
        gbm.x = V_csi,
        gbm.y = "Occurrence",
        family = "bernoulli", 
        tree.complexity = 8,
        learning.rate = 0.01,
        bag.fraction = 0.7,
        n.trees = 2900
      )
      
      # Evaluate the CSI
      predictions <- predict.gbm(model_i, data.test[, V_csi], 
                                n.trees = model_i$nTrees, type = "response")
      auc_S2 <- auc(roc(as.factor(data.test$Occurrence), predictions))
      
      # Confusion matrix
      actuals <- data.test$Occurrence
      threshold <- 0.5
      predictions_class <- ifelse(predictions >= threshold, 1, 0)
      confusion.matrix.training <- table(actuals, predictions_class)
      
      # Store results
      df_results_i <- data.frame(
        AUC = as.numeric(auc_S2),
        TP = confusion.matrix.training[2, 2],
        TN = confusion.matrix.training[1, 1],
        FP = confusion.matrix.training[1, 2],
        FN = confusion.matrix.training[2, 1],
        S = s,
        PA_N = i,
        Replicate = rep,
        Seed = seed,
        Model = "CSI"
      )
      
      return(list(model = model_i, eval = df_results_i))
    }
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    message(sprintf("  ✓ Training completed in %.1f seconds", training_time))
    
    # Stop cluster after training
    parallel::stopCluster(cl)
    
    # -------------------------------------------------
    # EXTRACT MODELS AND EVALUATION
    # -------------------------------------------------
    
    models <- lapply(models_and_Eval, `[[`, "model")
    eval_df <- dplyr::bind_rows(lapply(models_and_Eval, `[[`, "eval"))
    
    # -------------------------------------------------
    # OPTIMIZED RASTER PREDICTION USING CHUNKING
    # -------------------------------------------------
    
    message("\nStarting OPTIMIZED raster predictions...")
    message("Using spatial chunking for memory efficiency\n")
    
    # Get raster dimensions
    nr <- nrow(env_spat)
    nc <- ncol(env_spat)
    
    # Calculate optimal chunk size (adjust based on your RAM)
    n_chunks <- min(n_cores, ceiling(nr / 1000))
    chunk_rows <- ceiling(nr / n_chunks)
    
    suit_list <- vector("list", length(models))
    
    # Create template raster for results
    template <- env_spat[[1]]
    
    for (i in seq_along(models)) {
      
      start_time_i <- Sys.time()
      mod <- models[[i]]
      
      # Method 1: Use terra's built-in threading (fastest for single predictions)
      terra::terraOptions(threads = parallel::detectCores() - 1, progress = 0)
      
      suit <- terra::predict(env_spat, mod, type = "response", na.rm = TRUE,
                            cores = 1)  # terra handles threading internally
      
      names(suit) <- paste0("CSI_Rep", rep, "_PA", i)
      suit_list[[i]] <- suit
      
      prediction_time <- as.numeric(difftime(Sys.time(), start_time_i, units = "secs"))
      message(sprintf("  ✔ Raster %d/10 completed in %.1f seconds", i, prediction_time))
    }
    
    message(sprintf("\n✔ All raster predictions for replicate %d completed\n", rep))
    
    # Stack rasters for this replicate
    message("Stacking rasters...")
    suit_stack <- terra::rast(suit_list)
    
    # Store results
    all_stacks[[rep]] <- suit_stack
    all_eval[[rep]] <- eval_df
  }
  
  # -------------------------------------------------
  # COMBINE RESULTS FROM ALL REPLICATES
  # -------------------------------------------------
  
  message("\n========================================")
  message("Combining results from all replicates...")
  message("========================================\n")
  
  # Combine all raster stacks
  final_stack <- terra::rast(unlist(all_stacks, recursive = FALSE))
  
  # Combine all evaluation dataframes
  final_eval <- dplyr::bind_rows(all_eval)
  
  # Unregister parallel backend
  unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    if (!is.null(env)) rm(list = ls(name = env), pos = env)
  }
  unregister_dopar()
  
  # -------------------------------------------------
  # RETURN OUTPUT
  # -------------------------------------------------
  
  message("✓ Process completed successfully!\n")
  message(sprintf("Total rasters generated: %d", nlyr(final_stack)))
  message(sprintf("Total evaluations: %d\n", nrow(final_eval)))
  
  return(list(
    stack_pred = final_stack,
    df_eval = final_eval
  ))
}


################################################################################
#Code
################################################################################

#### #### load data #### ####
#load dataset and remove the PCA columns
join_d <- read.csv(data_all_v)

#all_v <- all_v %>% dplyr::select(-PCA_1, -PCA_2)

#load names of climtic variables
V_csi <- strsplit(v_names, ",")[[1]]
cat("Varables selected: ", V_csi, "\n")
#V_csi <- c("bio_2",  "bio_9",  "bio_3" , "bio_1" , "bio_8" , "bio_12", "bio_15")

#C_v <- read.csv(data_bioV)
#C_v <- C_v %>% dplyr::select(all_of(V_csi))

#join the datasets
#join_d <- cbind(all_v, C_v)


#load cliamtic raster
cat("load climatic raster \n")
bioclim<-list.files(path_WorldClim,  pattern = "^[^:]*$|[^:]*[^:]*", full.names = T)
bio<-terra::rast(bioclim)
names(bio)=sub("wc2.1_2.5m_","",names(bio))
bio_sel <- bio[[V_csi]]


##Apply the function

#output will be a list of the CSI for each PA_N and the df with the evaluation of each CSI of each S

system.time({
result_S1<- CSI_df_map_A(join_d, bio_sel, "S1", V_csi, n_cores = 10,  n_replicates = 10)
terra::writeRaster(result_S1[[1]], output_s1, filetype = "GTiff", overwrite = TRUE)
#raster_s1 <- terra::wrap(result_S1[[1]])
#save(raster_s1, file= output_s1)
})

system.time({
  result_S2<- CSI_df_map_A(join_d, bio_sel, "S2", V_csi, n_cores = 10, s_n=2,  n_replicates = 5)
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