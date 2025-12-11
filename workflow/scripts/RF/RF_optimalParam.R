###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Find optimal parameters RF
# Date: 03/12/2025
## Description: #Aim is to find the combination of parameters (ntrees max est) that achieves
#minimum predictive error (minimum error for predictions to independent samples).
################################################################################

args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
biotic <- args[2]
output <- args[3]


pkg <- c( "randomForest", "pROC", "dplyr", "tidyr", "foreach", "pbapply", "doParallel")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

ApplyingAllCombRF <- function(data, env_vars, rep_id = NULL) {
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  
  # Identifier les lignes avec des NA
  data_clean <- data[complete.cases(data[, env_vars]), ]
  
  X <- data_clean[, env_vars]
  Y <- as.factor(data_clean$Occurrence)
  
  # Grid searh
  n_estimators_grid <- c(100, 300, 500, 700, 1000)
  max_features_grid <- c(2, 3, 4, length(env_vars))
  
  all_comb_RF <- expand.grid(
    n_est = n_estimators_grid,
    max_feat = max_features_grid)
  
  ##Apply RF to each parameter
  
  resultAllComb <- foreach(i=1:nrow(all_comb_RF), .packages = c('randomForest')) %dopar% {
    resultOneComb <- randomForest(
      x = X, 
      y = Y, 
      ntree = all_comb_RF[i, 1],     # ✓ CORRIGÉ: n_est
      mtry = all_comb_RF[i, 2],      # ✓ CORRIGÉ: max_feat
      na.action = na.omit,
      importance = TRUE,
      replace = TRUE,
      nodesize = 1
      
    )
    
    oob_error <- resultOneComb$err.rate[all_comb_RF[i, 1],"OOB"]
    
    return(
      data.frame(
        ntree = all_comb_RF[i, 1],     # ✓ CORRIGÉ
        mtry = all_comb_RF[i, 2],
        oob_error = oob_error,
        oob_accuracy = 1 - oob_error
      )
    )
    }  
  parallel::stopCluster(cl)
  return(resultAllComb)
  }
  




################################################################################
#Code
################################################################################
repl_P_PCA_B_T_HM<-read.csv(occurrences)

order_data_s2 <- repl_P_PCA_B_T_HM %>% filter(S=="S2")

head(order_data_s2)
#### Process tribe list ####
biotic_var <- strsplit(biotic, " +")
biotic_var<-unlist(biotic_var)
cat(sprintf("  Tribes to analyze: %s\n", paste(biotic_var, collapse = ", ")))

explicative_var<-c(biotic_var, 
                   "PCA_1",
                   "PCA_2",
                   "trade",
                   "HM")
#explicative_var<-c("PCA_1",
#                   "PCA_2",
#                   "Leptomyrmecini",
#                   "Tapinomini",
#                   "Bothriomyrmecini",
#                   "Dolichoderini",
#                   "trade",
#                   "HM")

cat("\n Explicative variables : ", explicative_var)

n_reps=100

pb <- txtProgressBar(min = 1, max = 100, style = 3)

cat("\n Initializing OOB method to find optimal parameters \n ")
results_S2_RF <- lapply(1:n_reps, function(rep) {
  #data_current <- order_data_s2   # Assuming you have a list
  set.seed(rep)
  result <- ApplyingAllCombRF(order_data_s2, explicative_var, rep_id = rep)
  setTxtProgressBar(pb, rep)
  return(result)
})

head(results_S2_RF)
# Combine results
results_S2_RF_combined <- dplyr::bind_rows(results_S2_RF)

head(results_S2_RF_combined)

("Saving results...")
write.csv(results_S2_RF_combined,output)