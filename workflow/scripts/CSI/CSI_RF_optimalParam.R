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
climaitc_vars <- args[3]
output <- args[4]

pkg <- c( "randomForest", "pROC", "dplyr", "tidyr", "foreach", "pbapply", "doParallel")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

ApplyingAllCombRF <- function(data, env_vars, rep_id = NULL) {
  #cat("cluster \n")
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  
  #cat("clean data \n")
  # Identifier les lignes avec des NA
  data_clean <- data[complete.cases(data[, env_vars]), ]
  
  X <- data_clean[, env_vars]
  Y <- as.factor(data_clean$Occurrence)
  
  # Grid searh
  #cat("grid \n")
  n_estimators_grid <- c(100, 300, 500, 700, 1000)
  max_features_grid <- c(2, 3, 4, length(env_vars))
  
  all_comb_RF <- expand.grid(
    n_est = n_estimators_grid,
    max_feat = max_features_grid)
  
  ##Apply RF to each parameter
  #cat("Apply RF to each parameter \n")
  resultAllComb <- foreach(i=1:nrow(all_comb_RF), .packages = c('randomForest')) %dopar% {
    cat(" RF  \n")
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
    #cat(" OOB  \n")
    oob_error <- resultOneComb$err.rate[all_comb_RF[i, 1],"OOB"]
    
    return(
      data.frame(
        ntree = all_comb_RF[i, 1],     # ✓ CORRIGÉ
        mtry = all_comb_RF[i, 2],
        oob_error = oob_error,
        oob_accuracy = 1 - oob_error,
        PA_N = na.omit(data_clean$PA_N)[1],
        S = na.omit(data_clean$S)[1],
        rep = ifelse(is.null(rep_id), NA, rep_id)
      )
    )
  }  
  parallel::stopCluster(cl)
  return(resultAllComb)
}





################################################################################
#Code
################################################################################
data<-read.csv(occurrences)
#repl_P_PCA_B_T_HM<-read.csv("//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/AssignVariables/LH_PA_B_T_HM.csv")

order_data_s2 <- data %>% filter(S=="S2")

head(order_data_s2)

### Process tribe list ####
#biotic <- c("Leptomyrmecini", "Tapinomini", "Bothriomyrmecini", "Dolichoderini")


head(order_data_s2)
#### Process tribe list ####
biotic_var <- strsplit(biotic, " +")
biotic_var<-unlist(biotic_var)
cat(sprintf("  Tribes to analyze: %s\n", paste(biotic_var, collapse = ", ")))

#### Process climalist ####
climaitc_vars <- strsplit(climaitc_vars, " +")
climaitc_vars<-unlist(climaitc_vars)
cat(sprintf("  TClimatic variables are : %s\n", paste(climaitc_vars, collapse = ", ")))

explicative_var<-c(biotic_var, 
                   climaitc_vars,
                   "trade",
                   "HM")

cat("\n Explicative variables : ", explicative_var)

n_reps=10
results_S2 <- list()
pb <- txtProgressBar(min = 1, max = 100, style = 3)

cat("\n Initializing OOB method to find optimal parameters \n ")

for(pa_set in 1:10) {
  
  # Extract data for this PA set
  data_current <- order_data_s2 %>% dplyr::filter(PA_N %in% c(NA, pa_set))  # Assuming you have a list
  
  # Run 20 reps for this PA set
  results_pa <- lapply(1:n_reps, function(rep) {
    set.seed(pa_set * 1000 + rep)
    result <- ApplyingAllCombRF(data_current, explicative_var, rep_id = rep)
    setTxtProgressBar(pb, (pa_set-1)*10+rep)
    #result$PA_set <- pa_set   never activate or it crashes !!!
    
    class(result)
    head(result)
    return(result)
  })
  head(results_pa)
  #   
  results_S2[[pa_set]] <- dplyr::bind_rows(results_pa)
}

head(results_S2)
# Combine results
results_S2_RF_combined <- dplyr::bind_rows(results_S2)

head(results_S2_RF_combined)

("Saving results...")
write.csv(results_S2_RF_combined,output)