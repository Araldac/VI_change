###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Apply BRT with the optimal number of parameters
# Date: 03/12/2025
## Description: #Aim is to find the combination of parameters (ntrees max est) that achieves
#minimum predictive error (minimum error for predictions to independent samples).
################################################################################

args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
biotic <- args[2]
BRT_params <- args[3]
output <- args[4]


pkg <- c( "dismo", "pROC", "dplyr", "tidyr", "gbm", "pbapply")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

BRT_apply<-function(data, env_vars, S, seed, pa_n = NA){
  
  set.seed(seed)
  
   # Not nexwesary to remove NA because BRT handles them  
  data_clean <- data #[complete.cases(data[, env_vars]), ]
  
  ####Step 1 - AUC and other model related scores
  ind=sample(2,nrow(data_clean),replace=TRUE,prob=c(0.80,0.20))
  
  data.training=data_clean[ind==1,]
  data.test=data_clean[ind==2,]
  
  #cat(sprintf("Training set: %d samples\n", nrow(data.training)))
  #cat(sprintf("Test set:     %d samples\n", nrow(data.test)))
  #cat(sprintf("  - Presences (1): %d\n", sum(data.test$Occurrence == 1)))
  #cat(sprintf("  - Absences (0):  %d\n", sum(data.test$Occurrence == 0)))
  
  X <- data.training[, env_vars]
  y <- as.factor(data.training$Occurrence)
  
  
  BRT_S <- gbm.fixed(
    data = data.training, 
    gbm.x = explicative_var, ###be careful !!!
    gbm.y = "Occurrence",
    family = "bernoulli", 
    tree.complexity = BRTp$treeComplexity,
    learning.rate = BRTp$learningRate,
    bag.fraction = BRTp$bagFraction,
    n.trees = BRTp$nTrees)
  
  # Évaluation
  predictions <- predict.gbm(BRT_S, data.test[, env_vars], n.trees = BRTp$nTrees, type = "response")
  auc_S2 <- auc(roc(as.factor(data.test$Occurrence), predictions))
  
  # Valores reales
  actuals <- data.test$Occurrence
  
  # Convertir probabilidades a clases (0 o 1) usando umbral 0.5
  threshold <- 0.5
  predictions_class <- ifelse(predictions >= threshold, 1, 0)
  confusion.matrix.training <- table(actuals,predictions_class)
  

  #cat("Results for S: " , S, " PA_n : ", pa_n,"\n ")
  #cat(sprintf("AUC S2: %.4f\n", auc_S2))
  #print(confusion.matrix.training)
  
  ####Step 2- Obtain the relative importance of the variables
  #using all the data 
  
  #cat("Model with all data to have relative importance")
    BRT_S_A <- gbm.fixed(
      data = data_clean, 
      gbm.x = explicative_var, ###be careful !!!
      gbm.y = "Occurrence",
      family = "bernoulli", 
        tree.complexity = BRTp$treeComplexity,
        learning.rate = BRTp$learningRate,
        bag.fraction = BRTp$bagFraction,
        n.trees = BRTp$nTrees)

  # Importance des variables
  importance_S2 <- summary.gbm(BRT_S_A)  # Relative importance
  
  #Prepare where to store the results
  colnames_results<-c(env_vars, "AUC", "TP", "TN", "FP", "FN", "S", "PA_N", "Seed", "Model")
  df_results<-data.frame(matrix(ncol= length(colnames_results), nrow=1))
  colnames(df_results)<- colnames_results
  
  #Store the results for this run
  df_results$PA_N <- pa_n
  df_results$Model <- "BRT"
  df_results$Seed <- seed
  df_results$S <- S
  df_results$AUC <- auc_S2
  df_results$TP <- confusion.matrix.training[2,2]
  df_results$TN <- confusion.matrix.training[1,1]
  df_results$FP <- confusion.matrix.training[1,2]
  df_results$FN <- confusion.matrix.training[2,1]
  
 for (var in env_vars){
   df_results[1, var]<-importance_S2[var, 2]
 }
  
  #cat("Check if all the results are empty", is.na(df_results))                       
  return(df_results)
  
}


################################################################################
#Code
################################################################################
                  
#### #### load data #### #### 
data<-read.csv(occurrences)
head(order_data_s2)

#### #### extract optimal parameters for BRT #### #### 
BRTp <- read.csv(BRT_params)
print(BRTp)

#### Process tribe list ####
biotic_var <- strsplit(biotic, " +")
biotic_var<-unlist(biotic_var)
cat(sprintf("  Tribes to analyze: %s\n", paste(biotic_var, collapse = ", ")))

explicative_var<-c(biotic_var, 
                   "PCA_1",
                   "PCA_2",
                   "trade",
                   "HM")

cat("\n Explicative variables : ", explicative_var, "\n")
n_reps=100


#### #### BRT for S1 (100repx10 sets) #### #### 
cat("Starting BRT for S1 \n")
pb <- txtProgressBar(min = 1, max = 100, style = 3)
    
  results_S1_BRT <- lapply(1:n_reps, function(rep) {

    results_s1<-c()
    for (i in 1:10){
      data_s1_i <- data[data$S == "S1" & data$PA_N %in% c(NA, i) ,]
      BRT_S1_i <- BRT_apply(data_s1_i, explicative_var, "S1",rep, i)
      results_s1<- rbind(results_s1, BRT_S1_i)
      #cat("PA_N ", i, "done!")
    }
   setTxtProgressBar(pb, rep)
    return(results_s1)
    
  })
 cat("BRT for S1 finished \n")
  results_S1 <- dplyr::bind_rows(results_S1_BRT)
  
  
#### #### BRT for S2 (100 rep) #### #### 
    cat("Starting BRT for S2 \n")

pb <- txtProgressBar(min = 1, max = 100, style = 3)

  results_S2_BRT <- lapply(1:n_reps, function(rep) {

    seed_s2<-rep*20
      BRT_S2 <- BRT_apply(data_s2, explicative_var, "S2",seed_s2)
      setTxtProgressBar(pb, rep)

      return(BRT_S2)
  })
      cat("BRT for S2 finished \n")  
  results_S2 <- dplyr::bind_rows(results_S2_BRT)
  
  
cat("Aggregating results for S1 and S2")  
results_BRT <- rbind(results_S2, results_S1)
write.csv(results_BRT, output)