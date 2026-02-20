###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Forecast usign s1 data to S2
# Date: 20/02/2026
## Description: #
################################################################################

args <- commandArgs(trailingOnly = TRUE)
data <-args[1]
params <- args[2]
output <- args[3]


pkg <- c( "randomForest", "pROC", "dplyr", "tidyr", "foreach", "pbapply", "doParallel", "doSNOW")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

#### #### Train with X test with Y, evaluation ####  ####
RF_apply<-function(data, env_vars, params, S_train, S_test, PA_train, PA_test, rep, Mod){
  
  seed <- PA_train + 4*PA_test + rep
  set.seed(seed)
  
  # Idenitfy lines with NA 
  data_clean <- data[complete.cases(data[, env_vars]), ]
  
  data.training <- data_clean %>% filter(S == S_train, is.na(PA_N) | PA_N == PA_train) 
  data.test <- data_clean %>% filter(S == S_test, is.na(PA_N) | PA_N == PA_test)
  
  X <- data.training[, env_vars]
  y <- as.factor(data.training$Occurrence)
  
  rf_trained <- randomForest(
    x = X,
    y = y,
    ntree = params$ntree, 
    mtry = params$mtry, 
    importance = TRUE,
    replace = TRUE)
  
  # Évaluation
  predictions <- predict(rf_trained, data.test[, env_vars], type = "prob")[, 2]
  auc_S2 <- auc(roc(as.factor(data.test$Occurrence), predictions))
  
  # Valores reales
  actuals <- data.test$Occurrence
  
  # Convertir probabilidades a clases (0 o 1) usando umbral 0.5
  threshold <- 0.5
  predictions_class <- ifelse(predictions >= threshold, 1, 0)
  confusion.matrix.training <- table(actuals,predictions_class)
  
  #Prepare where to store the results
  colnames_results<-c("AUC", "TP", "TN", "FP", "FN", "S_train", "S_test", "PA_test", "PA_train", "Seed", "Rep", "Model")
  df_results<-data.frame(matrix(ncol= length(colnames_results), nrow=1))
  colnames(df_results)<- colnames_results
  
  #Store the results for this run
  df_results$PA_test <- PA_test
  df_results$PA_train <- PA_train
  df_results$Model <- Mod
  df_results$Seed <- seed
  df_results$S_train <- S_train
  df_results$S_test <- S_test
  df_results$Rep <- rep
  df_results$AUC <- as.numeric(auc_S2)
  df_results$TP <- confusion.matrix.training[2,2]
  df_results$TN <- confusion.matrix.training[1,1]
  df_results$FP <- confusion.matrix.training[1,2]
  df_results$FN <- confusion.matrix.training[2,1]
  
  return(df_results)
  
}


################################################################################
#Code
################################################################################
#load data 
data_PCA<- read.csv(data)
params_PCA <- read.csv(params)

head(data_PCA)
head(params_PCA)

taxa <- unique(data_PCA$Taxa)

if (length(taxa) > 1){
    cat("there is multiple taxa ", taxa )
} else{
  cat("Taxa is : ", taxa )
  }


#expand all options
all_comb_RF <- expand.grid(
  S_train = "S1",
  S_test = "S2",
  PA_train = 1:10,
  PA_test = 1:10,
  stringsAsFactors = FALSE)

#remove cases where it is training and testing on the same dataset
all_comb_RF <- all_comb_RF %>%
  filter(!(S_train == S_test & PA_train == PA_test))

cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)


#add a progress bar
registerDoSNOW(cl)
iterations <- nrow(all_comb_RF)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

PCA_r <- list()

PCA_vars <- c("PCA_1","PCA_2","MPD","trade","HM")


resultAllComb <- foreach(
  i = 1:nrow(all_comb_RF), .combine = rbind, .options.snow = opts,
  .packages = c('randomForest', 'dplyr', 'pROC')
) %dopar% {
  
  #--- PCA ---
  PCA_r <- lapply(1:10, function(rep){
    RF_apply(
      data      = data_PCA,
      env_vars  = PCA_vars,
      params    = params_PCA, 
      S_train   = all_comb_RF[i, 1],
      S_test    = all_comb_RF[i, 2],
      PA_train  = all_comb_RF[i, 3],
      PA_test   = all_comb_RF[i, 4],
      rep       = rep,
      Mod       = "PCA"
    )
  })
  
  
  
  # Return list for this combination
  list(PCA = PCA_r)
}

close(pb)
parallel::stopCluster(cl)

resultAllComb<-bind_rows(resultAllComb)

resultAllComb$Taxa <- taxa
head(resultAllComb)
write.csv(resultAllComb, file = output)
