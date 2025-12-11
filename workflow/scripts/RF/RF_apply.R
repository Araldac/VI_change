###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Apply RF with the optimal number of parameters
# Date: 03/12/2025
################################################################################

args <- commandArgs(trailingOnly = TRUE)
occurrences <-args[1]
biotic <- args[2]
RF_params <- args[3]
output <- args[4]


pkg <- c( "randomForest", "pROC", "dplyr", "tidyr", "pbapply")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Functions
################################################################################

RF_apply<-function(data, env_vars, S, seed, pa_n = NA){
        
        set.seed(seed)
  
        # Idenitfy lines with NA 
        data_clean <- data[complete.cases(data[, env_vars]), ]
        
        ####Step 1 - AUC and other model related scores
        ind=sample(2,nrow(data_clean),replace=TRUE,prob=c(0.80,0.20))
        
        data.training=data_clean[ind==1,]
        data.test=data_clean[ind==2,]
        
  
        X <- data.training[, env_vars]
        y <- as.factor(data.training$Occurrence)
        
          rf_S2 <- randomForest(
          x = X,
          y = y,
          ntree = RFp$ntree, 
          mtry = RFp$mtry, 
          importance = TRUE,
          replace = TRUE)
          
          # Évaluation
          predictions <- predict(rf_S2, data.test[, env_vars], type = "prob")[, 2]
          auc_S2 <- auc(roc(as.factor(data.test$Occurrence), predictions))
          
          # Valores reales
          actuals <- data.test$Occurrence
          
          # Convertir probabilidades a clases (0 o 1) usando umbral 0.5
          threshold <- 0.5
          predictions_class <- ifelse(predictions >= threshold, 1, 0)
          confusion.matrix.training <- table(actuals,predictions_class)
          
          #cat("Results for S: " , S, " PA_n : ", pa_n,"\n ")
          #cat(sprintf("AUC S2: %.4f\n", auc_S2))
          #cat(sprintf("OOB Error: %.4f\n", rf_S2$err.rate[1000, "OOB"]))
          
          
          ####Step 2- Obtain the relative importance of the variables
          #using all the data 
          X <- data_clean[, env_vars]
          y <- as.factor(data_clean$Occurrence)
          
          rf_S2 <- randomForest(
            x = X,
            y = y,
            ntree = RFp$ntree, 
            mtry = RFp$mtry, 
            importance = TRUE,
            replace = TRUE)
          
          importance_S2_G <- importance(rf_S2, type = 2)  # GINI
          importance_S2_perc <- importance_S2_G/sum(importance_S2_G[,1])

          #Prepare where to store the results
          colnames_results<-c(env_vars, "AUC", "TP", "TN", "FP", "FN", "S", "PA_N", "Seed", "Model")
          df_results<-data.frame(matrix(ncol= length(colnames_results), nrow=1))
          colnames(df_results)<- colnames_results
          
          #Store the results for this run
          df_results$PA_N <- pa_n
          df_results$Model <- "RF"
          df_results$Seed <- seed
          df_results$S <- S
          df_results$AUC <- as.numeric(auc_S2)
          df_results$TP <- confusion.matrix.training[2,2]
          df_results$TN <- confusion.matrix.training[1,1]
          df_results$FP <- confusion.matrix.training[1,2]
          df_results$FN <- confusion.matrix.training[2,1]

          
          for (var in env_vars){
            df_results[1, var]<-importance_S2_perc[var,]
          }
         
         #print(df_results)
          
          return(df_results)
          
}

################################################################################
#Code
################################################################################
                  
#### #### load data #### #### 
data<-read.csv(occurrences)

#### #### extract optimal parameters for RF #### #### 
RFp <- read.csv(RF_params)


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

cat("Starting RF for S1 \n")
#### #### RF for S1 (100repx10 sets) #### #### 
pb <- txtProgressBar(min = 1, max = 100, style = 3)

  results_S1_RF <- lapply(1:n_reps, function(rep) {
   
    results_s1<-c()
    for (i in 1:10){
      data_s1_i <- data[data$S == "S1" & data$PA_N %in% c(NA, i) ,]
      RF_S1_i <- RF_apply(data_s1_i, explicative_var, "S1",rep, i)
      results_s1<- rbind(results_s1, RF_S1_i)
      #cat("PA_N ", i, "done!")
    }
    setTxtProgressBar(pb, rep)
    return(results_s1)
    
  })
cat("BRT for S1 finished \n")

results_S1 <- dplyr::bind_rows(results_S1_RF)

 
#### #### RF for S2 (100 rep) #### #### 
data_s2<- data[data$S=="S2",]
cat("Starting BRT for S2 \n")
pb <- txtProgressBar(min = 1, max = 100, style = 3)

results_S2_RF <- lapply(1:n_reps, function(rep) {

      seed_s2<-rep*20
      result <- RF_apply(data_s2, explicative_var, "S2", seed_s2)
      
      setTxtProgressBar(pb, rep)
      return(result)
  })
cat("BRT for S2 finished \n ")  
  results_S2 <- dplyr::bind_rows(results_S2_RF)
  
  
cat("Aggregating results for S1 and S2")  
results_RF <- rbind(results_S2, results_S1)
write.csv(results_RF, output)

