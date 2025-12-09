################################################################################
## Project: Intership- Relative influence invasion factor
## Author: Aitana Ralda Corfas
## Date: 05/11/2025
## Style: :tidyverse_style
## Description: Parametrization of the BRT for S2--- Use in thecluster
################################################################################


library(gbm)
library(dismo)
library(dplyr)
library(pbapply)
library(foreach)
library(doParallel)

#1- Parametrization 
#Aim is to find the combination of parameters (lr, tc and nt) that achieves
#minimum predictive error (minimum error for predictions to independent samples).
repl_P_PCA_B_T_HM<-read.csv( "/scratch/araldaco/Internship/LH_allv.csv", sep=",")

#order_data<-repl_P_PCA_B_T_HM %>% dplyr::select(Occurrence, decimallatitude, decimallongitude, S, PA_N, BEN, ISO, everything())
#order_data_s1<-order_data %>% filter(S=="S1")
order_data_s2<-repl_P_PCA_B_T_HM %>% filter(S=="S2")

learningRate<-c(0.01, 0.005, 0.001, 0.0005)
bagFraction<-c(0.4, 0.55, 0.7) # In Elith et al., 2008, they say that best 0.5-0.75
treeComplexity<-c(1, 2, 3, 5, 8) #in Roura-Pascual said that best is the number of variables of a given model

df_BRTparamCombi <- expand.grid(
  lr = learningRate,
  bg = bagFraction, 
  tc = treeComplexity)

# Storage for results
results_S1 <- list()
results_S2 <- list()


ApplyingAllCombGBMStep <- function(all_comb, data, rep_id = NULL) {
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  resultAllComb <- foreach(i=1:nrow(all_comb), .packages = c('dismo')) %dopar% {
    resultOneComb <- gbm.step(
      data = data, 
      gbm.x = 9:16, 
      gbm.y = 2,
      family = "bernoulli", 
      tree.complexity = all_comb[i, 3],
      learning.rate = all_comb[i, 1],
      bag.fraction = all_comb[i, 2],
      n.folds = 10,
      silent = TRUE,
      plot.main = FALSE
    )
    return(
      data.frame(
        nTrees = resultOneComb$n.trees,
        treeComplexity = all_comb[i, 3],
        learningRate = all_comb[i, 1],
        bagFraction = all_comb[i, 2],
        meanDev = resultOneComb$cv.statistics$deviance.mean,
        seDev = resultOneComb$cv.statistics$deviance.se,
        PA_N = na.omit(data$PA_N)[1],
        S = na.omit(data$S)[1],
        rep = ifelse(is.null(rep_id), NA, rep_id),
        stringsAsFactors = FALSE
      )
    )
    
  }
  parallel::stopCluster(cl)
  return(dplyr::bind_rows(resultAllComb))
}



# ========================================
# FOR S2: 100 reps × 1 PA sets = 100 total
# ========================================

pb <- txtProgressBar(min = 1, max = 100, style = 3)
   
results_S2 <- lapply(1:100, function(rep) {
   set.seed(rep)
   result <- ApplyingAllCombGBMStep(df_BRTparamCombi, order_data_s2, rep_id = rep)
   setTxtProgressBar(pb, rep)
   return(result)
 })

# Combine results
results_S2_combined <- dplyr::bind_rows(results_S2)

##store the results
write.csv(results_S2_combined,"/scratch/araldaco/Internship/output_LH_parametrization_S2_v3.csv") ##assign the path 

