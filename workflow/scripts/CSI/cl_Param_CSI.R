################################################################################
## Project: Intership- Relative influence invasion factor
## Author: Aitana Ralda Corfas
## Date: 09/01/2026
## Style: :tidyverse_style
## Description: Parametrization of the BRT for CSI S2--- Use in thecluster
################################################################################


library(gbm)
library(dismo)
library(dplyr)
library(pbapply)
library(foreach)
library(doParallel)



# ========================================
# Functions
# ========================================

ApplyingAllCombGBMStep <- function(all_comb, data, rep_id = NULL) {
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  resultAllComb <- foreach(i=1:nrow(all_comb), .packages = c('dismo')) %dopar% {
    
     v_selected<- c("bio_2",  "bio_9",  "bio_3" , "bio_1" , "bio_8" , "bio_12", "bio_15")
    
    resultOneComb <- gbm.step(
      data = data, 
      gbm.x = v_selected, 
      gbm.y = "Occurrence",
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
        meanAUC =  resultOneComb$cv.statistics$discrimination.mean,
        seAUC =  resultOneComb$cv.statistics$discrimination.se,
        meanCor =  resultOneComb$cv.statistics$correlation.mean,
        seCor = resultOneComb$cv.statistics$correlation.se,
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

##The ROC statistics are : 
#discrimination.mean = cv.roc
#discrimination.se = cv.roc.se

# ========================================
# FOR S2: 10 reps × 10 PA sets = 100 total
# ========================================


#Prepare 
learningRate<-c(0.01, 0.005, 0.001, 0.0005)
bagFraction<-c(0.4, 0.55, 0.7) # In Elith et al., 2008, they say that best 0.5-0.75
treeComplexity<-c(1, 2, 3, 5, 8) #in Roura-Pascual said that best is the number of variables of a given model

df_BRTparamCombi <- expand.grid(
  lr = learningRate,
  bg = bagFraction, 
  tc = treeComplexity)

# Storage for results
results_S2 <- list()

##load data
envtcel_thin <- read.csv("/scratch/araldaco/Internship/LH_BioV.csv", sep = ",")

##Do a BRT 
order_data_s2<-envtcel_thin %>% filter(S=="S2")
order_data_s2$Occurrence <- as.numeric(order_data_s2$Occurrence)

pb <- txtProgressBar(min = 1, max = 100, style = 3)

for(pa_set in 1:10) {
  
  # Extract data for this PA set
  data_current <- order_data_s2 %>% dplyr::filter(PA_N %in% c(NA, pa_set))  # Assuming you have a list
  
  # Run 20 reps for this PA set
  results_pa <- lapply(1:10, function(rep) {
    set.seed(pa_set * 1000 + rep)
    result <- ApplyingAllCombGBMStep(df_BRTparamCombi, data_current, rep_id = rep)
    setTxtProgressBar(pb, (pa_set-1)*10+rep)
    #result$PA_set <- pa_set
    return(result)
  })
  #print(results_pa)
  #   
  results_S2[[pa_set]] <- dplyr::bind_rows(results_pa)
}
# Combine results
results_S2_combined <- dplyr::bind_rows(results_S2)

##store the results
write.csv(results_S2_combined,"/scratch/araldaco/Internship/LH_CSI_BRT_10PA.csv") ##assign the path 

