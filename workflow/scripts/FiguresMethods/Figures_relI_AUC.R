
library("ggpubr")
library(vcd)

dataRF <- read.csv("Internship UNIL/Data/comparison_smk/RF_LH_relimpAUC.csv")
dataBRT <- read.csv("Internship UNIL/Data/comparison_smk/BRT_LH_relimpAUC.csv")

#modifytu up to 100 --> to 0.1
head(dataBRT)
dataBRT[, 2:9]<- dataBRT[, 2:9]/100

data_RF_BRT<- rbind (dataRF, dataBRT)
data_RF_BRT$Model <- as.factor(data_RF_BRT$Model)

###Get Accuracy 
data_RF_BRT_acc <- data_RF_BRT %>% mutate(Accuracy = (TP+TN)/(TP+TN+FP+FN), Sensitivity = TP/(FN+TP), Specificity = TN/(TN+FP))

#Plot AUC
ggplot(data_RF_BRT_acc, aes(Model, AUC, fill =S))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "AUC of ROC for BRT and RF")

  
#PLot Accuracy
ggplot(data_RF_BRT_acc, aes(Model, Accuracy, fill =S))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Model Accuracy of BRT and RF")

#get Kappa
data_RF_BRT_kapp <- data_RF_BRT_acc %>%
  mutate(
    # Total observations
    N = TP + TN + FP + FN,
    
    # Expected agreement (correct formula)
    Pe = ((TP + FP) * (TP + FN) + (TN + FP) * (TN + FN)) / (N^2),
    
    # Observed agreement (if you don't have it already)
    Po = (TP + TN) / N,
    
    # Cohen's Kappa
    Kappa = (Po - Pe) / (1 - Pe)
  )

# Check the results
summary(data_RF_BRT_kapp$Pe)  # Should be between 0 and 1
summary(data_RF_BRT_kapp$Kappa)  # Should be between -1 and 1

#plot them 
ggplot(data_RF_BRT_kapp, aes(Model, Kappa, fill =S))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Model Cohens Kappa of BRT and RF")


#### get Variable importance

##calculate sd and mean of the relative iportance for each variable 

data_RF_BRT_acc_longer <- data_RF_BRT_acc %>% pivot_longer(cols= env_vars, names_to = "Variable", values_to = "Importance")



data_s <- data_RF_BRT_acc_longer %>% group_by(Model, S, Variable) %>% summarise(meanI=mean(Importance), sdI = sd(Importance))


c<-compare_means(Importance ~ S, data = data_RF_BRT_acc_longer, 
              group.by = c("Variable", "Model"))
c <- c %>% mutate(y.position = c(0.7), labels = ifelse(p < 0.05, sprintf("%2.1e", p), p))


ggplot(data_s, aes(Variable, meanI, fill= S))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymax=meanI+sdI, ymin = meanI -sdI),position = position_dodge2(width = 0.5, padding = 0.5))+
  facet_wrap(vars(Model), scales="fixed")+
  stat_pvalue_manual(data = as.data.frame(c), label = "p.signif", xmin="Variable", xmax = "Variable")
  