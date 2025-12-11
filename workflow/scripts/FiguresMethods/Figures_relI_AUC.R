
library("ggpubr")

dataRF <- read.csv("Internship UNIL/Data/comparison_smk/RF_LH_relimpAUC.csv")
dataBRT <- X

data_RF_BRT<- dataRF #rbind (dataRF, dataBRT)

###Get Accuracy 

data_RF_BRT_acc <- data_RF_BRT %>% mutate(Accuracy = (TP+TN)/(TP+TN+FP+FN), Sensitivity = TP/(FN+TP), Specificity = TN/(TN+FP))

ggplot(data_RF_BRT_acc, aes(Model, AUC, fill =S))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "AUC of ROC for BRT and RF")

ggplot(data_RF_BRT_acc, aes(Model, Accuracy, fill =S))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Model Accuracy of BRT and RF")

ggplot(data_RF_BRT_acc, aes(Sensitivity, Specificity, col = as.factor(PA_N)))+
  geom_point()

#### get Variable importance

##calculate sd and mean of the relative iportance for each variable 

data_RF_BRT_acc_longer <- data_RF_BRT_acc %>% pivot_longer(cols= env_vars, names_to = "Variable", values_to = "Importance")

data_s <- data_RF_BRT_acc_longer %>% group_by(Model, S, Variable) %>% summarise(meanI=mean(Importance), sdI = sd(Importance))


c<-compare_means(Importance ~ S, data = data_RF_BRT_acc_longer, 
              group.by = "Variable")
c <- c %>% mutate(y.position = c(0.45), labels = ifelse(p < 0.05, sprintf("%2.1e", p), p))

ggplot(data_s, aes(Variable, meanI, fill= S))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymax=meanI+sdI, ymin = meanI -sdI),position = position_dodge2(width = 0.5, padding = 0.5))+
  facet_wrap(vars(Model))+
  stat_pvalue_manual(data = as.data.frame(c), label = "p.signif", xmin="Variable", xmax = "Variable")
  