library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(compositions)
library(ggplot2)
library(patchwork)
library(reshape2)
library(dplyr)
library(grid)
library(sf)
library(ggpubr)
library(rpart)
library(plotly)
library(ggfortify)
library(viridis)
library(ggdendro)
library(Kendall)
library(tidyverse)
library(rstatix)
library(ggpubr)

############################
# ##  1) Paramètres utilisateur    ####
############################

data_path   <- "Internship UNIL/results/all_occ_PCA_2.csv"       # Chemin vers votre fichier CSV
species_col <- "Taxa"
pa_col      <- "PA_N_new"
S_col       <- "S"               # facteur S1 / S2

comp_cols<- c("MPD", "PCA_1", "PCA_2", "trade", "HM")

level_S1 <- "S1"
level_S2 <- "S2"

perc_scale <- 100                # si vos pourcentages sont en 0–100

cluster_mode <- "species_S"      # "species_S", "species_avg", "by_PA"

############################
# ## 2) Chargement     ####
############################
# pathallV <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/RF/"
# allV_files <- list.files(pathallV,  pattern = "_relimpAUC1\\.csv$", full.names = T)
# all_oc <- lapply(allV_files, function(f) {
#   df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
#   return(df)
# }) %>% bind_rows()  %>% mutate(Accuracy = (TP+TN)/(TP+TN+FP+FN), Sensitivity = TP/(FN+TP), Specificity = TN/(TN+FP))
# all_oc <- all_oc%>% mutate(PA_N_new = paste0(Taxa, "_", PA_N, S))
# write.csv(all_oc, "Internship UNIL/results/all_occ_PCA_2.csv")

raw <- read_csv(data_path, show_col_types = FALSE)

raw <- raw %>%
  mutate(across(all_of(c(species_col, pa_col, S_col)), as.factor))

# Forcer l'ordre S1, S2
raw[[S_col]] <- factor(raw[[S_col]], levels = c(level_S1, level_S2))

############################
# ## Visualization of %     ####
############################

palette8 <- c(
  "#1b9e77", "#1b9e7780",   # Group1 S1 / S2
  "#d95f02", "#d95f0280",   # Group2 S1 / S2
  "#7570b3", "#7570b380"   # Group3 S1 / S2
  #  "#e7298a", "#e7298a80"    # Group4 S1 / S2
)

v_remove <- c("X","AUC","TP","TN","FP","FN","Seed","Accuracy","Sensitivity", "Specificity", "PA_N", "PA_N_new")


raw_long <- raw %>% dplyr::select(all_of(comp_cols), Taxa, S) %>% pivot_longer(cols= !c(S, Taxa), names_to = "Variable", values_to = "Importance")
raw_long_S <- raw_long %>% group_by(Taxa, S, Variable) %>% summarise(meanI=mean(Importance), sdI = sd(Importance)) %>% 
    dplyr::mutate(
      Groupv = dplyr::case_when(
        Variable %in% c( "MPD") ~ "Biotic: Related species",
        Variable %in% c("HM","trade") ~ "Anthropogenic",
        Variable %in% c("PCA_1","PCA_2","CSI", "bio_5",  "bio_9",  "bio_3" , "bio_2" , "bio_8" , "bio_12", "bio_15") ~ "Abiotic: Climatic",
        TRUE ~ NA_character_
      )
    ) 
  
  
raw_long_S$Variable <- forcats::fct_relevel(
  raw_long_S$Variable,
    "MPD",
    "HM","trade","PCA_1","PCA_2"
  )
  
#PLOT 
Figure_VImp <-ggplot(raw_long_S, 
                       aes(Variable, meanI, 
                           fill = interaction(S, Groupv), 
                           drop = TRUE)) +
    
    geom_col(position = "dodge") +
    
    geom_errorbar(aes(ymax = meanI + sdI, ymin = meanI - sdI),
                  position = position_dodge2(width = 0.5, padding = 0.5)) +
    
    xlab("")+
    ylab("Variable importace")+
    
    scale_fill_manual(values = palette8) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
    
    facet_wrap(vars(Taxa), scales = "free_x", drop = TRUE)
  
############################
# ##  Evaluation metrics RF for variable importance   ####
############################

#plot evaluation metrics 
raw <- raw %>% mutate(
  # Total observations
  N = TP + TN + FP + FN,
  # Expected agreement (correct formula)
  Pe = ((TP + FP) * (TP + FN) + (TN + FP) * (TN + FN)) / (N^2),
  # Observed agreement (if you don't have it already)
  Po = (TP + TN) / N,
  # Cohen's Kappa
  Kappa = (Po - Pe) / (1 - Pe)
)

#AUC
auc_p<-ggplot(raw, aes(Taxa, AUC, fill =S))+
  geom_boxplot()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()
#+labs(title = "AUC of ROC for CSI and PCA RF models")

# Accuracy
acc_p <- ggplot(raw, aes(Taxa, Accuracy, fill =S))+
  geom_boxplot()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()
#+labs(title = "Model Accuracy of CSI and PCA RF models")

#Kappa
Kapp_p <- ggplot(raw, aes(Taxa, Kappa, fill =S))+
  geom_boxplot()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()

sens_p <-  ggplot(raw, aes(Taxa, Sensitivity, fill =S))+
  geom_boxplot()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()
#+labs(title = "Model Cohens Kappa of CSI and PCA RF models")


figure_Eval <- ggarrange(auc_p, acc_p, Kapp_p, sens_p, common.legend = TRUE, ncol=1, nrow=4, labels= c("A", "B", "C", "D"), legend ="right")
#ggexport(figure_Eval, filename = "~/Internship UNIL/Figures_SMK/figure_EvalCSI.pdf")
figure_Eval



############################
# ##  Evaluation metrics recast     ####
############################
pathallV <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/Anlaysis//"
allV_files <- list.files(pathallV,  pattern = "_forecast\\.csv$", full.names = T)
recast <- lapply(allV_files, function(f) {
  df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
  return(df)
}) %>% bind_rows()  %>% mutate(Accuracy = (TP+TN)/(TP+TN+FP+FN), Sensitivity = TP/(FN+TP), Specificity = TN/(TN+FP),
# Total observations
N = TP + TN + FP + FN,
# Expected agreement (correct formula)
Pe = ((TP + FP) * (TP + FN) + (TN + FP) * (TN + FN)) / (N^2),
# Observed agreement (if you don't have it already)
Po = (TP + TN) / N,
# Cohen's Kappa
Kappa = (Po - Pe) / (1 - Pe)
)

#AUC
auc_p_R <- ggplot(recast, aes(Taxa, AUC))+
  geom_boxplot()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()
#+labs(title = "AUC of ROC for CSI and PCA RF models")

# Accuracy
acc_p_R <- ggplot(recast, aes(Taxa, Accuracy))+
  geom_boxplot()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()
#+labs(title = "Model Accuracy of CSI and PCA RF models")

#Kappa
Kapp_p_R <- ggplot(recast, aes(Taxa, Kappa))+
  geom_boxplot()+
  theme_minimal()+  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  

sens_p_R <-  ggplot(recast, aes(Taxa, Sensitivity))+
  geom_boxplot()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  theme_minimal()
#+labs(title = "Model Cohens Kappa of CSI and PCA RF models")


figure_Eval_R <- ggarrange(auc_p_R, acc_p_R, Kapp_p_R, sens_p_R, common.legend = TRUE, ncol=1, nrow=4, labels= c("A", "B", "C", "D"), legend ="right")
#ggexport(figure_Eval, filename = "~/Internship UNIL/Figures_SMK/figure_EvalCSI.pdf")
figure_Eval_R


########################################
#  Heatmap %S1 , S2, S2-S1 and Kendall ####
########################################

#### make mean using LMMM
per_species_pvals_mean_df <- list()
#mean_s1 <- list()
#mean_s2 <- list()
plot_list <- list()
#per_species_wilcox_df <- list()

for (sp in species_list) {
  
  sub <- raw %>% filter(Taxa == sp)
  
  fits <- lapply(comp_cols, function(coord) {
    lmer(as.formula(paste0(coord, " ~ ", S_col, " + (1|", pa_col, ")")),
         data = sub)
  })
  names(fits) <- comp_cols
  # p-values
  pvals <- sapply(fits, function(m) summary(m)$coefficients[2, "Pr(>|t|)"])
  coef <-  sapply(fits, function(m) summary(m)$coefficients[2, "Estimate"])

    
  per_species_pvals_mean_df[[sp]] <- tibble(
    species = sp,
    variable = comp_cols,
    estimate = coef,
    p_value = pvals
  )
  
  
  sub_rank <- sub %>% select(all_of(comp_cols), S)
  sub_rank <- lapply(1:nrow(sub_rank), function(i) rank(-sub_rank[i, comp_cols])) %>% bind_rows() %>% mutate(S = sub_rank$S )
  sub_rank_long <- sub_rank %>% pivot_longer(cols= comp_cols, names_to = "Variable", values_to = "Values")

  plot_list[[sp]] <- ggplot(sub_rank_long, aes(Values, fill=S))+ labs(title = sp)+
      geom_bar(stat = "count", position="dodge") + facet_wrap(~Variable)
  
  cat("finished ", sp, "\n" )
  
}


per_species_pvals_mean_df <- bind_rows(per_species_pvals_mean_df)

ggarrange(plotlist = plot_list, ncol = 4, nrow = 4, common.legend = TRUE)

#filter non significant differences 
per_species_pvals_mean_df_sig <- per_species_pvals_mean_df
per_species_pvals_mean_df_sig$estimate[per_species_pvals_mean_df_sig$p_value > 0.05] <- 0

per_species_pvals_mean_df_long <- per_species_pvals_mean_df_sig %>% select(-p_value) %>% pivot_wider(names_from= variable, values_from = estimate)



"cluster_heatmap_plot" <- function(data, col_w_val, title_c, method_clust = "ward.D2"){
  # data: in wide format with species and one column for each variable
  # col_w_val: name of the columns with values on, in order of appearance from which to construct the dendogram
  # title_c: optional title for the heatmap
  
  #### make cluster 
  D <- dist(data[, col_w_val], method = "euclidean")
  hc <- hclust(D, method = method_clust)
  
  hc$labels <- data$species
  
  hc_dendro <- as.dendrogram(hc)
  
  
  dendro_plot <- ggdendrogram(data = hc_dendro, 
                              leaf_labels =TRUE, 
                              rotate = TRUE, 
                              labels=TRUE) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(l=0))+
    labs(y=NULL)
  
  ##make heatmap
  data_long <- data %>% group_by(species) %>%
    pivot_longer(all_of(col_w_val), names_to = "Variables", values_to = "Values")
  
  # Extract the order of the tips in the dendrogram
  hc_order <- order.dendrogram(hc_dendro)
  # Order the levels according to their position in the cluster
  data_long$species <- factor(x = data_long$species,
                              levels = data$species[hc_order], 
                              ordered = TRUE)
  
  
  heatmapp_plot <- ggplot(data_long, aes(Variables, species, fill = Values)) +
    geom_tile(color = "grey80") +
    scale_fill_gradient2(
      low = "#740580", mid = "white", high = "#F2C447",
      midpoint = 0
    ) +
    labs(
      #title = title_c,
      y = "Species",
      x="Variables"
    ) + 
    scale_y_discrete(limits = delta_plr_df$species[hc_order])+
    scale_x_discrete(limits = col_w_val)+
    #geom_text(aes(label=stars), color="black", size=5) + 
    theme_minimal(base_size = 14) +
    theme(legend.position= "left", axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  final_p <- ggarrange(heatmapp_plot, dendro_plot, align="h",  widths = c(2, 1))
  
  return(final_p)
  
}

comp_cols_f <- factor(comp_cols, levels= comp_cols, ordered=TRUE)

set.seed(123)
a<- cluster_heatmap_plot(per_species_pvals_mean_df_long, as.factor(comp_cols_f), "Heatmap of s2-s1", method = "complete")


###Calcualte Wilcoxon rank test for each variable rank   

sub_rank <- raw %>% select(all_of(comp_cols), S, Taxa)
sub_rank <- lapply(1:nrow(sub_rank), function(i) rank(sub_rank[i, comp_cols])) %>% bind_rows() %>% mutate(S = sub_rank$S, Taxa = sub_rank$Taxa)
mean_sub_rank <- sub_rank %>% group_by(Taxa, S) %>% summarise(across(all_of(comp_cols), mean), .groups = "drop")

wilcox_by_species <- sub_rank %>%
  pivot_longer(cols= comp_cols, names_to = "variable", values_to = "rank") %>%
  group_by(Taxa, variable) %>%
  summarise(
    n_S1 = sum(S == "S1"),
    n_S2 = sum(S == "S2"),
    mean_rank_S1 = mean(rank[S == "S1"]),
    mean_rank_S2 = mean(rank[S == "S2"]),
    # Wilcoxon rank-sum (unpaired) on the ranks
    p_wilcox = wilcox.test(rank ~ S, exact = FALSE)$p.value,
    # Effect size: normalized mean-rank difference in [-1,1] (divide by (k-1)=4 for k=5)
    effect_norm = (mean_rank_S2 - mean_rank_S1) / 4,
    .groups = "drop"
  ) %>%
  group_by(Taxa) %>%
  mutate(p_adj_BH = p.adjust(p_wilcox, method = "BH")) %>%
  ungroup()

wilcox_by_species$effect_norm[wilcox_by_species$p_wilcox > 0.05] <- 0
wilcox_by_species_df_long <- wilcox_by_species %>% select(Taxa, variable, effect_norm) %>% pivot_wider(names_from= variable, values_from = effect_norm) %>% mutate(species = Taxa)

set.seed(123)
b <- cluster_heatmap_plot(wilcox_by_species_df_long, as.factor(comp_cols_f), "Heatmap of s2-s1", method = "complete")


data_long <- wilcox_by_species_df_long %>% group_by(species) %>%
  pivot_longer(all_of(col_w_val), names_to = "Variables", values_to = "Values")

# Extract the order of the tips in the dendrogram

# Order the levels according to their position in the cluster
data_long$species <- factor(x = data_long$species,
                            levels = per_species_pvals_mean_df_long$species[hc_order], 
                            ordered = TRUE)


heatmapp_plot <- ggplot(data_long, aes(Variables, species, fill = Values)) +
  geom_tile(color = "grey80") +
  scale_fill_gradient2(
    low = "#740580", mid = "white", high = "#F2C447",
    midpoint = 0
  ) +
  labs(
    #title = title_c,
    y = "Species",
    x="Variables"
  ) + 
  scale_y_discrete(limits = delta_plr_df$species[hc_order])+
  scale_x_discrete(limits = col_w_val)+
  #geom_text(aes(label=stars), color="black", size=5) + 
  theme_minimal(base_size = 14) +
  theme(legend.position= "left", axis.text.x = element_text(angle = 45, hjust = 1))


ggarrange(a, b, ncol =2, nrow=1)



###kendall
sub_rank_long <- mean_sub_rank %>% pivot_longer(cols= comp_cols, names_to = "Variable", values_to = "Values")
df_kendall <- data.frame(matrix(nrow=13, ncol=2), row.names = sp_list)
colnames(df_kendall)<- c("tau", "p-value")
 for (sp in sp_list){
     comp_long_spS1 <- sub_rank_long %>% filter(Taxa == sp & S =="S1") 
     comp_long_spS2 <- sub_rank_long %>% filter(Taxa == sp & S =="S2") 
    
       k <- cor.test(comp_long_spS1$Values, comp_long_spS2$Values, method = "kendall", )
       df_kendall[sp, "tau"] <- k$estimate[1]
       df_kendall[sp, "p-value"] <- k$p.value
     
         
       }
df_kendall %>% arrange(desc(tau))




####Sp info 
first_records <- read.csv("Internship UNIL/Data/alien_ant_first_records.tsv", sep = "\t")
sp <- c("LH",  "TM",  "PL",  "MP",  "PM",  "MF",  "HP" ,  "HO", "WA", "AG", "TB", "TA", "HE")

a <- as.data.frame(unlist(list( "LH"= "Linepithema humile",
                                "TM"= "Tapinoma melanocephalum",
                                "WA"= "Wasmannia auropunctata",
                                "PL"= "Paratrechina longicornis",
                                "MP"= "Monomorium pharaonis",
                                "PM"= "Pheidole megacephala",
                                "MF"= "Monomorium floricola",
                                "HP"= "Hypoponera punctatissima",
                                "HO"= "Hypoponera opaciceps",
                                "AG"= "Anoplolepis gracilipes",
                                "HE"= "Hypoponera eduardi",
                                "TB"= "Tetramorium bicarinatum",
                                "TA"= "Technomyrmex albipes"), use.names = TRUE), ncol=13)
colnames(a) <- "species"
a <- tibble::rownames_to_column(a, "sp")

sp_info2 <- data.frame(matrix(ncol=0, nrow=13))
row.names(sp_info2) <- unique(raw$Taxa)

for (species in row.names(sp_info2)){
  N_sp <- antmaps_dis[[species]] %>% dplyr::filter(category == "N") %>% nrow()
  T_sp <- antmaps_dis[[species]] %>% dplyr::filter(category %in% c("E","N")) %>% nrow()
  sp_info2[species, "Nben"] <- N_sp
  sp_info2[species, "Tben"]<- T_sp
  
  
  name <- species
  species <- a$sp[a$species== name]
  cat("Start sp", species, "\n")
  thin <- read.csv(paste0("//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/DatasetPrep/",species,"_thinV.csv"))
  
  sp_info2[name, "S1"] <- nrow(thin[thin$S == "S1", ])
  sp_info2[name, "S2"]<- nrow(thin[thin$S == "S2", ])
  
  S1_Eben <- unique(thin[thin$S == "S1", "BEN"])
  S2_Eben <- unique(thin[thin$S == "S2", "BEN"])
  S2_Eben_U <- S2_Eben[! S2_Eben %in% S1_Eben]
  
  sp_info2[name, "U_ben_S1"] <- length(S1_Eben)
  sp_info2[name, "U_ben_S2"] <- length(S2_Eben_U)
  
  
  
  FR_SP <- first_records[first_records$Taxon == name, ]
  sp_info2[name, "Date"]<- min(FR_SP$FirstRecord)
  sp_info2[name, "tau"] <- df_kendall[name,"tau"]
}

print(sp_info2)
sp_info2 <- tibble::rownames_to_column(sp_info2, "species")

par(mfrow=c(3,3))
for (i in 1:7){
  col<- colnames(sp_info2)[i]
 plot(sp_info2[,col], sp_info2$tau, xlab= col) 
}


########################################
# 4) Variables s1-S2 P-PA ####
########################################

#load data pca all datasets
pathallV <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/AssignVariables//"
allV_files <- list.files(pathallV,  pattern = "_allv\\.csv$", full.names = T)
allv_pca <- lapply(allV_files, function(f) {
  df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
  return(df)
}) %>% bind_rows()

allv_pca_long <- allv_pca %>%  pivot_longer(all_of(comp_cols), names_to = "Variable", values_to = "Value") %>% select(Taxa, S, Occurrence, Variable, Value)%>%
  drop_na(Value)
allv_pca_long$Occurrence <- as.factor(allv_pca_long$Occurrence )
ggplot(allv_pca_long, aes(S, Value, fill= Occurrence ))+
  geom_boxplot()+
  facet_wrap(~Variable, ncol=5, scales = "free")

library(lme4)
library(emmeans)

list_models  <- list()
list_results <- list()
list_speffect <- list()
for (var in comp_cols) {
  
  cat("Fitting model for:", var, "\n")
  
  var_df <- allv_pca %>%
    select(S, Taxa, Occurrence, all_of(var)) %>%
    drop_na()
  
  var_df$S          <- as.factor(var_df$S)
  var_df$Taxa       <- as.factor(var_df$Taxa)
  var_df$Occurrence <- as.factor(var_df$Occurrence)
  
  # Linear mixed model
  #model <- lmer(get(var) ~ S * Occurrence + (1 | Taxa), data = var_df)
  model <- lmer(get(var) ~ Occurrence + (Occurrence | Taxa), data = var_df)
  
  # store# storeTaxa
  list_models[[var]] <- model
  
  # ANOVA table
  list_results[[var]] <- anova(model)
  
  library(dplyr)
  sp_effects <- coef(model)$Taxa %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Taxa")
  
  list_speffect[[var]] <- sp_effects

  
}



ggplot(allv_pca_long, aes(Occurrence, Value, fill=Occurrence))+
  geom_boxplot()+ theme_minimal()+ #geom_point(position = "jitter")+
  facet_wrap(~Variable, ncol=5, scales = "free")




#############################################################################
#############briyullon 


library(dplyr)

rank_cols <- c("MPD", "PCA_1", "PCA_2", "trade", "HM")

df_ranked <- raw %>% select(all_of(comp_cols), S, Taxa) 

df_ranked <- lapply(1:nrow(df_ranked), function(i) rank(-df_ranked[i, comp_cols])) %>% bind_rows() %>% mutate(S = df_ranked$S, Taxa = df_ranked$Taxa )

df_ranked_long <- df_ranked %>% group_by(S, Taxa) %>% pivot_longer(cols= all_of(comp_cols), names_to = "Variable", values_to = "Values")

sub_rank_parallel <- sub_rank_long %>% pivot_wider(names_from= S, values_from = Values)
raw_long_parallel <- raw_long  %>% group_by(Taxa) %>% mutate(row = row_number()) %>% 
  pivot_wider(names_from= S, values_from = Importance) %>%
  select(-row)

library(ggbump)
library(tidyverse)

ggparcoord(raw_long_parallel, columns = 3:4, groupColumn = 2) + facet_wrap(~Taxa)

ggplot(sub_rank_long,aes(x=S, y=Values,color=Variable, group=Variable))+
  geom_line() +
  facet_wrap(~Taxa)




##Plot 1 - Changes in the variable importance and their rank
palette8 <- c(
  "#1b9e77",   # Group1 S1 / S2
  "#d95f02",
  "#d9025330",   # Group2 S1 / S2
  "#7570b3", 
  "#780"   # Group3 S1 / S2
  #  "#e7298a", "#e7298a80"    # Group4 S1 / S2
)

raw_long_S$Variable <- forcats::fct_relevel(
  raw_long_S$Variable,
  "MPD",
  "HM","trade","PCA_1","PCA_2"
)

raw_long$Variable <- forcats::fct_relevel(
  raw_long$Variable,
  "MPD",
  "HM","trade","PCA_1","PCA_2"
)

ggplot(data = raw_long_S, aes(x=S, y=meanI, group=Variable, color=Variable))+
  geom_line( alpha=1, colour = "grey20")+ 
  geom_ribbon(aes(x =S, y = meanI, ymin = meanI - sdI, ymax = meanI + sdI, fill = Variable), alpha = .2, 	colour = "grey80")+
  geom_boxplot(data = as.data.frame(raw_long), aes(x=S, y=Importance, fill=Variable), 
               inherit.aes = FALSE,
               position = position_dodge(width = 0.2),
               width = 0.6, 
               alpha = 0.5
               
  )+
  labs(y = "Variable Importance (0-1)", x="")+
  scale_fill_manual(values = palette8)+
  theme_minimal()+
facet_wrap(~Taxa)




## v2

ggplot(data = raw_long_S, aes(x=S, y=meanI, group=Variable, color=Variable))+
  geom_line( alpha=1, colour = "grey20")+ 
  geom_ribbon(aes(x =S, y = meanI, ymin = meanI - sdI, ymax = meanI + sdI, fill = Variable), alpha = .2, 	colour = "grey80")+
  geom_boxplot(data = as.data.frame(raw_long), aes(x=S, y=Importance, fill=Variable), 
               inherit.aes = FALSE,
               position = position_dodge(width = 0.2),
               width = 0.6, 
               alpha = 0.5
               
  )+
  geom_point(data = fig4[fig4$significance == "y", ], aes(Year, pct_change + 0.25), shape = "*", size=4.233, color="black")
+
  labs(y = "Variable Importance (0-1)", x="")+
  scale_fill_manual(values = palette8)+
  theme_minimal()+
  facet_wrap(~Taxa)

