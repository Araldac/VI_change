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
library(gtable)
library(cowplot)
library(grid)
library(ggpattern)

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
  geom_violin()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()
#+labs(title = "AUC of ROC for CSI and PCA RF models")

# Accuracy
acc_p <- ggplot(raw, aes(Taxa, Accuracy, fill =S))+
  geom_violin()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()
#+labs(title = "Model Accuracy of CSI and PCA RF models")

#Kappa
Kapp_p <- ggplot(raw, aes(Taxa, Kappa, fill =S))+
  geom_violin()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
  theme_minimal()

sens_p <-  ggplot(raw, aes(Taxa, Sensitivity, fill =S))+
  geom_violin()+   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  
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
sp_info2 <- tibble::rownames_to_column(sp_info2, "Taxa")

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






################################################################################
## (new) Plot 1 - Changes in the variable importance and their rank ####
################################################################################

#needs the per_species_pvals_mean_df in the heatmap section for the significance levels

#set palette
palette8 <- c(
  "#A8D46B",   # Group1 S1 / S2
  "#A50026",
  "#FDB366",   # Group2 S1 / S2
  "#98CAE1", 
  "#364B9A"   # Group3 S1 / S2
  #  "#e7298a", "#e7298a80"    # Group4 S1 / S2
)

#variables as factor 
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



#add signficiance star and their placve in the plot
pvals_sig <- per_species_pvals_mean_df_sig

pvals_sig$star <- ""
pvals_sig$star[pvals_sig$p_value<= .05]  <- "*"
pvals_sig$star[pvals_sig$p_value <= .01]  <- "**"
pvals_sig$star[pvals_sig$p_value <= .001] <- "***"

pvals_sig2 <- pvals_sig %>%
  mutate(Taxa = species, Variable = variable) %>%
  select(Taxa, Variable, everything())

# Make a small table with S2 meanI per Taxa × Variable
meanI_S2 <- raw_long_S %>%
  filter(S == "S2") %>%
  select(Taxa, Variable, meanI) %>%                 # keep one row per Taxa×Variable (adjust if needed)
  rename(meanI_S2 = meanI)

# Now you can join safely
out <- pvals_sig2 %>%
  inner_join(meanI_S2, by = c("Taxa", "Variable"))

out$Variable <- forcats::fct_relevel(
  out$Variable,
  "MPD",
  "HM","trade","PCA_1","PCA_2"
)

library(dplyr)

raw_long_S2 <- raw_long_S %>%
  left_join(out %>% select(Taxa, Variable, p_value),
            by = c("Taxa", "Variable")) %>%
  mutate(ribbon = if_else(p_value > 0.05 | is.na(p_value), 0, sdI), mean_ribbon = if_else(p_value > 0.05 | is.na(p_value), 0, meanI) )



#plot
p <- ggplot(data = raw_long_S2, aes(x=S_num, y=meanI, group=Variable, color=Variable))+
  #geom_line( alpha=1, colour = "grey20", show.legend = FALSE)+ 
  geom_ribbon(aes(x =S_num, y = mean_ribbon, ymin = mean_ribbon - ribbon, ymax = mean_ribbon + ribbon, fill = Variable), alpha = .2, 	colour = "grey80")+
  # geom_boxplot(data = as.data.frame(raw_long), aes(x=S, y=Importance, fill=Variable), 
  #              inherit.aes = FALSE,
  #              position = position_dodge(width = 0.2),
  #              width = 0.6, 
  #              alpha = 0.5
  #              
  # )+
  geom_point(show.legend = TRUE, size = 2)+
  geom_errorbar(aes(x =S_num, ymax = meanI + sdI, ymin = meanI - sdI))+
  geom_text(data=out, aes(x= 2.2, y= meanI_S2, label=star, colour= Variable),  show.legend = FALSE,  inherit.aes = FALSE,  size=6) +
  labs(y = "Variable Importance %", x="")+
  scale_y_continuous(breaks=c(0, 0.2, .4, .6), labels =c(0, 20, 40, 60))+
  #scale_x_discrete(labels= c("S1", "S2"), limits= 1:2)+
  scale_x_continuous(lim= c(0.95, 2.25), breaks = c(1, 2), labels= c("S1", "S2"))+
  scale_fill_manual(values = palette8)+
  scale_color_manual(values = palette8, drop = FALSE) +
  theme_bw()+
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  facet_wrap(~Taxa, ncol=3)

# theme_classic()# Source - https://stackoverflow.com/a/58734961
# Posted by Artem Sokolov
# Retrieved 2026-03-11, License - CC BY-SA 4.0

shift_legend3 <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}

#put the ñegend in empty grd space
p_leg <- (shift_legend3(p))

# ggplot2::ggsave(filename =  "~/Internship UNIL/Figures_SMK/VariableImp13sp.jpg",
#                 plot = p_leg,
#                 width = 310, 
#                 height = 190, units = "mm",
                device = "jpg", dpi = 500)
jpeg(filename =  "~/Internship UNIL/Figures_SMK/VariableImp13sp.jpeg",
     width = 190, 
     height = 300, units = "mm",
      res = 500)
#p
(shift_legend3(p))
dev.off()

################################################################################
## (new) Plot 2 - Direction of variables 0-1  ####
################################################################################

# allv_pca_long for data 
# list_models for significance values

p_v <- lapply(list_models, function(m){
  p <- summary(m)
  return(p$coefficients[2, "Pr(>|t|)"])
} )


p_val <- data.frame(matrix(ncol= 0, nrow=5))
p_val$Variable <- names(p_v)
p_val$p <- t(p_v %>% bind_rows())
p_val$group1 <- as.factor(0)
p_val$group2 <- as.factor(1)
p_val[,".y."] <- c(270, 17, 5.5, 2.7e+15, 30)
p_val$p.signif <- "ns"
p_val$p.signif[p_val$p<= .05]  <- "*"
p_val$p.signif[p_val$p <= .01]  <- "**"
p_val$p.signif[p_val$p <= .001] <- "***"

allv_pca_mean <-  allv_pca_long %>% group_by(Taxa,Occurrence, Variable) %>% summarise(across(Value, mean), .groups = "drop")

palette13 <- c(brewer.pal(n = 12, name = "Paired"), "#666666")

p2 <- ggplot(allv_pca_long, aes(Occurrence, Value, fill=Occurrence))+
  geom_boxplot(show.legend = FALSE, alpha = .5)+ theme_minimal()+ 
  geom_line(data = allv_pca_mean, aes(x= Occurrence, y = Value, group = Taxa, color= Taxa), linewidth = 1.3,  inherit.aes = FALSE)+
  scale_fill_manual(values = c("white", "grey"))+
  geom_point(data = allv_pca_mean, aes(x= Occurrence, y = Value, group = Taxa, color= Taxa), show.legend = TRUE, size = 2)+
  #scale_color_manual(values = wes_palette("Zissou1", 13, type = "continuous"))+
  scale_color_manual(values = palette13) +
  theme_bw()+
  labs(x = "", y = "")+
  theme(legend.position = "bottom")+
  facet_wrap(~Variable, ncol=5, scales = "free")+
   scale_x_discrete(labels= c("Background points", "Presences"))+
  stat_pvalue_manual(p_val, label = "p.signif", y.position = ".y.",tip.length = 0.01)

jpeg(filename =  "~/Internship UNIL/Figures_SMK/Vchangeglobal.jpeg",
     width = 310, 
     height = 190, units = "mm",
     res = 500)
p2
dev.off()

################################################################################
## (new) Map 1 - q25-q75 pca1 and pca 2  ####
################################################################################

pca11 <- unname(quantile(allv_pca$PCA_1[allv_pca$Occurrence == 1], .25))
pca12 <- unname(quantile(allv_pca$PCA_1[allv_pca$Occurrence == 1], .75))

pca21 <- unname(quantile(allv_pca$PCA_2[allv_pca$Occurrence == 1], .25))
pca22 <- unname(quantile(allv_pca$PCA_2[allv_pca$Occurrence == 1], .75))

library(terra)
r<- terra::rast("//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/variables/Climate_PCA.tif")

r1 <- r[[1]]
r2 <- r[[2]]
r1 <- terra::ifel(r1 <= pca12 & r1 >= pca11, 1, 0)
r2 <- terra::ifel(r2 <= pca22 & r2 >= pca21, 2, 0)

filter_r <- r1+r2
plot(filter_r)

library(terra)

# Supposons que pca11, pca12, pca21, pca22 sont des valeurs numériques définies

r_classified <- terra::ifel(
  r[[1]] >= pca11 & r[[1]] <= pca12 & r[[2]] >= pca21 & r[[2]] <= pca22, 
  3,  # Cas où les deux conditions sont vraies (entre les deux bornes)
  terra::ifel(
    r[[1]] >= pca11 & r[[1]] <= pca12, 
    1,  # Seulement dans les bornes de la couche 1
    terra::ifel(
      r[[2]] >= pca21 & r[[2]] <= pca22, 
      2,  # Seulement dans les bornes de la couche 2
      NA  # Aucune des deux conditions (ou 0 si tu préfères)
    )
  )
)

plot(r[[1]], col="grey", legend=FALSE); 
plot(r_classified, add=TRUE); 
points(allv_pca$decimallongitude[allv_pca$Occurrence==1], allv_pca$decimallatitude[allv_pca$Occurrence==1], pch=20, cex=.3, col = rgb(.7, 0, 0, 0.4))


################################################################################
## (new) Table 1 - Evaluation metrics of RF  ####
################################################################################



# raw for the data on the evaluations

eval_means <- raw %>% group_by(Taxa) %>% 
summarise(across(
  .cols = c(AUC, Kappa, Accuracy, Sensitivity), 
  .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
  .names = "{col}_{fn}"
))

library(tidyverse)

# Suponiendo que tu dataframe se llama 'df'
df_formateado <- eval_means %>%
  # 1. Seleccionamos solo las columnas necesarias (Taxa, S y las métricas)

  # 2. Unimos Mean y SD con el símbolo ±
  # Usamos sprintf para formatear los decimales si es necesario (ej. 3 decimales)
  mutate(
    AUC = paste0(sprintf("%.3f", AUC_Mean ), " ± ", sprintf("%.3f", AUC_SD)),
    Kappa = paste0(sprintf("%.3f",Kappa_Mean), " ± ", sprintf("%.3f", Kappa_SD)),
    Accuracy = paste0(sprintf("%.3f", Accuracy_Mean), " ± ", sprintf("%.3f", Accuracy_SD)),
    Sensitivity = paste0(sprintf("%.3f", Sensitivity_Mean), " ± ", sprintf("%.3f", Sensitivity_SD))
  ) %>%
  
  # 5. Seleccionamos las columnas finales
  select(Taxa, AUC, Kappa, Accuracy, Sensitivity) %>% inner_join(sp_info2, by = "Taxa") 

# Ver el resultado
print(df_formateado)

formattable::formattable(df_formateado)



################################################################################
## plot 3 - heatmaps  ####
################################################################################
p_vals_hm <- per_species_pvals_mean_df 
p_vals_hm$estimate[p_vals_hm$p_value > 0.05 ] <- 0

p1 <- ggplot(p_vals_hm, aes(variable, species, fill = estimate)) +
  geom_tile(color = "grey80") +
  scale_fill_gradient2(
    low = "#740580", mid = "white", high = "#F2C447",
    midpoint = 0
  ) +
  labs(
    title = "Effect size between S2-S1",
    y = "Species",
    x="Variables"
  ) + 
  scale_x_discrete(limits = col_w_val)+
  scale_y_discrete(limits = sp_info$species)+
  #geom_text(aes(label=stars), color="black", size=5) + 
  theme_minimal(base_size = 14) +
  theme(legend.position= "left", axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_blank())


### arrange data ####
#take mean variable importance
mean_wide <- raw_long_S %>% group_by(Taxa, S) %>% select(Taxa, S, Variable, meanI) %>% pivot_wider(names_from = Variable, values_from = meanI)
#appply ranking S1 and S2

library(dplyr)
library(tidyr)
library(purrr)

# variables to rank
vars <- c("HM", "MPD", "PCA_1", "PCA_2", "trade")

# 1. Compute within-row ranks for each Taxa/S
ranked <- mean_wide %>%
  rowwise() %>%
  mutate(across(all_of(vars),
                ~ rank(c_across(all_of(vars)), ties.method = "first")[which(vars == cur_column())],
                .names = "rank_{col}")) %>%
  ungroup()

# 2. Pivot longer to compute rank differences
rank_diff <- ranked %>%
  pivot_longer(cols = starts_with("rank_"),
               names_to = "Variable",
               names_prefix = "rank_",
               values_to = "Rank") %>% select(Taxa, Variable, Rank, S) %>%
  pivot_wider(names_from = S, values_from = Rank, names_prefix = "rank_") %>%
  mutate(RankDiff = rank_S2 - rank_S1)

# 3. Add value of S2 for each variable
value_S2 <- mean_wide %>%
  filter(S == "S2") %>%
  pivot_longer(cols = all_of(vars),
               names_to = "Variable",
               values_to = "Value_S2") %>%
  select(Taxa, Variable, Value_S2)

# 4. Final dataset
final_df <- rank_diff %>%
  left_join(value_S2, by = c("Taxa", "Variable")) %>%
  select(Taxa, Variable, RankDiff, Value_S2)

final_df
#### plot ####
p2 <- ggplot(final_df, aes(Variable, Taxa)) +
  geom_tile(aes(fill= RankDiff)) +
  geom_point(aes(size = Value_S2))+
  scale_fill_gradient2(
    low = "#D55E00", mid = "white", high = "#56B4E9",
    midpoint = 0
  ) +
  labs(
    title = "Rank and % importance in S2",
    y = "",
    x=""
  ) + 
  scale_y_discrete(limits = sp_info$species)+
  scale_x_discrete(limits = col_w_val)+
  #geom_text(aes(label=stars), color="black", size=5) + 
  theme_minimal(base_size = 14) +
  theme(legend.position= "right", axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(p1,p2, ncol=2, widths = c(0.9, 1.2))


############################
# ##  4) Carte S1-S2   ####
############################

pathallV <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/AssignVariables/"
allV_files <- list.files(pathallV,  pattern = "_allv\\.csv$", full.names = T)
all_coords <- lapply(allV_files, function(f) {
  df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
  df <- df %>%  filter(Occurrence ==1 )
  
  return(df)
}) %>% bind_rows() %>% select(BEN, S, Taxa)

# all maps toegther


ben_s_sp_df <- all_coords %>%
  select(BEN, S, Taxa) %>%
  filter(S %in% c("S1", "S2")) %>%
  group_by(Taxa, BEN) %>%
  summarise(
    # If S1 is present for this Taxa+BEN, keep S1, else S2
    S = if (any(S == "S1")) "S1" else "S2",
    .groups = "drop"
  )

taxa_list <- sort(unique(ben_s_sp_df$Taxa))


ben_wide <- ben_s_sp_df %>% select(BEN, S) %>% 
  mutate(value = 1) %>%                  # mark presence
  distinct(BEN, S, .keep_all = TRUE) %>% # avoid duplicates
  pivot_wider(names_from = S,
              values_from = value,
              values_fill = 0)           # fill missing with 0

ben_wide


facet_df <- ant_ll %>%
  st_drop_geometry() %>%
  distinct(BEN) %>%
  left_join(ben_wide, by = c("BEN")) %>%
  left_join(ant_ll %>% select(BEN, geometry), by = "BEN") %>%
  st_as_sf()

facet_df[is.na(facet_df)] <- 0 

library(ggpattern)
ggplot(facet_df) +
  ggpattern::geom_sf_pattern(aes(fill=as.factor(S1), pattern = as.factor(S2)),
                             color = "grey90",
                             size = 0.01,
                             pattern_density = 1.1, 
                             pattern_fill    = 'black',
                             pattern_colour  = 'black')+

  labs(
    title = "BEN status by species",
    subtitle = "S1 overrides S2 within each (Taxa, BEN)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x=element_blank(), axis.text.y = element_blank())


#if needed to do one by one plot

# Keep only needed columns and only S1/S2
ben_s_sp_df <- all_coords %>%
  select(BEN, S, Taxa) %>%
  filter(S %in% c("S1", "S2")) %>%
  group_by(Taxa, BEN) %>%
  summarise(
    # If S1 is present for this Taxa+BEN, keep S1, else S2
    S = if (any(S == "S1")) "S1" else "S2",
    .groups = "drop"
  )

ant_ll <- st_transform(antmaps_pol, 4326)  # WGS84 (lon/lat)

plot_species_map <- function(species) {
  sp_df <- ben_s_sp_df %>% filter(Taxa == species)
  
  native_df <- antmaps_dis[[species]] %>% filter(category == "N") %>% select(gid) %>% add_column(Native = 1)
  
  # left_join keeps all BEN polygons; S will be NA where species not present
  geom_sp <- ant_ll %>%
    left_join(sp_df, by = "BEN")
  
  geom_sp$S[geom_sp$BEN %in% native_df$gid] <- "N"
  
  plot <- ggplot(geom_sp) +
    geom_sf(aes(fill = S), color = "grey60", size = 0.1) +
    scale_fill_manual(
      values = c(S1 = "#e41a1c", S2 = "#ff7f00", N = "green"),
      drop = FALSE,
      na.translate = FALSE,
      name = "Status"
    ) +
    labs(
      title = paste0("BEN status for ", species),
      subtitle = "S1 overrides S2 where both exist"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  plot
  return(plot)
}




