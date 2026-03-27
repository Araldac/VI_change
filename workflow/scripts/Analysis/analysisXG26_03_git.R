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
library(paletteer)

############################
# ##  1) Paramètres utilisateur    ####
############################

data_path   <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/XGboost/bind_SHAP.csv"       # Chemin vers votre fichier CSV
data_path_metrics   <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/XGboost/bind_metrics.csv"       # Chemin vers votre fichier CSV

species_col <- "Taxa"
pa_col      <- "PA_N"
S_col       <- "S"               # facteur S1 / S2

comp_cols<- c("MPD", "PCA_1", "PCA_2", "trade", "HM")

level_S1 <- "S1"
level_S2 <- "S2"

perc_scale <- 100                # si vos pourcentages sont en 0–100

cluster_mode <- "species_S"      # "species_S", "species_avg", "by_PA"


############################
# ## 2) Chargement     ####
############################
raw <- read_csv(data_path, show_col_types = FALSE)
raw$Method[is.na(raw$Method)] <- "Equal"
head(raw)
species_list <- unqiue(raw$Taxa)


############################
# ## 3) Eval metrics      ####
############################
#### plot
raw_metrics <- read_csv(data_path_metrics, show_col_types = FALSE)

raw
#raw_metrics$Model <- "Equal"
#raw_metrics$Model[raw_metrics$PA_N == "Weighted"] <- "Weighted"

raw_metrics <- raw_metrics %>% mutate(Sensitivity = TP/(FN+TP))
auc_p <- ggplot(raw_metrics, aes(Taxa, roc_auc, fill= S))+
  geom_boxplot()+  geom_point(alpha=0.2)+ theme_bw() + 
  theme(axis.text.x = element_blank(), legend.title = element_blank()) +facet_wrap(methods~model)


acc_p <- ggplot(raw_metrics, aes(Taxa, balanced_acc, fill= S))+
  geom_boxplot()+  geom_point(alpha=0.2)+ theme_bw() + 
  theme(axis.text.x = element_blank(), legend.title = element_blank()) +facet_wrap(methods~model)

sens_p <- ggplot(raw_metrics, aes(Taxa, Sensitivity, fill= S))+
  geom_boxplot()+  geom_point(alpha=0.2)+ theme_bw() + 
  theme(axis.text.x = element_blank(), legend.title = element_blank()) +facet_wrap(methods~model)

time_p <- ggplot(raw_metrics, aes(Taxa, log(time), fill= S))+
  geom_boxplot()+  geom_point(alpha=0.2)+ theme_bw() + 
  theme(axis.text.x = element_blank(), legend.title = element_blank()) +facet_wrap(methods~model)

figure_Eval_R <- ggarrange(acc_p, auc_p, sens_p, time_p, common.legend = TRUE, ncol=4, nrow=1, labels= c("A", "B", "C", "D"), legend ="right")
#ggexport(figure_Eval, filename = "~/Internship UNIL/Figures_SMK/figure_EvalCSI.pdf")
#figure_Eval_R


#LMM to test wether Model and S have an effect on the AUC and accuracy values with species as random factors
emmeans_l <- list()
eval_metrics<- c("roc_auc", "balanced_acc", "Sensitivity", "time")

for( coord in eval_metrics) {
  fit <- lmer(as.formula(paste0(coord, " ~ S+ model*methods + (1|Taxa)")),
       data = raw_metrics)
  fits[[coord]] <- fit
  emmeans_l[[coord]] <- emmeans(fit, pairwise ~ model | methods, adjust = "tukey")$emmeans

}
names(fits) <- eval_metrics
names(emmeans_l) <- eval_metrics

model_evaluations <- tibble(
  Metrics = eval_metrics,
  estimate_S=  sapply(fits, function(m) summary(m)$coefficients[2, "Estimate"]),
  p_value_S = sapply(fits, function(m) summary(m)$coefficients[2, "Pr(>|t|)"]),
  estimate_weighted=  sapply(fits, function(m) summary(m)$coefficients[3, "Estimate"]),
  p_value_weighted = sapply(fits, function(m) summary(m)$coefficients[3, "Pr(>|t|)"]),
  estimate_XG=  sapply(fits, function(m) summary(m)$coefficients[4, "Estimate"]),
  p_value_Model = sapply(fits, function(m) summary(m)$coefficients[4, "Pr(>|t|)"]),
  estimate_weightXG=  sapply(fits, function(m) summary(m)$coefficients[5, "Estimate"]),
  p_value_weightXG = sapply(fits, function(m) summary(m)$coefficients[5, "Pr(>|t|)"])
)

summary(fits[[3]])

############################
# ## 3) Shap values      ####
############################

#### calcualte differences
raw_long <- raw %>% pivot_longer(all_of(comp_cols), names_to = "Variable", values_to = "Values")

### A- LMM ####
coef_sp <- list()
emmeans_sp <- list()
contrasts_sp<- list()
for (sp in species_list) { ##do this with lmer wih PA_N as random
  
  sub <- raw_long %>% filter(Taxa == sp, Model == "Weighted")
  sub$S <- as.factor(sub$S)
  sub$Variable <- as.factor(sub$Variable)
  
  fits <-  lmer(as.formula("Values ~ S*Variable + (1| Replicate)"),
         data = sub)
  print(sp)
  #print(summary(fits))
  #print(anova(fits))
  coef_sp[[sp]] <- fits
  
   emean <- emmeans(fits, pairwise ~ S | Variable, adjust = "tukey")
   #print(emean)  %>% bind_rows
   emean_df <- as.data.frame(emean$emmeans)
   emean_df$Taxa <- sp 
   emmeans_sp[[sp]] <- emean_df
   
   contrast_df <- as.data.frame(emean$contrasts)
   contrast_df$Taxa <- sp 
   contrasts_sp[[sp]] <-contrast_df 
 
}

emmeans_long <- emmeans_sp %>% bind_rows
contrast_long <- contrasts_sp %>% bind_rows
contrast_long_p <- contrast_long
contrast_long_p$estimate[contrast_long_p$p.value > 0.05] <- 0

#because i am inerested in S2- S1 not S1-S2
contrast_long_p$estimate <- contrast_long_p$estimate * (-1)

### B- Rank ####
#rank difference of emmeans 
wide_mean <- emmeans_long %>% select(Taxa, S, emmean, Variable) %>% pivot_wider(names_from = Variable, values_from = emmean)
sub_rank <- lapply(1:nrow(wide_mean), function(i) rank(wide_mean[i, comp_cols])) %>% bind_rows() %>% mutate(S = wide_mean$S, Taxa = wide_mean$Taxa)

#setting rank differences
rank_diff <- sub_rank %>%
  pivot_longer(cols = all_of(comp_cols),
               names_to = "Variable",
               # names_prefix = "rank_",
               values_to = "Rank") %>% select(Taxa, Variable, Rank, S) %>%
  pivot_wider(names_from = S, values_from = Rank, names_prefix = "rank_") %>%
  mutate(RankDiff = rank_S2 - rank_S1) %>% select(Taxa, Variable, RankDiff, rank_S1 , rank_S2)
rank_diff$RankDiff[rank_diff$RankDiff == 1] <- "+1"
rank_diff$RankDiff[rank_diff$RankDiff == 2] <- "+2"
rank_diff$RankDiff[rank_diff$RankDiff == 3] <- "+3"
rank_diff$RankDiff[rank_diff$RankDiff == 0] <- "."

rank_diff$RankDiff <- as.character(rank_diff$RankDiff)

rank_diff$RankDiff <- forcats::fct_relevel(
  rank_diff$RankDiff,
  "-3", "-2", "-1", ".", 
  "+1","+2","+3"
)

rank_palette <- c("#B10318", "#D82526", "#F26C64","white", "#9FCD99", "#69B764" ,"#309343"   )


## plot emmeas S1-S2
emmean_p <- ggplot(emmeans_long, aes(Variable, emmean, color  =S))+
  geom_point(position = position_dodge(.5), size=3)+
  ylim(0,5.7)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),size=1, width=0.3, position = position_dodge(.5))+
  theme(legend.position = "horizontal", legend.location = "bottom")+
  theme_bw()+
  geom_text(data=rank_diff, aes(x=Variable, y= 5.5, colour=RankDiff, label= RankDiff), show.legend = FALSE,  inherit.aes = FALSE,  size=5) +
  #coord_flip()+
 # theme(legend.position = "none")+
  scale_colour_manual(values = c( "#364B9A",  "#98CAE1", rank_palette ))+
  facet_wrap(~Taxa)

shift_legend3 <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}
shift_legend3(emmean_p)


#plot contrasts and rank differences 
heatmapp_rank <- ggplot(rank_diff, aes(Variable, Taxa, fill = RankDiff)) +
  geom_tile(color = "grey80") +
  geom_point(data = as.data.frame(contrast_long_p), aes(Variable, Taxa, color= estimate), size = 8, inherit.aes = FALSE)+
  scale_fill_manual(values = rank_palette) +
  scale_color_paletteer_c("ggthemes::Classic Orange-Blue")+
  labs(
    #title = title_c,
    y = "Species",
    x="Variables"
  ) + 
  #geom_text(aes(label=stars), color="black", size=5) + 
  theme_minimal(base_size = 14) +
  theme(legend.position= "bottom", axis.text.x = element_text(angle = 45, hjust = 1))



############################
# ##  4) Carte S1-S2    ####
############################

pathallsp <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/AssignVariables/"
allV_files <- list.files(pathallsp,  pattern = "_allv\\.csv$", full.names = T)
all_coords <- lapply(allV_files, function(f) {
  df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
  df <- df %>%  filter(Occurrence ==1 )
  
  return(df)
}) %>% bind_rows() #%>% select(BEN, S, Taxa)


all_coords_ben <- all_coords %>% left_join(ant_ll %>% 
                                             select(BEN, geometry),
                                           by = "BEN") %>% st_as_sf()


  all_coords_ben$S <- forcats::fct_relevel(
    all_coords_ben$S ,
    "S1","S2" 
  )

# obtain BEN x sp
ben_s_sp_df <- all_coords %>%
  select(BEN, S, Taxa) %>%
  filter(S %in% c("S1", "S2")) %>%
  group_by(Taxa, BEN) %>%
  summarise(
    # If S1 is present for this Taxa+BEN, keep S1, else S2
    S = if (any(S == "S1")) "S1" else "S2",
    .groups = "drop"
  )

#wide ben 
ben_wide <- ben_s_sp_df %>% select(BEN, S, Taxa) %>% 
  mutate(value = 1) %>%                  # mark presence
  distinct(BEN, S, .keep_all = TRUE) %>% # avoid duplicates
  pivot_wider(names_from = S,
              values_from = value,
              values_fill = 0)           # fill missing with 0

#merge antmaps bentities
facet_df <- ant_ll %>%
  st_drop_geometry() %>%
  distinct(BEN) %>%
  left_join(ben_wide, by = c("BEN")) %>%
  left_join(ant_ll %>% select(BEN, geometry), by = "BEN") %>%
  st_as_sf()

df <- facet_df %>%
  mutate(
    S_unique = case_when(
      S1 == 1 & S2 == 1 ~ "S1",
      S1 == 1 & S2 == 0 ~ "S1",
      S1 == 0 & S2 == 1 ~ "S2",
      TRUE ~ NA_character_
    )
  )

df<- df %>% filter(! is.na(Taxa)) 
df$S <- as.factor (df$S)

all_coords_ben <- all_coords_ben %>% arrange(desc(S))

#map LH
ggplot(data=all_coords_ben[all_coords_ben$Taxa == "Linepithema humile",])+
  geom_sf(data = ant_ll$geometry, fill = "grey93")+
  geom_sf(aes(fill=as.factor(S)))+
  geom_point(data=all_coords[all_coords$Taxa == "Linepithema humile",],
             aes(x=decimallongitude, y=decimallatitude, color=S, alpha =S), 
             inherit.aes = FALSE)+
  scale_alpha_manual(values = c(1, 0.4), guide = "none")+
  scale_color_manual(
    values = c("#B2182BFF", "#2166ACFF"),
    name = "Occurrence in :"
  )+
  scale_fill_manual(
    values = c("S1" = "#FDDBC7FF", "S2" = "#D1E5F0FF", "S1 & S2"= "#009E73"),
    drop = FALSE,
    na.translate = FALSE,
    name = "First record in : "
  ) +    
  xlab("")+
  ylab("")+
  theme_bw() +
  theme(axis.text = element_blank(), 
                     axis.title = element_blank(),
                     legend.position = "bottom")+ 
  facet_wrap(~Taxa) 


#map rest 
ggplot(data=all_coords_ben[all_coords_ben$Taxa != "Linepithema humile",])+
  geom_sf(data = ant_ll$geometry, fill = "grey93")+
  geom_sf(aes(fill=as.factor(S)))+
  geom_point(data=all_coords[all_coords$Taxa != "Linepithema humile",],
             aes(x=decimallongitude, y=decimallatitude, color=S, alpha =S), 
             inherit.aes = FALSE)+
  scale_alpha_manual(values = c(1, 0.4), guide = "none")+
  scale_color_manual(
    values = c("#B2182BFF", "#2166ACFF"),
    name = "Occurrence in :"
  )+
  scale_fill_manual(
    values = c("S1" = "#FDDBC7FF", "S2" = "#D1E5F0FF", "S1 & S2"= "#009E73"),
    drop = FALSE,
    na.translate = FALSE,
    name = "First record in : "
  ) +    
  xlab("")+
  ylab("")+
 
  theme_bw() + theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = "bottom")+ 
  facet_wrap(~Taxa, ncol=3, nrow=4) 










###############3333brouillon

##plot 
raw_long_S <- raw_long %>% group_by(Taxa, S, Variable) %>% summarise(meanI=mean(Values), sdI = sd(Values)) 

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




##add sigficance levels 
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
p <- ggplot(data = raw_long_S, aes(x=S, y=meanI, group=Variable, color=Variable)) +
  geom_ribbon(aes(x =S, y = meanI, ymin = meanI - sdI, ymax = meanI + sdI, fill = Variable), alpha = .2, 	colour = "grey80")+
  # geom_boxplot(data = as.data.frame(raw_long), aes(x=S, y=Importance, fill=Variable), 
  #              inherit.aes = FALSE,
  #              position = position_dodge(width = 0.2),
  #              width = 0.6, 
  #              alpha = 0.5
  #              
  # )+
  geom_point(show.legend = TRUE, size = 2)+
  geom_errorbar(aes(x =S, ymax = meanI + sdI, ymin = meanI - sdI),width = 0.2 )+
  #geom_text(data=out, aes(x= 2.2, y= meanI_S2, label=star, colour= Variable),  show.legend = FALSE,  inherit.aes = FALSE,  size=6) +
  labs(y = "SHAP value", x="")+
  scale_fill_manual(values = palette8)+
  scale_color_manual(values = palette8, drop = FALSE) +
  theme_bw()+
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  facet_wrap(~Taxa, ncol=3, scales = "free" )

# theme_classic()# Source - https://stackoverflow.com/a/58734961
# Posted by Artem Sokolov
# Retrieved 2026-03-11, License - CC BY-SA 4.0

shift_legend3 <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}
shift_legend3(p)




##plot visualization 2

wide_mean <- raw_long_S %>% select(Taxa, S, meanI, Variable) %>% pivot_wider(names_from = Variable, values_from = meanI)
sub_rank <- lapply(1:nrow(wide_mean), function(i) rank(wide_mean[i, comp_cols])) %>% bind_rows() %>% mutate(S = wide_mean$S, Taxa = wide_mean$Taxa)

rank_diff <- sub_rank %>%
  pivot_longer(cols = all_of(comp_cols),
               names_to = "Variable",
              # names_prefix = "rank_",
               values_to = "Rank") %>% select(Taxa, Variable, Rank, S) %>%
  pivot_wider(names_from = S, values_from = Rank, names_prefix = "rank_") %>%
  mutate(RankDiff = rank_S2 - rank_S1) %>% select(Taxa, Variable, RankDiff)


ggplot(data = raw_long_S, aes(x=Variable, y=meanI, group=S, color=S)) +
  
  geom_point(show.legend = TRUE, size = 2)+
  geom_errorbar(aes(x =Variable, ymax = meanI + sdI, ymin = meanI - sdI),width = 0.2 )+
  #geom_text(data=out, aes(x= 2.2, y= meanI_S2, label=star, colour= Variable),  show.legend = FALSE,  inherit.aes = FALSE,  size=6) +
  labs(y = "SHAP value", x="")+
  geom_text(data=rank_diff, aes(x=Variable, y= 5.1,  label= RankDiff),  show.legend = FALSE,  inherit.aes = FALSE,  size=6) +
  #scale_y_continuous(breaks=c(0, 0.2, .4, .6), labels =c(0, 20, 40, 60))+
  #scale_x_discrete(labels= c("S1", "S2"), limits= 1:2)+
  #scale_x_continuous(lim= c(0.95, 2.25), breaks = c(1, 2), labels= c("S1", "S2"))+
  scale_fill_manual(values = palette8)+
  coord_flip()+
  scale_color_manual(values = palette8, drop = FALSE) +
  theme_bw()+
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  facet_wrap(~Taxa, ncol=3 )
