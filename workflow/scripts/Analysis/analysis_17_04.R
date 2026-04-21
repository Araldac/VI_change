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
library(fitdistrplus)
library(betareg)
library(mgcv)

############################
# ##  1) Parameters    ####
############################

data_path   <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/XGboost/bind_SHAP.csv"       # Chemin vers votre fichier CSV
data_path_metrics   <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025/results/species/XGboost/bind_metrics.csv"       # Chemin vers votre fichier CSV
            # facteur S1 / S2

comp_cols<- c("MPD", "PCA_1", "PCA_2", "trade", "HM")

############################
# ## 2) Charge data     ####
############################
raw <- read_csv(data_path, show_col_types = FALSE)

raw$Method <- as.factor(raw$Method)
raw$Model <- as.factor(raw$Model)

head(raw)

species_list <- unique(raw$Taxa)


############################
# ## ) Eval metrics    ####
############################

raw_metrics <- read_csv(data_path_metrics, show_col_types = FALSE)

raw_metrics <- raw_metrics %>% mutate(Sensitivity = TP/(FN+TP))

#Taxa and S 
df_metrics <- raw_metrics %>% filter(model =="Weighted", methods == "XG") %>% 
  group_by(Taxa, S) %>% 
  summarise(
    across(
      all_of(c("roc_auc", "balanced_acc", "Sensitivity")), 
      list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

df_metrics

#Overall S 
overall_df_metrics <-   raw_metrics %>% filter(model =="Weighted", methods == "XG") %>% 
  group_by(S) %>% 
  summarise(
    across(
      all_of(c("roc_auc", "balanced_acc", "Sensitivity")), 
      list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

############################
# ## ) LMM             ####
############################

#disitrbution of values per variable
ggplot(raw_long, aes(Values, color=Variable))+geom_density()

raw_long <- raw %>% pivot_longer(all_of(comp_cols), names_to = "Variable", values_to = "Values")

coef_sp <- list()
emmeans_sp <- list()
contrasts_sp<- list()

#LMM for each species
for (sp in species_list) { 
  
  sub <- raw_long %>% filter(Taxa == sp, Model == "Weighted", Method == "XG")
  sub$S <- as.factor(sub$S)
  sub$Variable <- as.factor(sub$Variable)
  
  fits <-  lmer(as.formula("Values ~ S*Variable + (1| Replicate)"),
         data = sub)
  
  print(sp)
  #print(summary(fits))
  #print(anova(fits))
  coef_sp[[sp]] <- summary(fits)
  
  #plots if needed
  #plot_redres(fits, type = "std_cond")
  #plot_resqq(fits)

  
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

#remove non significant contrasts
contrast_long_p$estimate[contrast_long_p$p.value > 0.05] <- 0

#because i am inerested in S2- S1 not S1-S2
contrast_long_p$estimate <- contrast_long_p$estimate * (-1)


#get abs sum of contrasts
t2 <- contrasts_sp %>% bind_rows() %>% dplyr::select( contrast, estimate, Variable, p.value, Taxa)  %>% group_by (Taxa) %>% pivot_wider(names_from = contrast, values_from = estimate, names_prefix = "A")

sum_diff <- data.frame(matrix(nrow=13, ncol=0))
sum_diff$Taxa <- species_list  

for (sp in species_list){
  sum_diff$sum[sum_diff$Taxa == sp] <-  sum(abs(t2$`AS1 - S2`[t2$Taxa == sp]))
}
print(sum_diff)

sum_diff_v <- data.frame(matrix(nrow=5, ncol=0))
sum_diff_v$Variable <- comp_cols  
for (variable_v in comp_cols){
  sum_diff_v$sum[sum_diff_v$Variable == variable_v] <-  sum(abs(t2$`AS1 - S2`[t2$Variable == variable_v]))
}

print(sum_diff_v)
############################
# ## ) (F2X) SHAP values ####
############################

##Df with mean   
raw_long_S <- raw_long %>% 
  filter(Method == "XG", Model == "Weighted" ) %>% 
  group_by(Taxa, S, Variable) %>% 
  summarise(meanI=mean(Values), sdI = sd(Values)) 

#variables as factor in specific order
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

##Sigficance levels 
pvals_sig <- as.data.frame(contrast_long_p)
pvals_sig$star <- ""
pvals_sig$star[pvals_sig$p.value > .05]  <- "ns" #non-significant
pvals_sig$star[pvals_sig$p.value<= .05]  <- "s"  #significant

pvals_sig$star <- forcats::fct_relevel(
  pvals_sig$star,
  "s",
  "ns"
)

#join to df
raw_long_S2 <- raw_long_S %>%
  left_join(pvals_sig %>% dplyr::select(Taxa, Variable, star),
            by = c("Taxa", "Variable"))

#set palette
palette8 <- c(
  "#A8D46B",   
  "#A50026",
  "#FDB366",   
  "#98CAE1", 
  "#364B9A"   
  
)
#plot
p<- ggplot(data = raw_long_S2, aes(x=S, y=meanI, group=Variable, color=Variable, linetype=star)) +
  geom_point(show.legend = TRUE, size = 2)+
  geom_line()+
  labs(y = "SHAP value", x="")+
  scale_fill_manual(values = palette8)+
  scale_color_manual(values = palette8, drop = FALSE) +
  theme_bw()+
  labs(
    color = "Variable",
    linetype = "Change significance") +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) +
  theme(legend.direction = "horizontal",
        aspect.ratio = 1, 
        legend.position = "bottom",
        strip.text.x = element_text(size = 11),
        legend.text =  element_text(size = 11) ,
        legend.title =  element_text(size = 11, face="bold") ,
        legend.box="vertical"
  )+
  scale_x_discrete(labels = c("T1", "T2"))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  facet_wrap(~Taxa, ncol=5, scales = "free",
             labeller = label_wrap_gen(width=10))

# Source - https://stackoverflow.com/a/58734961
# Posted by Artem Sokolov
# Retrieved 2026-03-11, License - CC BY-SA 4.0
shift_legend3 <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}
shift_legend3(p)











############################
# ## ) (FSX) Rank & Contrast ####
############################

#rank difference of emmeans 
wide_mean <- emmeans_long %>% dplyr::select(Taxa, S, emmean, Variable) %>% pivot_wider(names_from = Variable, values_from = emmean)
sub_rank <- lapply(1:nrow(wide_mean), function(i) rank(wide_mean[i, comp_cols])) %>% bind_rows() %>% mutate(S = wide_mean$S, Taxa = wide_mean$Taxa)

#setting rank differences
rank_diff <- sub_rank %>%
  pivot_longer(cols = all_of(comp_cols),
               names_to = "Variable",
               # names_prefix = "rank_",
               values_to = "Rank") %>% dplyr::select(Taxa, Variable, Rank, S) %>%
  pivot_wider(names_from = S, values_from = Rank, names_prefix = "rank_") %>%
  mutate(RankDiff = rank_S2 - rank_S1) %>% dplyr::select(Taxa, Variable, RankDiff, rank_S1 , rank_S2)

# order rank difference
rank_diff$RankDiff<- as.character(rank_diff$RankDiff)
rank_diff$RankDiff <- forcats::fct_relevel(
  rank_diff$RankDiff,
  "-3", "-2", "-1", "0",
  "1","2","3"
)

rank_palette <- c("#B10318", "#D82526", "#F26C64","white", "#9FCD99", "#69B764" ,"#309343"   )

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







#mod_meth_ord <- list()
mod_meth_ord#[["RF_Equal"]] <- sum_diff %>% arrange(sum)
t2 <- contrasts_sp %>% bind_rows() %>% select( contrast, estimate, Variable, p.value, Taxa)  %>% group_by (Taxa) %>% pivot_wider(names_from = contrast, values_from = estimate, names_prefix = "A")

sum_diff <- data.frame(matrix(nrow=13, ncol=0))
sum_diff$Taxa <- species_list  

  for (sp in species_list){
    sum_diff$sum[sum_diff$Taxa == sp] <-  sum(abs(t2$`AS1 - S2`[t2$Taxa == sp]))
    
  }
  sum(abs(t2$`AS1 - S2`))
list_diff[[species]] <- sum_diff

############################
# ##  ) (F1 & FS1) Map S1-S2    ####
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


#map rest of species
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

############################
# ##  ) VIF and Variable correlation    ####
############################
library(ade4)
dataRaw <- na.omit(all_coords[, comp_cols]) #from section ) (F1 & FS1) Map S1-S2

#clustering of variables 
distancem <- 1-abs(cor(dataRaw, method="spearman")) #creer une matrice de distance
clustdist <- hclust(as.dist(distancem), method="complete") #clusterizer les variables
#par(mar=c(1,4,2,1))
plot(clustdist, cex=1) #observer la clusterization
abline(h=0.3, lty=2, col="red", lwd=2)

#correlation plot
corrplot::corrplot.mixed(cor(dataRaw, use = "complete"), order = 'AOE')

#pca 
pca_c <- dudi.pca(dataRaw, center=TRUE, scale=TRUE, nf=length(comp_cols), scannf = FALSE )
s.corcircle(pca_c$co)

#VIF of variables
vif_1 <- vif(dataRaw)
print(vif_1)


