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
library(this.path)

############################
# ##  1) Parameters    ####
############################
#my_path <- "//wsl.localhost/Ubuntu/home/admin_ait/internship-snakemake-2025" # add the working directory in the snakemake workflow
my_path <- dirname(dirname(dirname(this.path::this.dir()))) # Get path to the VI_change folder


data_path   <- "/results/species/XGboost/bind_SHAP.csv"
data_path_metrics   <- "/results/species/XGboost/bind_metrics.csv"
            # facteur S1 / S2

comp_cols<- c("MPD", "PCA_1", "PCA_2", "trade", "HM")

############################
# ## 2) Charge data     ####
############################
raw <- read_csv(paste0(my_path, data_path), show_col_types = FALSE)

raw$Method <- as.factor(raw$Method)
raw$Model <- as.factor(raw$Model)

head(raw)

species_list <- unique(raw$Taxa)


############################
# ## ) Eval metrics    ####
############################

raw_metrics <- read_csv(paste0(my_path,data_path_metrics), show_col_types = FALSE)

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

#data for Table S3
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

overall_df_metrics

############################
# ## ) z-tests and Table2      ####
############################
raw_long <- raw %>% pivot_longer(all_of(comp_cols), names_to = "Variable", values_to = "Values")

df_zt <- data.frame(matrix(ncol=4, nrow=13*5))
colnames(df_zt) <- c("Taxa", "Variable", "z", "p_value")

i= 1
for (sp in species_list) { 
  for (var_name in comp_cols){
    sub1 <- raw %>% 
      filter(Taxa == sp, Model == "Weighted", Method == "XG", S == "S1") %>%
      dplyr::select(var_name)
    
    sub2 <- raw %>% 
      filter(Taxa == sp, Model == "Weighted", Method == "XG", S == "S2") %>%
      dplyr::select(var_name)
    
    ## Previous approach, kept for comparison: Welch t-test across replicates.
    ## To use it instead of the z-test, uncomment these 4 lines and comment out the z-test below.
    # tt <- t.test(sub2, sub1)
    # df_zt$z[i] <- tt$statistic # t value
    # df_zt$p_v[i] <- tt$p.value
    # test_label <- "t"

    ## z-test on the bootstrap replicates: the SD of the 100 bootstrap values is the
    ## standard error of each period's estimate (Efron & Tibshirani 1986, 1993), and the
    ## difference between the two independent periods is tested with z = (S2 - S1) / sqrt(SE1^2 + SE2^2)
    ## (Altman & Bland 2003)
    v1 <- sub1[[1]]
    v2 <- sub2[[1]]
    z <- (mean(v2) - mean(v1)) / sqrt(sd(v1)^2 + sd(v2)^2)
    df_zt$z[i] <- z
    df_zt$p_v[i] <- 2 * pnorm(-abs(z))
    test_label <- "z"

    df_zt$Taxa[i] <- sp
    df_zt$Variable[i] <- var_name
    
    i = i+1
    
  }}

# Assume 'p_values' is a vector of your 65 raw p-values, apply the Benjamini-Hochberg correction
# (controls the false discovery rate). Previous Bonferroni correction kept below for comparison.
adjusted_pBH <- p.adjust(df_zt$p_v, method = "BH")
#adjusted_pbonf <- p.adjust(df_zt$p_v, method = "bonferroni")
# Check which are significant at FDR 0.05
#significant <- adjusted_p > 0.05
df_zt$p_value <- adjusted_pBH
#df_zt$p_value <- adjusted_pbonf

#p-values beow 0.001 to < 0.001
df_zt$p_valueC <- df_zt$p_value
df_zt$p_valueC[df_zt$p_value < 0.001] <- "< 0.001"

# Check of the z-test assumption: the bootstrap values of each species, variable and period
# should look roughly bell-shaped
pdf(paste0(my_path, "/workflow/scripts/Extra/bootstrap_distributions.pdf"), width = 12, height = 30)
print(
  raw_long %>%
    filter(Model == "Weighted", Method == "XG") %>%
    ggplot(aes(x = Values, fill = S)) +
    geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
    facet_wrap(~ Taxa + Variable, scales = "free", ncol = 5) +
    theme_bw()
)
dev.off()

add_stars <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}


#Create table with z-test results
df_wide <- df_zt %>%
  mutate(
    #stars = sapply(p_value, add_stars),
    cell_text = paste0(test_label, " = ", round(z, 2),
                       ", p = ", ifelse(p_value < 0.001, "< 0.001", 
                                        round(p_value, 3)) 
                     )#  " ", stars)
  ) %>%
  pivot_wider(
    names_from = Variable,
    values_from = cell_text,
    id_cols = Taxa
  )

# View result --> Table 3
View(df_wide)

#plot desnities (optional)

# ##Sigficance levels 
pvals_zt <- as.data.frame(df_zt)
# pvals_zt$star <- ""
# pvals_zt$star[pvals_zt$p.value > .05]  <- "ns" #non-significant
# pvals_zt$star[pvals_zt$p.value<= .05]  <- "s"  #significant
# 
# pvals_zt$star <- forcats::fct_relevel(
#   pvals_zt$star,
#   "s",
#   "ns"
# )

# 
# #graph of densities
# raw_long_filtered <- raw_long %>% filter(Model == "Weighted", Method == "XG") %>%
#     dplyr::select(Taxa, Variable, Values, S)
# 
# ggplot(raw_long_filtered, aes(Values, fill=S)) +
#   geom_density(alpha= 0.5) +
# 
#     facet_grid(Taxa ~Variable )

############################
# ## ) (F3) SHAP values ####
############################

##Df with mean   
raw_long_S <- raw_long %>%
  filter(Method == "XG", Model == "Weighted" ) %>%
  group_by(Taxa, S, Variable) %>%
  summarise(meanI=mean(Values), sdI = sd(Values))

##Sigficance levels 
pvals_sig <- as.data.frame(pvals_zt)
pvals_sig$star <- ""
pvals_sig$star[pvals_sig$p_value > .05]  <- "non-significant" #non-significant
pvals_sig$star[pvals_sig$p_value <= .05]  <- "significant"  #significant

pvals_sig$star <- forcats::fct_relevel(
  pvals_sig$star,
  "significant",
  "non-significant"
)

#join to df --> mean per period and significance  
raw_long_S2 <- raw_long_S %>%
  left_join(pvals_sig %>% dplyr::select(Taxa, Variable, star),
            by = c("Taxa", "Variable"))

raw_long_S2$V_name <- raw_long_S2$Variable
raw_long_S2$V_name[raw_long_S2$Variable == "MPD"] <- "Mean Phylogenetic Distance"
raw_long_S2$V_name[raw_long_S2$Variable == "HM"] <- "Human Modification"
raw_long_S2$V_name[raw_long_S2$Variable == "trade"] <- "Trade"


#variables as factor in specific order
raw_long_S2$V_name <- forcats::fct_relevel(raw_long_S2$V_name ,
  "Mean Phylogenetic Distance",
  "Human Modification","Trade","PCA_1","PCA_2"
)


#set palette
palette8 <- c(
  "#A8D46B",   
  "#A50026",
  "#FDB366",   
  "#98CAE1", 
  "#364B9A"   
  
)
#plot
p<- ggplot(data = raw_long_S2, aes(x=S, y=meanI, group=V_name, color=V_name, linetype=star)) +
  geom_point(show.legend = TRUE, size = 2)+
  geom_line()+
  labs(y = "SHAP value", x="")+
  # # scale_fill_manual(values = palette8, labels =c("Mean Phylogenetic Distance",
  # #                                                "Human Modification","Trade","PCA_1","PCA_2"))+
   scale_color_manual(values = palette8 )+ #, drop = FALSE, labels =c("Mean Phylogenetic Distance",
  #                                                               "Human Modification","Trade","PCA_1","PCA_2") ) +
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
        strip.text.x = element_text(size = 11, face = "italic"),
        legend.text =  element_text(size = 12) ,
        legend.title =  element_text(size = 12, face="bold") ,
        legend.box="vertical"
  )+
  scale_x_discrete(labels = c("T1", "T2"))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  facet_wrap(~Taxa, ncol=4, scales = "free",
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
a <- shift_legend3(p)
grid.draw(a)

#pdf("~/Internship UNIL/Figure3.pdf",
pdf(paste0(my_path, "/workflow/scripts/Extra/Figure3.pdf"),
    width = 10, 
    height = 19)
grid.draw(a)
dev.off()


############################
# ##  ) (F1 & FS1) Map S1-S2    ####
############################

#load data and occurrences
load(paste0(my_path, "/data/distribution/AntmapsPolGeneral.RData"))
pathallsp <- "/results/species/AssignVariables/"
allV_files <- list.files(paste0(my_path, pathallsp),  pattern = "_allv\\.csv$", full.names = T)
all_coords <- lapply(allV_files, function(f) {
  df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
  df <- df %>%  filter(Occurrence ==1 )
  
  return(df)
}) %>% bind_rows() #%>% select(BEN, S, Taxa)

#number of occurrences per Period
# Step 1: Calculate occurrences per species (already done in your code)
occurrences <- all_coords %>%
  filter(Occurrence == 1) %>%
  group_by(S, Taxa) %>%
  summarise(n = n(), .groups = "drop")

# Step 2: Calculate mean and SD for S1 and S2
result <- occurrences %>%
  group_by(S) %>%
  summarise(
    Mean = mean(n),
    SD = sd(n),
    Count = n()  # Number of species in the group
  )

#Add geomoetry to each coordinate
all_coords_ben <- all_coords %>% left_join(antmaps_pol %>% 
                                             dplyr::select(BEN, geometry),
                                           by = "BEN") %>% st_as_sf()
#order the periods 
all_coords_ben$S <- forcats::fct_relevel(
  all_coords_ben$S ,
  "S1","S2" 
)

#update periods names (S1 = T1 and S2 = T2)
all_coords_ben <- all_coords_ben %>%
  mutate(
    S_new = case_when(
      S == "S1"  ~ "T1",
      S == "S2" ~ "T2",
      TRUE ~ NA_character_
    )
  )

all_coords <- all_coords %>%
  mutate(
    S_new = case_when(
      S == "S1"  ~ "T1",
      S == "S2" ~ "T2",
      TRUE ~ NA_character_
    )
  )


# obtain BEN x sp
ben_s_sp_df <- all_coords %>%
  dplyr::select(BEN, S, Taxa) %>%
  filter(S %in% c("S1", "S2")) %>%
  group_by(Taxa, BEN) %>%
  summarise(
    # If S1 is present for this Taxa+BEN, keep S1, else S2
    S = if (any(S == "S1")) "S1" else "S2",
    .groups = "drop"
  )

#wide ben 
ben_wide <- ben_s_sp_df %>% 
  dplyr::select(BEN, S, Taxa) %>% 
  mutate(value = 1) %>%                  # mark presence
  distinct(BEN, S, .keep_all = TRUE) %>% # avoid duplicates
  pivot_wider(names_from = S,
              values_from = value,
              values_fill = 0)           # fill missing with 0

#merge antmaps bentities
facet_df <- antmaps_pol %>%
  st_drop_geometry() %>%
  distinct(BEN) %>%
  left_join(ben_wide, by = c("BEN")) %>%
  left_join(antmaps_pol %>% dplyr::select(BEN, geometry), by = "BEN") %>%
  st_as_sf()

df <- facet_df %>%
  mutate(
    S_unique = case_when(
      S1 == 1 & S2 == 1 ~ "T1",
      S1 == 1 & S2 == 0 ~ "T1",
      S1 == 0 & S2 == 1 ~ "T2",
      TRUE ~ NA_character_
    )
  )

df<- df %>% filter(! is.na(Taxa)) 
df$S_unique <- as.factor(df$S_unique)

all_coords_ben <- all_coords_ben %>% arrange(desc(S_new))

# #change projection antmaps_pol
# antmaps_pol_R <- st_transform(antmaps_pol, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

#map LH
LH_map <- ggplot(data=all_coords_ben[all_coords_ben$Taxa == "Linepithema humile",])+
  geom_sf(data = antmaps_pol$geometry, fill = "grey93")+
  geom_sf(aes(fill=as.factor(S_new)))+
  geom_point(data=all_coords[all_coords$Taxa == "Linepithema humile",],
             aes(x=decimallongitude, y=decimallatitude, color=S_new, alpha =S_new), 
             inherit.aes = FALSE)+
  scale_alpha_manual(values = c(1, 0.4), guide = "none")+
  scale_color_manual(
    values = c("#B2182BFF", "#2166ACFF"),
    name = "Occurrence in :"
  )+
  scale_fill_manual(
    values = c("T1" = "#FDDBC7FF", "T2" = "#D1E5F0FF", "S1 & S2"= "#009E73"),
    drop = FALSE,
    na.translate = FALSE,
    name = "First record in : "
  ) +    
  xlab("")+
  ylab("")+
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        strip.text.x = element_text(size = 13, face = "italic"),
        legend.text =  element_text(size = 13) ,
        legend.title =  element_text(size = 13, face="bold") ,
        legend.position = "bottom",
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())+ 
  facet_wrap(~Taxa) + 
  #  ggspatial::annotation_scale(
  #   location = "bl",
  #   bar_cols = c("grey60", "white"),
  #   #text_family = "ArcherPro Book"
  # ) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"))

#pdf("~/Internship UNIL/Figures_M/Figure1.pdf",
pdf(paste0(my_path, "/workflow/scripts/Extra/Figure1.pdf"),
    width = 15, 
    height = 6)
LH_map
dev.off()


#map rest of species
all_map <- ggplot(data=all_coords_ben[all_coords_ben$Taxa != "Linepithema humile",])+
  geom_sf(data = antmaps_pol$geometry, fill = "grey93")+
  geom_sf(aes(fill=as.factor(S_new)))+
  geom_point(data=all_coords[all_coords$Taxa != "Linepithema humile",],
             aes(x=decimallongitude, y=decimallatitude, color=S_new, alpha =S_new), 
             inherit.aes = FALSE)+
  scale_alpha_manual(values = c(1, 0.4), guide = "none")+
  scale_color_manual(
    values = c("#B2182BFF", "#2166ACFF"),
    name = "Occurrence in :"
  )+
  scale_fill_manual(
    values = c("T1" = "#FDDBC7FF", "T2" = "#D1E5F0FF", "S1 & S2"= "#009E73"),
    drop = FALSE,
    na.translate = FALSE,
    name = "First record in : "
  ) +  
  xlab("")+
  ylab("")+
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        strip.text.x = element_text(size = 13, face = "italic"),
        legend.text =  element_text(size = 13) ,
        legend.title =  element_text(size = 13, face="bold") ,
        legend.position = "bottom",
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())+
   facet_wrap(~Taxa, ncol= 3)#+
  # ggspatial::annotation_north_arrow(
  #   location = "bl", which_north = "true",
  #   pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"))

#pdf("~/Internship UNIL/Figures_M/FigureS1.pdf",
pdf(paste0(my_path, "/workflow/scripts/Extra/FigureS1.pdf"),    
width = 12,
height = 12)

# jpeg("~/Internship UNIL/Figures_M/FigureS1.jpg",
#     res= 300,
#     width = 100, 
#     height = 150)
all_map
dev.off()

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
#vif_1 <- vif(dataRaw)
#print(vif_1)


