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

############################
# ##  1) Paramètres utilisateur    ####
############################

data_path   <- "Internship UNIL/results/all_occ_PCA.csv"       # Chemin vers votre fichier CSV
species_col <- "Taxa"
pa_col      <- "PA_N_new"
S_col       <- "S"               # facteur S1 / S2

comp_cols<- c("MPD", "PCA_1", "PCA_2", "trade", "HM")
comp_cols_r <- c( "bio_2", "bio_5", "bio_9",
                "bio_12", "bio_15", "trade", "HM","MPD")

level_S1 <- "S1"
level_S2 <- "S2"

perc_scale <- 100                # si vos pourcentages sont en 0–100

cluster_mode <- "species_S"      # "species_S", "species_avg", "by_PA"

############################
# ## 2) Chargement     ####
############################

raw <- read_csv(data_path, show_col_types = FALSE)

raw <- raw %>%
  mutate(across(all_of(c(species_col, pa_col, S_col)), as.factor))

# Forcer l'ordre S1, S2
raw[[S_col]] <- factor(raw[[S_col]], levels = c(level_S1, level_S2))


data <- raw
############################
### 3) PLR            ####
############################

  #---------------------------------------------------------------
  ## 1. PREPARE DATA ####
  #---------------------------------------------------------------
  df <- data %>%
    mutate(
      across(all_of(c(species_col, pa_col, S_col)), as.factor)
    )
  
  df[[S_col]] <- factor(df[[S_col]], levels = c(level_S1, level_S2))
  
  X <- df %>% select(all_of(comp_cols)) %>% as.matrix()
  X <- X / rowSums(X)  # closure
  
  #---------------------------------------------------------------
  ## 2. PAIRWISE LOG-RATIOS via pwlr()  ####
  #---------------------------------------------------------------
  pwlr_raw <- pwlr(acomp(X)) %>%
    as.matrix() %>% 
    as.data.frame()
  
  colnames(pwlr_raw) <- make.names(colnames(pwlr_raw))
  
  
  pwlr_meta <- bind_cols(
    raw %>% select(all_of(c(species_col, S_col, pa_col))),
    pwlr_raw
  )
  
  
  plr_names <- colnames(pwlr_raw)
  species_list <- levels(pw_meta[[species_col]])
  
  #---------------------------------------------------------------
  ## 3. PER-SPECIES MIXED MODELS  ####
  #---------------------------------------------------------------
  per_species_pvals <- list()
  delta_plr_list <- list()
  plr_mean_s1 <- list()
  plr_mean_s2 <- list()
  
  for (sp in species_list) {
    
    sub <- pwlr_meta %>% filter(.data[[species_col]] == sp)
    
    fits <- lapply(plr_names, function(coord) {
      lmer(as.formula(paste0(coord, " ~ ", S_col, " + (1|", pa_col, ")")),
           data = sub)
    })
    names(fits) <- plr_names
    
    # p-values
    pvals <- sapply(fits, function(m) anova(m)[S_col, "Pr(>F)"])
    pvals_holm <- p.adjust(pvals, method = "holm")
    
    per_species_pvals[[sp]] <- tibble(
      species = sp,
      plr = plr_names,
      p_value = pvals,
      p_value_holm = pvals_holm
    )
    
    # ΔPLR = S2 - S1
    deltas <- sapply(fits, function(m) {
      emm <- emmeans(m, specs = S_col)
      as.numeric(summary(contrast(emm, method = list(S2_S1 = c(-1, 1))))$estimate)
    })
    
    delta_plr_list[[sp]] <- tibble(
      species = sp,
      !!!setNames(as.list(deltas), plr_names)
    )
    
    s1 <- sapply(fits, function(m) {
      emm <- emmeans(m, specs = S_col)
      summary(emm)$emmean[1]
    })
    
    s2 <- sapply(fits, function(m) {
      emm <- emmeans(m, specs = S_col)
      summary(emm)$emmean[2]
    })
    
    plr_mean_s1[[sp]] <- tibble(
      S= "S1",
      species = sp,
      !!!setNames(as.list(s1), plr_names)
    )
    
    plr_mean_s2[[sp]] <- tibble(
      S= "S2",
      species = sp,
      !!!setNames(as.list(s2), plr_names)
    )
    
    cat("finished ", sp, "\n" )
  }
  
  
  per_species_pvals_df <- bind_rows(per_species_pvals)
  delta_plr_df <- bind_rows(delta_plr_list)
  s_plr_df <- bind_rows(plr_mean_s1)
  s_plr_df <- rbind(s_plr_df, bind_rows(plr_mean_s2))
  
  #---------------------------------------------------------------
  ## 4. CLUSTERING ON ΔPLR  ####
  #---------------------------------------------------------------
  D <- dist(delta_plr_df[, -1], method = "euclidean")
  hc <- hclust(D, method = "ward.D2")
  
  hc$labels <- delta_plr_df$species
  
  hc_dendro <- as.dendrogram(hc)
  
  library(ggdendro)
  
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
  
  #---------------------------------------------------------------
  ## 5. HEATMAP OF ΔPLR  #### 
  #---------------------------------------------------------------
  delta_long <- delta_plr_df %>%
    pivot_longer(-species, names_to = "plr", values_to = "delta")
  
  # heatmap_plot <- ggplot(delta_long, aes(plr, species, fill = delta)) +
  #   geom_tile(color = "grey40") +
  #   scale_fill_gradient2(low = "#2166AC", mid = "white",
  #                        high = "#B2182B", midpoint = 0) +
  #   theme_minimal(base_size = 12) +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #   labs(title = "ΔPLR heatmap (S2 - S1)", fill = "ΔPLR")
  
  ##add the p-values
  delta_long$p.values <- per_species_pvals_df$p_value
  delta_long$stars <- cut(delta_long$p.values, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))  # Create column of significance labels
  
  # Extract the order of the tips in the dendrogram
  hc_order <- order.dendrogram(hc_dendro)
  # Order the levels according to their position in the cluster
  delta_long$species <- factor(x = delta_long$species,
                               levels = delta_plr_df$species[hc_order], 
                               ordered = TRUE)
  
  
  heatmap_plot  <- ggplot(delta_long, aes(plr, species, fill = delta)) +
    geom_tile(color = "grey80") +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0
    ) +
    labs(
      title = "ΔILR heatmap (S2 – S1)",
      x = "ILR coordinate",
      y = "Species",
      fill = "ΔILR"
    ) +
    scale_y_discrete(limits = delta_plr_df$species[hc_order])+
    geom_text(aes(label=stars), color="black", size=5) + 
    theme_minimal(base_size = 14) +
    theme(legend.position= "left", axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  # All together for bio 
  # grid.newpage()
  # print(heatmap_plot, 
  #       vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
  # print(dendro_plot, 
  #       vp = viewport(x = 0.89, y = 0.55, width = 0.2, height = 0.81))
  # 
  
  # All together for pca
  grid.newpage()
  print(heatmap_plot,
        vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
  print(dendro_plot,
        vp = viewport(x = 0.89, y = 0.58, width = 0.2, height = 0.70))

  
  
  #---------------------------------------------------------------
  ## 6. PCA   ####
  #---------------------------------------------------------------
  
  #on ΔPLR
  pca <- prcomp(delta_plr_df[, -1], scale. = TRUE)
  
  var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
  pc1_lab <- paste0("PC1 (", sprintf("%.1f", 100 * var_exp[1]), "%)")
  pc2_lab <- paste0("PC2 (", sprintf("%.1f", 100 * var_exp[2]), "%)")
  
  pca_df <- data.frame(
    species = delta_plr_df$species,
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2]
  )
  
  pca_plot <- ggplot(pca_df, aes(PC1, PC2, label = species)) +
    geom_point(size = 3) +
    geom_text(nudge_y = 0.05) +
    theme_minimal() +
    labs(title = "PCA of ΔPLR", x = pc1_lab, y = pc2_lab,)
  
  #on s1 and 2
  library(easyCODA)
  lras.raw <- easyCODA::LR(as.data.frame(raw[,comp_cols]),  weight=FALSE)
  lras <- bind_cols(
    raw %>% select(all_of(c(species_col, S_col, pa_col))),
    lras.raw$LR
  )
  PLOT.LRA(lras)
  
  pca_s <- prcomp(s_plr_df[, plr_names], scale. = TRUE)
  
  # Compute variance explained for axis labels
  var_exp <- (pca_s$sdev^2) / sum(pca_s$sdev^2)
  pc1_lab <- paste0("PC1 (", sprintf("%.1f", 100 * var_exp[1]), "%)")
  pc2_lab <- paste0("PC2 (", sprintf("%.1f", 100 * var_exp[2]), "%)")
  
  # PCA scores for plotting
  pca_df_s <- data.frame(
    species = s_plr_df$species,
    S = s_plr_df$S,
    PC1 = pca_s$x[, 1],
    PC2 = pca_s$x[, 2]
  )
  
  pca_df_s_long <- pca_df_s %>% pivot_wider(
    names_from = S,
    values_from = c(PC1, PC2),
    names_sep = "_"
  ) 
    
    
   ggplot(pca_df_s, aes(PC1, PC2, label = species, col=S)) +
    geom_point(size = 3) +
    geom_text(nudge_y = 0.05) +
    theme_minimal() +
    labs(title = "PCA of S1 and S2")
  
  
  ggplot() +
    # Arrows per species (S1 centroid -> S2 centroid)
    geom_segment(
      data = pca_df_s_long,
      aes(x = PC1_S1, y = PC2_S1, xend = PC1_S2, yend = PC2_S2, color = species),
      linewidth = 1,
      arrow = arrow(length = unit(3.5, "mm"), type = "closed")
    ) +
    geom_text(
      data = pca_df_s_long,
      aes(x = PC1_S1, y = PC2_S1, label = species, color = species),
      hjust = -0.05, vjust = 0.5, size = 3, show.legend = FALSE
    ) +
    labs(
      x = pc1_lab, y = pc2_lab,
      title = "PCA on PLR values S1 and S2",
      #subtitle = "Points: PA_N replicates (colored by species, shaped by S). Arrows: Species centroid shift S1 → S2.",
      color = "Species", shape = "Set (S)"
    ) +
    #coord_equal() +
    theme_minimal(base_size = 12)+
    theme(legend.position='none')
  
  ############################
  # ## 3) Mantel test  ####
  ############################ 
  

D_ilr <- dist(delta_plr_df[, -1], method = "euclidean")
sp_info <- sp_info %>% arrange(factor(species, delta_ilr$species))
D_env <- dist(scale(sp_info[, c("Nben","Tben","S1","S2","Date")]))

library(vegan)
mantel(D_ilr, D_env,  permutations = 9999)

data <- sp_info
vegan::mantel(D_ilr, dist(scale(data$Nben)),  permutations = 9999)
vegan::mantel(D_ilr, dist(scale(data$Tben)),  permutations = 9999)
vegan::mantel(D_ilr, dist(scale(data$S1)),  permutations = 9999)
vegan::mantel(D_ilr, dist(scale(data$S2)),  permutations = 9999)
vegan::mantel(D_ilr, dist(scale(data$Date)),  permutations = 9999)
vegan::mantel(D_ilr, phylo_dist ,  permutations = 9999)


mrm_delta <- ecodist::MRM(D_ilr ~  dist(scale(data$Nben)) + dist(scale(data$Tben)) + dist(scale(data$S1)) + dist(scale(data$S2)) + dist(scale(data$Date)) , nperm=9999)
mrm_delta


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

taxa_list <- sort(unique(ben_s_sp_df$Taxa))

facet_df <- ant_ll %>%
  st_drop_geometry() %>%
  distinct(BEN) %>%
  crossing(Taxa = taxa_list) %>%
  left_join(ben_s_sp_df, by = c("BEN", "Taxa")) %>%
  left_join(ant_ll %>% select(BEN, geometry), by = "BEN") %>%
  st_as_sf()

for( species in species_list){
  sp_df <- ben_s_sp_df %>% filter(Taxa == species)
  
  native_df <- antmaps_dis[[species]] %>% 
    filter(category == "N") %>% 
    select(gid) %>% 
    add_column(S = "N", Taxa = species) %>%
    rename("BEN" = gid) %>% left_join(ant_ll %>% select(BEN, geometry), by = "BEN") %>% 
    select(BEN, Taxa, S, geometry) %>%
    st_as_sf()
  
  facet_df  <- rbind(facet_df, native_df)
}

ggplot(facet_df) +
  geom_sf(aes(fill = S), color = "grey40", size = 0.01) +
  scale_fill_manual(
    values = c(S1 = "#f4c195", S2 = "#ee2e31", N= "#679100"),
    drop = FALSE,
    na.translate = FALSE,
    name = "Status"
  ) +
  facet_wrap(~ Taxa) +
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

