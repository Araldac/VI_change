###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Calculate cophenetic distance between the alien sp and native ones
# Date: 03/12/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
distributionData_N <- args[1]
antmaps_pol <-args[2]
tree <- args [3]
sp_name <- args [4]
#output_cophenetic <- args[3]
#output_filtered_Ndist <- args[4]
output_MPD <- args[5]

#download/install packages
pkg <- c("ape", "dplyr")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

################################################################################
#Code
################################################################################

load(distributionData_N)
cat("length before filtering \n")

load(antmaps_pol)


#read tre file
ant <- ape::read.tree(tree)

#create cophenetic distance
cophen_ant <- cophenetic.phylo(ant)

#filter antmaps to the genera that is present in the tree

filtered_antmaps <- list()
for (i in names(N_antmaps_dis)){
  #print(i)
  name_sp <- i
  gen <- strsplit(name_sp, " ")[[1]][1]
  
  if (gen %in% colnames(cophen_ant)){
    
    filtered_antmaps[[i]] <- N_antmaps_dis[[i]]
  }
  
}



##save csv of cophentic distance
#write.csv(cophen_ant, file = output_cophenetic)

#save new antmaps filtered
#save(filtered_antmaps, file = output_filtered_Ndist)

cat("length before filtering \n")
length(N_antmaps_dis)
cat("length after filtering \n")
length(filtered_antmaps)

##### #### bentity x species matrix #### ####

benxsp_MPD.df <- data.frame(matrix(ncol= 1, nrow=length(antmaps_pol$BEN)))
row.names(benxsp_MPD.df) <- antmaps_pol$BEN
colnames(benxsp_MPD.df) <- sp_name

## 1) Precompute genera
inv_genera      <-   setNames(sub(" .*", "", sp_name), sp_name)
species_names   <- names(filtered_antmaps)
species_genera  <- setNames(sub(" .*", "", species_names), species_names)

## 2) Build a long occurrence table: (gid, species)
occ_long <- do.call(
  rbind,
  mapply(function(df, sp) {
    if (is.null(df) || nrow(df) == 0 || is.null(df$gid)) return(NULL)
    data.frame(gid = as.character(df$gid), species = sp, stringsAsFactors = FALSE)
  }, filtered_antmaps, species_names, SIMPLIFY = FALSE)
)

# Reduce to presence/absence (remove duplicate gid–species rows if any)
if (!is.null(occ_long) && nrow(occ_long) > 0) {
  occ_long <- unique(occ_long)
} else {
  stop("No valid (gid, species) occurrences found in `filtered_antmaps`.")
}

## 3) BEN → list of species present
species_by_gid <- split(occ_long$species, occ_long$gid)  # species appear once per gid

## 4) Convert to BEN → vector of genera (with species multiplicity preserved)
genera_by_gid <- lapply(
  species_by_gid,
  function(sps) {
    # For each species in this BEN, take its genus (no 'unique': preserves species weighting)
    gs <- species_genera[sps]
    # Filter to genera present in the cophenetic matrix
    gs[!is.na(gs) & gs %in% rownames(cophen_ant)]
  }
)

## 5) Keep only invasive genera that exist in the cophenetic matrix columns
inv_unique_genera <- unique(inv_genera)
inv_unique_genera <- inv_unique_genera[inv_unique_genera %in% colnames(cophen_ant)]
if (length(inv_unique_genera) == 0) {
  stop("None of the invasive genera are present as columns in `cophen_ant`.")
}

## 6) Compute per-BEN mean distances to each invasive genus (column means of sub-matrices)
# Result: a list BEN -> named numeric vector (names = invasive genera)
means_by_gid <- lapply(
  genera_by_gid,
  function(gs) {
    if (length(gs) == 0) {
      # No genera in this BEN: return NA for every invasive genus
      out <- rep(NA_real_, length(inv_unique_genera))
      names(out) <- inv_unique_genera
      return(out)
    }
    subm <- cophen_ant[gs, inv_unique_genera, drop = FALSE]
    m <- colMeans(subm, na.rm = TRUE)
    # If an invasive genus column is all NA, colMeans returns NaN; coerce to NA
    m[is.nan(m)] <- NA_real_
    m
  }
)

## 7) Fill the output matrix by mapping invasive species -> their genus mean for each BEN
bens <- as.character(antmaps_pol$BEN)

# Ensure output dims & names
if (is.null(rownames(benxsp_MPD.df))) rownames(benxsp_MPD.df) <- bens
if (is.null(colnames(benxsp_MPD.df))) colnames(benxsp_MPD.df) <- sp_name

# Prepare a default NA vector for BENs with no data
na_vec <- rep(NA_real_, length(inv_unique_genera))
names(na_vec) <- inv_unique_genera

for (ben in bens) {
  means_vec <- means_by_gid[[as.character(ben)]]
  if (is.null(means_vec)) means_vec <- na_vec
  
  # Map species columns via their genus
  benxsp_MPD.df[ben, ] <- means_vec[ inv_genera[colnames(benxsp_MPD.df)] ]
}


write.csv(benxsp_MPD.df , file = output_MPD)