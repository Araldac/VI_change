###############################################################################
# Project: Intersnship UNIL 2025
# Athor: Aitana
# Description: Historical data: convert to current countries and interpolates NA
# Date: 26/11/2025
###############################################################################

## Load arguments ####

args <- commandArgs(trailingOnly = TRUE)
trade1 <- args[1]
trade2 <- args[2]
trade3 <- args[3]
antmaps_pol <-args[4]
output <- args[5]

pkg <- c("readxl", "countrycode", "sf", "dplyr", "tidyr", "purrr", "pbapply", "zoo")#, "", "", "")
lapply(pkg, function(x) require(x, quietly = TRUE, character.only = TRUE))

load(antmaps_pol)

################################################################################
#Functions
################################################################################

#### #### List of ISO from historical to current countries  #### ####
historical_to_iso3_split <- list(
  # Italian states → Italy (unified in 1861)
  "2SICIL" = c("ITA"),
  "ROME" = c("ITA"),
  "SARD" = c("ITA"),
  "TUSC" = c("ITA"),
  
  # German states → Germany (unified in 1871)
  "PRUS" = c("DEU"),
  "BREMEN" = c("DEU"),
  "HAMBG" = c("DEU"),
  "HANOV" = c("DEU"),
  "HANS" = c("DEU"),
  "LUBECK" = c("DEU"),
  "MECKL" = c("DEU"),
  "OLDENB" = c("DEU"),
  "SCHLES" = c("DEU"),
  "ZOLL" = c("DEU"),
  "EDEU" = c("DEU"),  # East Germany → unified Germany
  "WDEU" = c("DEU"),  # West Germany → unified Germany
  "DANTZ" = c("POL"),  # Danzig → Poland
  
  # Alsace-Lorraine → France (as requested)
  "ALSLOR" = c("FRA"),
  
  # Austria-Hungary → ALL successors (dissolved 1918)
  "AUTHUN" = c("AUT", "HUN", "CZE", "SVK", "HRV", "SVN", "BIH", "ROU"),
  
  # Czechoslovakia → Czech Republic + Slovakia (split 1993)
  "CZSK" = c("CZE", "SVK"),
  
  # Ottoman Empire → Turkey + Arab states (dissolved 1922)
  "OTTO" = c("TUR", "SYR", "IRQ", "JOR", "PSE", "LBN", "SAU", "YEM"),
  
  # USSR → 15 republics (dissolved 1991)
  "USSR" = c("RUS", "UKR", "BLR", "KAZ", "UZB", "TKM", "KGZ", "TJK", 
             "GEO", "ARM", "AZE", "LTU", "LVA", "EST", "MDA"),
  
  # British India → India + Pakistan + Bangladesh (as requested)
  "GBRIND" = c("IND", "PAK", "BGD"),
  
  # British Mediterranean → Malta + Cyprus
  "GBRMEDI" = c("MLT", "CYP"),
  
  # British Somaliland → Somalia
  "GBRSOM" = c("SOM"),
  
  # British Papua → Papua New Guinea
  "GBRPAPUA" = c("PNG"),
  
  # British Borneo → Malaysia + Brunei--> in antmaps Brunei is with borneo in IDN
  "GBRBORNEO" = c("MYS", "IDN"),
  
  # British West Africa → Nigeria, Ghana, Sierra Leone, Gambia
  "GBRWAFRI" = c("NGA", "GHA", "SLE", "GMB"),
  
  # British Africa (general, pre-1939, excluding specific colonies)
  # Main British colonies in Africa not in GBRWAFRI:
  "GBRAFRI" = c("ZAF", "KEN", "UGA", "TZA", "ZMB", "ZWE", "MWI", "BWA", "LSO", "SWZ"),
  
  # South African colonies → South Africa
  "ZAFCAP" = c("ZAF"),
  "ZAFNAT" = c("ZAF"),
  "ZAFORA" = c("ZAF"),
  "ZAFTRA" = c("ZAF"),
  
  # British West Indies → Jamaica, Barbados, Trinidad, etc.
  "GBRWINDIES" = c("JAM", "TTO", "GUY", "LA"),
  
  # Rhodesias → Zimbabwe + Zambia
  "RHOD" = c("ZWE", "ZMB"),
  
  # Australian colonies → Australia (federated 1901)
  "AUSNSW" = c("AUS"),
  "AUSQUE" = c("AUS"),
  "AUSSTH" = c("AUS"),
  "AUSTAS" = c("AUS"),
  "AUSVIC" = c("AUS"),
  "AUSWST" = c("AUS"),
  
  # Canadian regions → Canada
  "CANPRINCED" = c("CAN"),
  "CANQBCONT" = c("CAN"),
  "NFLD" = c("CAN"),
  
  # French West Africa → Senegal, Mali, Mauritania, Niger, Burkina Faso, Guinea, Côte d'Ivoire, Benin
  "FRAAOF" = c("SEN", "MLI", "MRT", "NER", "BFA", "GIN", "CIV", "BEN"),
  
  # French Equatorial Africa → Gabon, Congo, CAR, Chad
  "FRAAEF" = c("GAB", "COG", "CAF", "TCD"),
  
  # French Africa (general) → Algeria, Tunisia, Morocco
  "FRAAFRI" = c("DZA", "TUN", "MAR"),
  
  # French East Africa → Djibouti
  "FRAEAFRI" = c("DJI"),
  
  # French Indochina → Vietnam, Laos, Cambodia
  "INDOCHI" = c("VNM", "LAO", "KHM"),
  "FRAIND" = c("VNM", "LAO", "KHM"),
  
  # French Oceania → French Polynesia
  "FRAOCEA" = c("PYF"),
  
  # French West Indies → (Martinique, Guadeloupe) but not saved in 
  "FRAWINDIES" = c("LA"),
  
  # Combined French Africa
  "AOFAEF" = c("SEN", "MLI", "MRT", "NER", "BFA", "GIN", "CIV", "BEN", "GAB", "COG", "CAF", "TCD"),
  
  # Syria-Lebanon mandate → Syria + Lebanon
  "SYRLBN" = c("SYR", "LBN"),
  
  # Portuguese Africa → Angola, Mozambique
  "PRTAFRI" = c("AGO", "MOZ"),
  
  # Portuguese West Africa → Angola
  "PRTWAFRI" = c("AGO"),
  
  # Portuguese India → India (Goa)
  "PRTIND" = c("IND"),
  
  # German Africa → Tanzania (German East Africa)
  "DEUAFRI" = c("TZA"),
  
  # German West Africa → Namibia
  "DEUWAF" = c("NAM"),
  
  # German New Guinea → Papua New Guinea
  "DEUNEWGUI" = c("PNG"),
  
  # Togo-Cameroon → Togo + Cameroon
  "TGOCMR" = c("TGO", "CMR"),
  
  # Italian East Africa → Eritrea, Ethiopia, Somalia
  "ITAEAFRI" = c("ERI", "ETH", "SOM"),
  
  # Italian Somaliland → Somalia
  "ITASOM" = c("SOM"),
  
  # Other territories
  "FIUME" = c("HRV"),
  "TRIEST" = c("ITA"),
  "SARRE" = c("DEU"),
  "TANGER" = c("MAR"),
  "MARESP" = c("MAR"),
  
  # Rwanda-Urundi → Rwanda + Burundi
  "RWABDI" = c("RWA", "BDI"),
  
  # Malay states → Malaysia
  "FEDMYS" = c("MYS"),
  "UNFEDMYS" = c("MYS"),
  
  # Straits Settlements → Singapore + Malaysia--> in antmaps both are together
  "STRAITS" = c("MYS"),
  
  # Islands
  "AZORES" = c("PRT"),
  "MADEIRA" = c("PRT"),
  "CANARY" = c("ESP"),
  "CHISL" = c("GBR"),
  
  # Zanzibar → Tanzania
  "ZANZ" = c("TZA"),
  
  # US territories → USA
  "ALASKA" = c("USA"),
  "TEXAS" = c("USA"),
  
  # Asian territories
  "MANCHU" = c("CHN"),  # Manchuria → China
  "KWANTU" = c("CHN"),  # Kwantung → China
  
  # Middle East
  "ADEN" = c("YEM"),
  
  # Barbary States → Libya, Tunisia, Algeria
  "BARBAR" = c("LBY", "TUN", "DZA"),
  
  # Split kingdoms
  "UKNLD" = c("NLD", "BEL"),  # United Kingdom Netherlands → Netherlands + Belgium
  "SWENOR" = c("SWE", "NOR"),  # Sweden-Norway → Sweden + Norway
  "ULSTER" = c("GBR"),  # Northern Ireland → UK
  
  #All islands of the caribbean assign to LA--> Lesser antilles
  "BRB"=c("LA"),
  "GRD"=c("LA"),
  "VCT"=c("LA"),
  "LCA"=c("LA"),
  "DMA"=c("LA"),
  "KNA"=c("LA"),
  "ATG"=c("LA"),
  "MTQ"=c("LA"),
  "GLP"=c("LA")
)


#### #### Find the area of the countries to divide the trade amongst area #### ####
isolist_to_rel_area2 <- function(list_iso, antmaps_pol) {
  f1 <- function(x) {
    match_rows <- antmaps_pol[antmaps_pol$ISO == x, ]
    if (nrow(match_rows) == 0) {
      warning(sprintf("⚠ ISO '%s' not found in antmaps_pol", x))
      return(NA_real_)
    } else {
      return(sum(st_area(match_rows$geometry), na.rm = TRUE))
    }
  }
  
  list_areas <- vapply(list_iso, f1, numeric(1))
  
  # Handle missing or zero total area
  if (all(is.na(list_areas)) || sum(list_areas, na.rm = TRUE) == 0) {
    warning("⚠ All area values are NA or zero — returning equal weights.")
    rel_prop <- rep(1 / length(list_iso), length(list_iso))
    names(rel_prop) <- list_iso
    return(rel_prop)
  }
  
  total_a <- sum(list_areas, na.rm = TRUE)
  rel_prop <- list_areas / total_a
  names(rel_prop) <- list_iso
  return(rel_prop)
}

#### #### convert trade with proportional split trgsrding area of the coutntry #### ####

convert_historical_to_iso3 <- function(trade_data, 
                                       origin_col = "iso_o", 
                                       dest_col = "iso_d",
                                       flow_col = "FLOW",
                                       year_col = "year") {
  
  
  cat(sprintf("Input records: %s\n", format(nrow(trade_data), big.mark = ",")))
  cat(sprintf("Years covered: %d - %d\n", min(trade_data[[year_col]]), 
              max(trade_data[[year_col]])))
  cat("\nProcessing...\n\n")
  
  start_time <- Sys.time()
  
  # Step 1: Calculate weights for each historical code
  cat("→ Calculating split weights (using year when ALL successors present)...\n\n")
  
  all_weights_origin <- list()
  all_weights_dest <- list()
  
  historical_codes_to_process <- names(historical_to_iso3_split)
  
  cat("Processing ORIGIN codes:\n")
  for (hist_code in historical_codes_to_process) {
    successors <- historical_to_iso3_split[[hist_code]]
    
    if (length(successors) > 1) {
      # Calculate weights for origin
      all_weights_origin[[hist_code]] <- isolist_to_rel_area2(successors, antmaps_pol)
    }
  }
  
  cat("\nProcessing DESTINATION codes:\n")
  for (hist_code in historical_codes_to_process) {
    successors <- historical_to_iso3_split[[hist_code]]
    
    if (length(successors) > 1) {
      # Calculate weights for destination
      all_weights_dest[[hist_code]] <- isolist_to_rel_area2(successors, antmaps_pol)
    }
  }


  # Step 2: Process each row
  cat("→ Expanding trade flows to successor states...\n\n")
  
  pb <- txtProgressBar(min = 0, max = nrow(trade_data), style = 3, width = 50)
  
  result_list <- list()
  
  for (i in 1:nrow(trade_data)) {
    row <- trade_data[i, ]
    
    origin_code <- row[[origin_col]]
    dest_code <- row[[dest_col]]
    
    # Get successor codes
    origin_successors <- historical_to_iso3_split[[origin_code]]
    dest_successors <- historical_to_iso3_split[[dest_code]]
    
    # If not in mapping, check if already ISO3
    if (is.null(origin_successors)) {
      origin_successors <- c(origin_code)
    }
    if (is.null(dest_successors)) {
      dest_successors <- c(dest_code)
    }
    
    # Get weights
    if (length(origin_successors) > 1) {
      origin_weights <- all_weights_origin[[origin_code]]
    } else {
      origin_weights <- c(1.0)
      names(origin_weights) <- origin_successors
    }
    
    if (length(dest_successors) > 1) {
      dest_weights <- all_weights_dest[[dest_code]]
    } else {
      dest_weights <- c(1.0)
      names(dest_weights) <- dest_successors
    }
    
    # Create all combinations
    for (orig_succ in origin_successors) {
      for (dest_succ in dest_successors) {
        orig_weight <- origin_weights[orig_succ]
        dest_weight <- dest_weights[dest_succ]
        
        # Calculate proportional flow
        split_flow <- row[[flow_col]] * orig_weight * dest_weight
        
        # Create new row
        new_row <- row
        new_row$iso3_origin <- orig_succ
        new_row$iso3_destination <- dest_succ
        new_row$FLOW_split <- split_flow
        new_row$origin_weight <- orig_weight
        new_row$dest_weight <- dest_weight
        new_row$original_origin <- origin_code
        new_row$original_destination <- dest_code
        
        result_list[[length(result_list) + 1]] <- new_row
      }
    }
    
    if (i %% max(1, floor(nrow(trade_data) / 100)) == 0) {
      setTxtProgressBar(pb, i)
    }
  }
  
  close(pb)
  
  # Combine all results
  result <- bind_rows(result_list)
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat("\n\n✓ CONVERSION COMPLETED!\n\n")
  cat("--- Summary Statistics ---\n")
  cat(sprintf("Input records:  %s\n", format(nrow(trade_data), big.mark = ",")))
  cat(sprintf("Output records: %s\n", format(nrow(result), big.mark = ",")))
  cat(sprintf("Expansion ratio: %.2fx\n", nrow(result) / nrow(trade_data)))
  cat(sprintf("Processing time: %.2f seconds\n\n", elapsed))
  
  cat("--- Code Conversion Summary ---\n")
  cat(sprintf("Unique origin codes (original): %d\n", 
              length(unique(trade_data[[origin_col]]))))
  cat(sprintf("Unique origin codes (ISO3): %d\n", 
              length(unique(result$iso3_origin))))
  cat(sprintf("Unique destination codes (original): %d\n", 
              length(unique(trade_data[[dest_col]]))))
  cat(sprintf("Unique destination codes (ISO3): %d\n", 
              length(unique(result$iso3_destination))))
  
  # Check for unmapped codes
  unmapped_origin <- setdiff(
    unique(trade_data[[origin_col]]),
    c(names(historical_to_iso3_split), unique(result$iso3_origin))
  )
  unmapped_dest <- setdiff(
    unique(trade_data[[dest_col]]),
    c(names(historical_to_iso3_split), unique(result$iso3_destination))
  )
  
  if (length(unmapped_origin) > 0) {
    cat("\n⚠ WARNING: Unmapped origin codes found:\n")
    print(unmapped_origin)
  }
  
  if (length(unmapped_dest) > 0) {
    cat("\n⚠ WARNING: Unmapped destination codes found:\n")
    print(unmapped_dest)
  }
  
  # Show sample split weights
  cat("\n--- Sample Split Weights (based on earliest trade) ---\n")
  sample_codes <- names(all_weights_origin)[1:min(5, length(all_weights_origin))]
  for (code in sample_codes) {
    cat(sprintf("\n%s → \n", code))
    weights <- all_weights_origin[[code]]
    for (j in 1:length(weights)) {
      cat(sprintf("  %s: %.3f (%.1f%%)\n", names(weights)[j], 
                  weights[j], weights[j] * 100))
    }
  }
  
  return(result)
}


#### #### interpolate the NAs with the data before and after #### ####
# Interpolate by year, separately for each original (historical) country pair, so that
# historical entities merged into the same ISO3 pair (e.g. Newfoundland and Canada)
# are not interpolated into each other.
# Rows are sorted by year first, because the trade table is not in year order.
# Years before the first or after the last available value stay NA.
process_pair <- function(data, pair) {
  if (pair[1] != pair[2]) {
    result <- data %>%
      filter(iso3_origin == pair[1], iso3_destination == pair[2]) %>%
      group_by(original_origin, original_destination) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(FLOW_split = na.approx(FLOW_split, x = year, na.rm = FALSE)) %>%
      ungroup()
    return(result)
  } else {
    return(NULL)
  }
}

################################################################################
#Code
################################################################################

#### #### laod data #### ####
#load data
bitrade1 <- read_excel(trade1)
bitrade2 <- read_excel(trade2)
bitrade3 <- read_excel(trade3)

#join th three datasets
all_bitrade<-rbind(bitrade1[,1:6], bitrade2, bitrade3)

#### #### convert dataset to current iso #### #### 
trade_iso3 <- convert_historical_to_iso3(
  all_bitrade,
  origin_col = "iso_o",
  dest_col = "iso_d",
  flow_col = "FLOW",
  year_col = "year"
)

#### #### inteprolate the NA #### ####

# Get unique origin and destination countrie
u_o<-unique(trade_iso3$iso3_origin)
u_d<-unique(trade_iso3$iso3_destination)
#check if all countries are stated as destination and origin 
u_o %in% u_d
u_d %in% u_o

all_comb<-expand_grid(u_o, u_d)# Create all combinations
all_comb<-as.data.frame(all_comb)

#tApply the NA inrerpolation to all the combinations
                                                                                                #takes a loot of time 1h 39m 53s
bitrademod <- pbapply::pblapply(1:nrow(all_comb), function(i) {
  process_pair(trade_iso3, as.character(all_comb[i, ]))
})

bitrademod <- dplyr::bind_rows(bitrademod)

write.csv(bitrademod, output)

