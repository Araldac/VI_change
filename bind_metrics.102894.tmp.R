
library(dplyr)
list_files <- strsplit('results/species/XGboost/LH_applyXGboost_metrics_Equal.csv results/species/XGboost/LH_applyXGboost_metrics_Weighted.csv results/species/XGboost/TM_applyXGboost_metrics_Equal.csv results/species/XGboost/TM_applyXGboost_metrics_Weighted.csv results/species/XGboost/PL_applyXGboost_metrics_Equal.csv results/species/XGboost/PL_applyXGboost_metrics_Weighted.csv results/species/XGboost/MP_applyXGboost_metrics_Equal.csv results/species/XGboost/MP_applyXGboost_metrics_Weighted.csv results/species/XGboost/PM_applyXGboost_metrics_Equal.csv results/species/XGboost/PM_applyXGboost_metrics_Weighted.csv results/species/XGboost/MF_applyXGboost_metrics_Equal.csv results/species/XGboost/MF_applyXGboost_metrics_Weighted.csv results/species/XGboost/HP_applyXGboost_metrics_Equal.csv results/species/XGboost/HP_applyXGboost_metrics_Weighted.csv results/species/XGboost/HO_applyXGboost_metrics_Equal.csv results/species/XGboost/HO_applyXGboost_metrics_Weighted.csv results/species/XGboost/WA_applyXGboost_metrics_Equal.csv results/species/XGboost/WA_applyXGboost_metrics_Weighted.csv results/species/XGboost/AG_applyXGboost_metrics_Equal.csv results/species/XGboost/AG_applyXGboost_metrics_Weighted.csv results/species/XGboost/TB_applyXGboost_metrics_Equal.csv results/species/XGboost/TB_applyXGboost_metrics_Weighted.csv results/species/XGboost/TA_applyXGboost_metrics_Equal.csv results/species/XGboost/TA_applyXGboost_metrics_Weighted.csv results/species/XGboost/HE_applyXGboost_metrics_Equal.csv results/species/XGboost/HE_applyXGboost_metrics_Weighted.csv', " +")
list_files<-unlist(list_files)

print(list_files)

all_oc <- lapply(list_files, function(f) {
df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
return(df)
}) %>% dplyr::bind_rows()  

head(all_oc)
write.csv(all_oc, 'results/species/XGboost/bind_metrics.csv')

