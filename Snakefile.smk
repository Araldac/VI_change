
#list_files = ["results/species/RF/RF_TM_relimpAUC1.csv", "results/species/CSI/Raw_RF_TM_relimpAUC1.csv"]
#list_files = expand("results/species/RF/RF_{species}_relimpAUC1.csv", species=sp)
sp = ["LH",  "TM",  "PL",  "MP",  "PM",  "MF",  "HP" ,  "HO", "WA", "AG", "TB", "TA", "HE"]
#sp = ["MP", "TA", "HE"]

var_type = ["CSI", "Raw"]

list_AV =  expand("results/species/AssignVariables/{species}_allv.csv", species=sp)
#subset_sp = ["LH",  "TM",  "PL"]
#list_RF =  expand("results/species/RF/RF_{species}_params.csv", species=sp)


list_RF2 =  expand("results/species/RF/RF_{species}_relimpAUC1.csv", species=sp)

list_RF3 =  expand("results/species/CSI/Raw_RF_{species}_relimpAUC1.csv", species=sp)


rule all:
    input: list_RF2

#Snakefile at repo root
include: "workflow/rules/DatasetPrep.smk"   # adjust filename to yours
include: "workflow/rules/VariablesPrep.smk"   # adjust filename to yours
include: "workflow/rules/AssignVariables.smk"   # adjust filename to yours
#include: "workflow/rules/BRT.smk"   # adjust filename to yours
include: "workflow/rules/RF.smk"   # adjust filename to yours
include: "workflow/rules/CSI.smk"   # adjust filename to yours