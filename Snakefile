
sp = ["LH",  "TM",  "PL",  "MP",  "PM",  "MF",  "HP" ,  "HO", "WA", "AG", "TB", "TA", "HE"]

rule all:
    input: 'results/species/XGboost/bind_SHAP.csv'


#Snakefile at repo root
include: "workflow/rules/A_DatasetPrep.smk"   
include: "workflow/rules/B_VariablesPrep.smk" 
include: "workflow/rules/C_AssignVariables.smk"   
include: "workflow/rules/D_XGboost.smk"  