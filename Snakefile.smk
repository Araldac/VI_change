
sp = ["LH",  "TM",  "PL",  "MP",  "PM",  "MF",  "HP" ,  "HO", "WA", "AG", "TB", "TA", "HE"]

rule all:
    input: 'results/species/XGboost/bind_SHAP.csv'

#Snakefile at repo root
include: "workflow/rules/DatasetPrep.smk"   
include: "workflow/rules/VariablesPrep.smk" 
include: "workflow/rules/AssignVariables.smk"   
include: "workflow/rules/XGboost.smk"  