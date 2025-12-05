
#Snakefile at repo root
include: "workflow/rules/DatasetPrep.smk"   # adjust filename to yours
include: "workflow/rules/VariablesPrep.smk"   # adjust filename to yours
include: "workflow/rules/AssignVariables.smk"   # adjust filename to yours
include: "workflow/rules/BRT.smk"   # adjust filename to yours