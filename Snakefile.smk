
#Snakefile at repo root
include: "workflow/rules/DatasetPrep.smk"   # adjust filename to yours
include: "workflow/rules/VariablesPrep.smk"   # adjust filename to yours
include: "workflow/rules/AssignVariables.smk"   # adjust filename to yours
include: "workflow/rules/BRT.smk"   # adjust filename to yours
include: "workflow/rules/RF.smk"   # adjust filename to yours
include: "workflow/rules/CSI.smk"   # adjust filename to yours

sp = ["LH", "TS", "TM", "WA"]
var_type = ["CSI", "Raw"]