
# Variable Change over Time

Explore wether variable importance of drivers of establihsment success in ant species varies with time.


## Prerequisites
[Snakemake installation option A](https://snakemake.readthedocs.io/en/v7.8.0/tutorial/setup.html)
or
[Snakemake installation option B](https://github.com/ThibaultLatrille/workshop-snakemake-unil2025)
 
## Roadmap

- 1. Obtaining first records csv for each species on script worflow/scripts/VariablesPrerp/Trade_first_records.R
- 2. "Dry run" to test wether the dependencies of the data, rules and scripts work well run 
  
```bash
  snakemake   --cores 7   --use-conda  --dry-run 
```
- 3. Run Snakemake workflow
Contains four parts: 
  A_ DatasetPrep: using the occurrences loaded from GBIF for, select alien occurreces, clean, thin and verify that they make sense based on first record data
  B_VariablesPrep: prepare the four variables Climate, Mean Phylogenetic distance, Trade and Human modification
  C_AssignVariables: draw background points. To each occurrence and background point assign the valu for each variable
  D_XGBoost: run algorithm and extract the SHAP values, one model per period and 100 replicates

For running the four parts at once
 ```bash
snakemake  --cores 10 --use-conda --rerun-triggers mtime

snakemake  --cores 10 --use-conda results/species/XGboost/bind_metrics.csv --rerun-triggers mtime

```
The "--rerun-triggers mtime " prevents snakemake 
from rerunning rules due to changes in the software environment (e.g., conda or container definitions)

For running each part inidivually:

Create a run all function 
 ```bash
snakemake  --cores 10 --use-conda --rerun-triggers mtime

snakemake  --cores 10 --use-conda results/species/XGboost/bind_metrics.csv --rerun-triggers mtime

```
- 4. Run analysis and figure on Rscript Extra/Analysis.R and ShapBeeswarmPlot on Extra/ShapPlot.ipynb


