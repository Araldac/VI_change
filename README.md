
# Variable Change over Time

Explore wether variable importance of drivers of establihsment success in ant species varies with time.


## Prerequisites
[Snakemake installation option A](https://snakemake.readthedocs.io/en/v7.8.0/tutorial/setup.html)
or
[Snakemake installation option B](https://github.com/ThibaultLatrille/workshop-snakemake-unil2025)
 
## Roadmap

- A. Obtaining first records csv for each species on script Extra/FirstRecbySp.R
- B. "Dry run" to test wether the dependencies of the data, rules and scripts work well run 
  
```bash
  snakemake -s  --cores 7   --use-conda  --dry-run 
```
- C. Run Snakemake workflow
  
 ```bash
snakemake  --cores 10 --use-conda --rerun-triggers mtime

snakemake  --cores 10 --use-conda results/species/XGboost/bind_metrics.csv --rerun-triggers mtime

```
The "--rerun-triggers mtime " prevents snakemake 
from rerunning rules due to changes in the software environment (e.g., conda or container definitions)

- D. Run analysis and figure on script Extra/Analysis.R


