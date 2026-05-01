
# Variable Change over Time

explore wether variable importance of drivers of establihsment success in ant species varies with time.


## Prerequisites
[Snakemake installation](https://snakemake.readthedocs.io/en/v7.8.0/tutorial/setup.html)

 
## Roadmap

- Additional browser support

- Add more integrations


## Running Tests

To do a "dry run", test wether the dependencies of the data, rules and scripts work well run 

```bash
  snakemake -s Snakefile.smk  --cores 7   --use-conda  --dry-run 
```

## Usage/Examples
Run all the snakemake workflow to obtain the same dataset. 

```bash
snakemake -s Snakefile.smk  --cores 10 --use-conda --rerun-triggers mtime  
```
The "--rerun-triggers mtime " prevents snakemake 
from rerunning rules due to changes in the software environment (e.g., conda or container definitions)