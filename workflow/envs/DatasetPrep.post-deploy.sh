#!/usr/bin/env bash
set -euo pipefail
Rscript -e 'options(download.file.method = "curl"); install.packages("GeoThinneR", repos = "https://cloud.r-project.org", dependencies = FALSE); library(GeoThinneR)'