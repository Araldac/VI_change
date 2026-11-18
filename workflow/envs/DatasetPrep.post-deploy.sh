#!/usr/bin/env bash
set -euo pipefail
Rscript -e 'options(download.file.method = "curl"); remotes::install_version("GeoThinneR", version = "2.1.3", repos = "https://cloud.r-project.org", dependencies = FALSE, upgrade = "never"); library(GeoThinneR)'