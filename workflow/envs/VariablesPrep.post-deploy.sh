#!/usr/bin/env bash
set -euo pipefail
Rscript -e 'options(download.file.method = "curl"); remotes::install_github("Nowosad/popgrids@1588c0b", upgrade = "never"); library(popgrids)'