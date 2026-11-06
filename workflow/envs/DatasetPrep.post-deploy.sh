#!/usr/bin/env bash
set -euo pipefail
Rscript -e 'install.packages("GeoThinneR", repos = "https://cloud.r-project.org", dependencies = FALSE)'