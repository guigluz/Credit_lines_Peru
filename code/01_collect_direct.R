# ==============================================================================
# 01_collect_direct.R -- Download SBS cuadro B-2343
#   "Estructura de los Creditos Directos por Modalidad"
# ==============================================================================
# Thin wrapper over sbs_collect() in code/00_sbs_utils.R, which enumerates the
# deterministic SBS URL pattern, downloads with caching, normalizes each file's
# extension to its true container format, and writes an auditable manifest.
#
# Coverage: 306 files, Enero 2001 - Junio 2026.
# Writes only into data/raw/sbs_boletin/ -- pre-existing files under data/raw/
# are never touched (see claude.md).
#
# Usage:  Rscript code/01_collect_direct.R
# ==============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
source("code/00_sbs_utils.R")

LOG <- "output/logs/01_collect_direct.log"
dir.create(dirname(LOG), recursive = TRUE, showWarnings = FALSE)
con <- file(LOG, open = "wt"); sink(con, split = TRUE); on.exit({sink(); close(con)}, add = TRUE)
cat("=== 01_collect_direct.R ===\n", format(Sys.time()), "\n\n")

sbs_collect("B-2343", start_ym = c(2001L, 1L), end_ym = c(2026L, 6L))
cat("Done.\n")
