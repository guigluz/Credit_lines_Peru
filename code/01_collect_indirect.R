# ==============================================================================
# 01_collect_indirect.R -- Download SBS cuadro B-2322
#   "Estructura de los Creditos Indirectos"
# ==============================================================================
# This is the target table for the credit-lines work: it carries the column
# "Lineas de credito no utilizadas y creditos concedidos no desembolsados".
#
# SBS lists B-2322 from Diciembre 1998, but the pre-2001 files are a DIFFERENT
# table ("Creditos contingentes y cobranzas", levels, multi-row headers) and the
# oldest use a 2-digit year in the filename. Collection starts at Enero 2001.
#
# Usage:  Rscript code/01_collect_indirect.R
# ==============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
source("code/00_sbs_utils.R")

LOG <- "output/logs/01_collect_indirect.log"
dir.create(dirname(LOG), recursive = TRUE, showWarnings = FALSE)
con <- file(LOG, open = "wt"); sink(con, split = TRUE); on.exit({sink(); close(con)}, add = TRUE)
cat("=== 01_collect_indirect.R ===\n", format(Sys.time()), "\n\n")

sbs_collect("B-2322", start_ym = c(2001L, 1L), end_ym = c(2026L, 6L))
cat("Done.\n")
