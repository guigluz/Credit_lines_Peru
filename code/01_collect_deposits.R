# ==============================================================================
# 01_collect_deposits.R -- Download SBS cuadro B-2358
#   "Depositos y Creditos por Oficina y Empresa Bancaria"
# ==============================================================================
# Listed on the SBS site as "Creditos Directos y Depositos por Oficinas", under
# the "Alcance y Participacion de Mercado" section of Informacion Estadistica de
# Banca Multiple. The in-file title differs from the listed name; both are
# recorded so the table can be traced back to the publication.
#
# This is the only SBS bulletin cuadro at the BRANCH (oficina) grain: per bank
# per oficina it gives deposits by type (vista / ahorro / plazo, each split
# MN/ME) and direct credits (MN/ME), in miles de S/.
#
# Coverage (probed against the file host, 2026-08-28):
#   * files exist from 2001-03 (patchy) and continuously from 2001-12;
#     latest available month is 2026-06 (2026-07 is 404).
#   * the "Codigo Oficina" column -- the only branch identifier in the table --
#     DOES NOT EXIST in 2001-03 or in ANY of the 12 months of 2004 (17-column
#     files instead of 18). Collection therefore starts at Enero 2010, which
#     also matches the regime in which B-2322 reports unused credit lines
#     (see code/02_clean_indirect.R).
#   * container format is NOT a single clean break: over 2010-01..2026-06 the
#     manifest records five transitions (xls->xlsx at 2013-09, back to xls at
#     2015-06, xlsx at 2015-09, xls at 2015-10, xlsx from 2017-02 on). Every
#     file is still named ".XLS" regardless. Nothing downstream depends on the
#     extension: sbs_normalize_ext() stores each file under its true signature.
#
# Usage:  Rscript code/01_collect_deposits.R
# ==============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
source("code/00_sbs_utils.R")

LOG <- "output/logs/01_collect_deposits.log"
dir.create(dirname(LOG), recursive = TRUE, showWarnings = FALSE)
con <- file(LOG, open = "wt"); sink(con, split = TRUE); on.exit({sink(); close(con)}, add = TRUE)
cat("=== 01_collect_deposits.R ===\n", format(Sys.time()), "\n\n")

sbs_collect("B-2358", start_ym = c(2010L, 1L), end_ym = c(2026L, 6L))
cat("Done.\n")
