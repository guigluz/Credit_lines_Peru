# ==============================================================================
# 11_loan_reserve_shock_regression.R -- Khwaja-Mian-style reduced-form loan-growth
# regressions on the bank reserve-requirement shocks built in code/10_reserve_shocks.R
# ==============================================================================
# Run from the project root:   Rscript code/11_loan_reserve_shock_regression.R
# Requires:                    Rscript code/10_reserve_shocks.R (run first)
#
# DESIGN
#   Loan growth is log growth of a firm's credit balance at a bank between two
#   consecutive month-ends, separately for credit lines and other credit. The
#   regression compares banks lending to the SAME firm in the SAME month
#   (firm-by-date fixed effects, so firm demand is absorbed), regressing growth on
#   the bank's reserve-requirement shock; standard errors are clustered by bank.
#
#   The shocks are keyed by bank_standardized (the project's hub key); the credit
#   file identifies banks by the RCC entity code (`entidad`). This script builds
#   the entidad -> bank_standardized crosswalk and merges credit rows to shocks
#   through it: credit row -> entidad -> bank_standardized -> shock.
#
# INPUTS (never modified)
#   data/raw/muestra_anonima_base2015.parquet        credit, firm x bank x product x currency x month
#   data/processed/bank_encaje_shocks.csv            bank shocks, unique by bank_standardized x fecha_reporte (10_reserve_shocks.R)
#   data/processed/bank_deposit_shares.csv           deposit shares, every institution x month (10_reserve_shocks.R)
#   data/documentation/RCC_empresas_supervisadas_2026.xls   RCC entity code -> name (tab-delimited text
#                                                    saved with an .xls extension, ISO-8859-1)
#   data/documentation/bank_map_manual.csv           curated name -> bank_standardized hub map
#
# OUTPUTS
#   data/processed/crosswalks/xwalk_rcc.csv          entidad code -> bank_standardized (project spoke format + code)
#   data/processed/credit_balances_by_currency.csv.gz  currency-specific intermediate (entidad x firm x credit_line x moneda x date)
#   data/processed/credit_line_growth_panel.csv.gz   relationship-month panel with growth, exclusion reason and shocks
#   output/tables/tab_encaje_loan_growth.tex         regression table fragment
#   output/validation/encaje_shock_*.csv             diagnostics (coverage, classification, exclusions, regressions)
#   output/logs/11_loan_reserve_shock_regression.log this script's console log
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
})
source("code/00_sbs_utils.R")   # canon_bank(), standardize_banks(), read_bank_map(); loads dplyr/stringr, hardens the locale

# ---- parameters ---------------------------------------------------------------
CREDIT_PQ  <- "data/raw/muestra_anonima_base2015.parquet"
SHOCKS_CSV <- "data/processed/bank_encaje_shocks.csv"
SHARES_CSV <- "data/processed/bank_deposit_shares.csv"
RCC_LIST   <- "data/documentation/RCC_empresas_supervisadas_2026.xls"
PROC  <- "data/processed"
VALID <- "output/validation"
TABS  <- "output/tables"
LOGF  <- "output/logs/11_loan_reserve_shock_regression.log"

# Shock measures regressed (columns of bank_encaje_shocks.csv: bank_shock_<measure>); the table
# layout (two panels of columns) assumes exactly these two.
MEASURES <- c("gen_min_legal", "gen_marginal")

CREDIT_LINE_PRODUCTS <- c("tarjeta_credito", "cuenta_corriente", "cartas_credito",
                          "avales_fianzas", "multilinea", "otras_lineas")

N_BOOT <- 9999L          # wild cluster bootstrap draws
SEED   <- 20261009L
FEW_CLUSTERS <- 30L      # flag conventional cluster inference below this many bank clusters

for (d in c(PROC, VALID, TABS, dirname(LOGF), file.path(PROC, "crosswalks")))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
log_con <- file(LOGF, open = "wt"); sink(log_con, split = TRUE)

hdr <- function(s) cat("\n", strrep("=", 78), "\n", s, "\n", strrep("=", 78), "\n", sep = "")
fmt <- function(x) format(x, big.mark = ",", trim = TRUE)
write_val <- function(x, name) fwrite(x, file.path(VALID, paste0("encaje_shock_", name, ".csv")))

# ==============================================================================
# 1. Credit data, bank shocks and the entidad -> bank_standardized crosswalk
# ==============================================================================
hdr("1. Credit data, shocks and RCC crosswalk")
cr <- as.data.table(read_parquet(CREDIT_PQ, col_select = c("fecha_reporte", "identificador", "entidad",
                                                           "moneda_codigo", "producto", "credito_vigente")))
cat(sprintf("Credit file: %s rows, %s firms, %d entidad codes, %d dates.\n", fmt(nrow(cr)),
            fmt(uniqueN(cr$identificador)), uniqueN(cr$entidad), uniqueN(cr$fecha_reporte)))
stopifnot(inherits(cr$fecha_reporte, "Date"), is.numeric(cr$credito_vigente))
cr_dates <- sort(unique(cr$fecha_reporte))
stopifnot(all(as.integer(format(cr_dates + 1, "%d")) == 1L), all(diff(cr_dates) %in% 28:31))   # consecutive month-ends
cat(sprintf("Credit calendar: %d consecutive month-ends, %s to %s.\n", length(cr_dates),
            format(min(cr_dates)), format(max(cr_dates))))

# ---- bank shocks from 10_reserve_shocks.R (keyed by bank_standardized) ---------
shocks <- fread(SHOCKS_CSV)
shocks[, `:=`(fecha_reporte = as.Date(fecha_reporte), baseline_date = as.Date(baseline_date))]
stopifnot(all(paste0("bank_shock_", MEASURES) %in% names(shocks)),
          !anyDuplicated(shocks[, .(bank_standardized, fecha_reporte)]),            # unique on the merge key
          all(cr_dates %in% shocks$fecha_reporte),                                  # shocks cover the credit calendar
          uniqueN(shocks$baseline_date) == 1L)
BASELINE_DATE <- shocks$baseline_date[1L]
cat(sprintf("Shocks: %s bank-months, %d banks, %s to %s; deposit shares fixed at %s.\n",
            fmt(nrow(shocks)), uniqueN(shocks$bank_standardized), format(min(shocks$fecha_reporte)),
            format(max(shocks$fecha_reporte)), format(BASELINE_DATE)))

# Rate changes inside the credit calendar (common to all banks: one row per month)
win <- unique(shocks[fecha_reporte %in% cr_dates, c("fecha_reporte", grep("^d_", names(shocks), value = TRUE)), with = FALSE])
stopifnot(nrow(win) == length(cr_dates))
for (j in MEASURES) for (cur in c("mn", "me")) {
  v <- win[[paste0("d_", cur, "_", j)]]
  cat(sprintf("  d_%s_%-14s months with data %3d | zero %3d | tighter %2d | looser %2d | NA %3d\n",
              cur, j, sum(!is.na(v)), sum(v == 0, na.rm = TRUE), sum(v > 0, na.rm = TRUE),
              sum(v < 0, na.rm = TRUE), sum(is.na(v))))
}

# ---- baseline coverage by institution, rebuilt from the all-months share panel ----
shares <- fread(SHARES_CSV, encoding = "UTF-8")
shares[, date := as.Date(date)]
sh <- merge(shares[, .(bank_standardized = unique(bank_standardized))],
            shares[date == BASELINE_DATE, .(bank_standardized, s_mn_0 = s_mn, s_me_0 = s_me, row_reason = reason)],
            by = "bank_standardized", all.x = TRUE)
sh[, reason := fifelse(is.na(row_reason), "no Reporte row at baseline date", row_reason)]
sh[, valid := reason == "valid"]
chk <- merge(unique(shocks[, .(bank_standardized, s_mn_0)]), sh[valid == TRUE, .(bank_standardized, s_mn_chk = s_mn_0)],
             by = "bank_standardized", all = TRUE)
stopifnot(!anyNA(chk), max(abs(chk$s_mn_0 - chk$s_mn_chk)) < 1e-9)   # the shock table used exactly these baseline shares
cat(sprintf("Baseline %s: %d Reporte institutions, valid shares for %d (all of them in the shock table); excluded: %s.\n",
            format(BASELINE_DATE), nrow(sh), sum(sh$valid),
            paste(sprintf("%d %s", sh[valid == FALSE, .N, by = reason]$N, sh[valid == FALSE, .N, by = reason]$reason), collapse = "; ")))

# ---- RCC entity code -> bank_standardized (the project hub) --------------------
rcc <- fread(RCC_LIST, sep = "\t", encoding = "Latin-1", colClasses = "character")
stopifnot(identical(names(rcc), c("Codigo", "Tipo", "Estado", "Nombre")))
setnames(rcc, c("codigo", "tipo", "estado", "bank_raw"))
rcc[, c("tipo", "estado", "bank_raw") := lapply(.SD, enc2utf8), .SDcols = c("tipo", "estado", "bank_raw")]
rcc[, entidad := as.integer(codigo)]
stopifnot(!anyNA(rcc$entidad), !anyDuplicated(rcc$entidad), !anyDuplicated(rcc$bank_raw))

manual <- read_bank_map()
rcc_std <- as.data.table(standardize_banks(rcc$bank_raw, "rcc", manual))
stopifnot(identical(rcc_std$bank_raw, rcc$bank_raw))
rcc[, `:=`(bank_standardized = rcc_std$bank_standardized, review_required = rcc_std$review_required)]
credit_entidades <- unique(cr$entidad)
rcc[, in_credit_sample := entidad %in% credit_entidades]

# Why a code has no hub id (documentation only; never used to guess a mapping)
rcc[, unmapped_note := fcase(
  !is.na(bank_standardized),                                   "",
  estado == "Venta de Cartera",                                 "portfolio-sale / guarantee vehicle, not an operating lender",
  tipo %in% c("Caja Municipal de Ahorro y Crédito", "Caja Rural de Ahorro y Crédito",
              "Cooperativa de Ahorro y Crédito (supervisión directa SBS)"),
                                                                "type outside Reporte coverage (Banca Multiple + Empresas Financieras only)",
  default =                                                     "not in Reporte or any bulletin crosswalk")]
fwrite(rcc[order(entidad), .(entidad, bank_raw, tipo, estado, bank_standardized, review_required,
                             unmapped_note, in_credit_sample)],
       file.path(PROC, "crosswalks", "xwalk_rcc.csv"))
cat(sprintf("RCC list: %d codes (%s). Mapped to a hub bank_standardized: %d; unmapped: %d.\n",
            nrow(rcc), paste(names(table(rcc$estado)), table(rcc$estado), sep = "=", collapse = ", "),
            sum(!is.na(rcc$bank_standardized)), sum(is.na(rcc$bank_standardized))))
cat(sprintf("Credit sample: %d distinct entidad codes; %d are in the RCC list, %d are not (historic/removed entities): %s\n",
            length(credit_entidades), sum(credit_entidades %in% rcc$entidad), sum(!credit_entidades %in% rcc$entidad),
            paste(sort(setdiff(credit_entidades, rcc$entidad)), collapse = ", ")))
# a code maps to one bank; several codes MAY share a bank (none do here)
dupb <- rcc[!is.na(bank_standardized), .N, by = bank_standardized][N > 1]
cat(sprintf("Hub banks reached by more than one RCC code: %s\n",
            if (nrow(dupb)) paste(dupb$bank_standardized, collapse = ", ") else "none"))

# ==============================================================================
# 2. Product classification, merge with shocks
# ==============================================================================
hdr("2. Product classification and merge")
# Normalize whitespace and case only; product meanings unchanged.
cr[, producto := { p <- tolower(trimws(gsub("\\s+", " ", as.character(producto)))); p[p == ""] <- NA_character_; p }]
unknown <- setdiff(CREDIT_LINE_PRODUCTS, unique(cr$producto))
if (length(unknown)) stop("credit-line products absent from the data: ", paste(unknown, collapse = ", "))
cr[, credit_line := fifelse(is.na(producto), NA, producto %in% CREDIT_LINE_PRODUCTS)]

pc <- cr[, .(n_rows = .N, n_firms = uniqueN(identificador), sum_credito_vigente = sum(credito_vigente, na.rm = TRUE),
             n_positive_rows = sum(credito_vigente > 0, na.rm = TRUE),
             share_positive_rows = mean(credito_vigente > 0, na.rm = TRUE)), by = .(producto, credit_line)][order(-credit_line, -n_rows)]
write_val(pc, "product_classification")
cat("Product classification (credit_line = TRUE for the six listed products):\n")
print(pc[, .(producto, credit_line, n_rows, n_firms, n_positive_rows, share_positive_rows = round(share_positive_rows, 3))], nrows = 50)
cat(sprintf("NOTE: credito_vigente is identically zero for %s; credit-line growth therefore rests on %s.\n",
            paste(pc[credit_line == TRUE & n_positive_rows == 0, producto], collapse = ", "),
            paste(pc[credit_line == TRUE & n_positive_rows > 0, producto], collapse = " and ")))
cat(sprintf("Rows: credit_line TRUE %s | FALSE %s | missing producto %s (%s excluded from both regression samples).\n",
            fmt(sum(cr$credit_line, na.rm = TRUE)), fmt(sum(!cr$credit_line, na.rm = TRUE)),
            fmt(sum(is.na(cr$credit_line))), fmt(sum(is.na(cr$credit_line)))))

cat(sprintf("credito_vigente: missing %s | negative %s | zero %s (%.1f%%) | positive %s\n",
            fmt(sum(is.na(cr$credito_vigente))), fmt(sum(cr$credito_vigente < 0, na.rm = TRUE)),
            fmt(sum(cr$credito_vigente == 0, na.rm = TRUE)), 100 * mean(cr$credito_vigente == 0, na.rm = TRUE),
            fmt(sum(cr$credito_vigente > 0, na.rm = TRUE))))

# Entity statistics (computed before the merge, on the raw credit rows)
ent_stats <- cr[, .(n_firms = uniqueN(identificador), n_obs = .N,
                    first_date = min(fecha_reporte), last_date = max(fecha_reporte),
                    sum_credito_vigente = sum(credito_vigente, na.rm = TRUE)), by = entidad]

# ---- merge shocks: credit row -> entidad -> bank_standardized -> shock -------------
# Both steps are many-to-one: the crosswalk is unique by entidad and the shock table is unique by
# (bank_standardized, fecha_reporte) (both asserted), so no credit row can be multiplied or lost.
n0 <- nrow(cr)
cr <- merge(cr, rcc[, .(entidad, bank_standardized)], by = "entidad", all.x = TRUE, sort = FALSE)
stopifnot(nrow(cr) == n0)
cr <- merge(cr, shocks, by = c("bank_standardized", "fecha_reporte"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(cr) == n0)
cr[, matched := !is.na(s_mn_0)]               # a shock row exists: mapped to a bank AND valid baseline shares
cat(sprintf("Merge via entidad -> bank_standardized on (bank_standardized, fecha_reporte): %s of %s credit rows matched (%.1f%%), %s unmatched.\n",
            fmt(sum(cr$matched)), fmt(n0), 100 * mean(cr$matched), fmt(sum(!cr$matched))))
bk <- cr[, .(n_obs = .N, matched = any(matched)), by = entidad]
cat(sprintf("  Banks (entidad): %d matched to a shock row, %d unmatched.\n", sum(bk$matched), sum(!bk$matched)))

# Entity coverage and the reason each unmatched code is excluded
cov <- merge(ent_stats, rcc[, .(entidad, bank_raw, tipo, estado, bank_standardized, unmapped_note)], by = "entidad", all.x = TRUE)
cov <- merge(cov, sh[, .(bank_standardized, baseline_reason = reason, s_mn_0, s_me_0)], by = "bank_standardized", all.x = TRUE)
cov[, status := fcase(
  !entidad %in% rcc$entidad,           "excluded: code not in RCC list",
  is.na(bank_standardized),            paste0("excluded: no hub id, ", unmapped_note),
  baseline_reason != "valid" | is.na(baseline_reason),
                                       paste0("excluded: no valid baseline (", fifelse(is.na(baseline_reason), "not in Reporte", baseline_reason), ")"),
  default =                            "included: shock available")]
setorder(cov, -sum_credito_vigente)
write_val(cov[, .(entidad, bank_raw, tipo, estado, bank_standardized, n_firms, n_obs, first_date, last_date,
                  sum_credito_vigente, s_mn_0, s_me_0, status)], "entity_coverage")
stopifnot(setequal(cov[status == "included: shock available", entidad], bk[matched == TRUE, entidad]))   # coverage agrees with the merge
cat("\nEntity coverage (credit sample), by status:\n")
print(cov[, .(entidades = .N, firms_obs_rows = sum(n_obs), share_of_balance = round(sum(sum_credito_vigente) / sum(cov$sum_credito_vigente), 4)), by = status])
cat("\nBanks with a shock (included):\n")
print(cov[status == "included: shock available", .(entidad, bank_standardized, n_firms, n_obs, s_mn_0 = round(s_mn_0, 3), s_me_0 = round(s_me_0, 3))])
cat("\nLargest excluded entities by balance:\n")
print(head(cov[status != "included: shock available", .(entidad, bank_raw, status, n_firms, share = round(sum_credito_vigente / sum(cov$sum_credito_vigente), 4))], 12))
hb <- setdiff(sh[valid == TRUE, bank_standardized], rcc[, bank_standardized])
cat(sprintf("\nReporte institutions with a valid baseline but no RCC code (no credit rows to attach to): %s\n",
            paste(hb, collapse = ", ")))

# Sensitivity of the baseline choice: MN share at the baseline vs at the first credit month (diagnostic only)
alt <- shares[date == min(cr_dates) & valid == TRUE, .(bank_standardized, s_mn_first_credit_month = s_mn)]
cmp <- merge(sh[valid == TRUE, .(bank_standardized, s_mn_0)], alt, by = "bank_standardized")
cmp <- merge(cmp, rcc[!is.na(bank_standardized), .(entidad = entidad[1L]), by = bank_standardized], by = "bank_standardized")
cat(sprintf("\nBaseline robustness, %d banks with an RCC code: MN share at %s vs %s: corr %.3f, max |diff| %.3f\n",
            nrow(cmp), format(BASELINE_DATE), format(min(cr_dates)), cor(cmp$s_mn_0, cmp$s_mn_first_credit_month),
            max(abs(cmp$s_mn_0 - cmp$s_mn_first_credit_month))))
print(cmp[order(entidad), .(entidad, bank_standardized, s_mn_baseline = round(s_mn_0, 3),
                            s_mn_first_credit_month = round(s_mn_first_credit_month, 3))])

# ==============================================================================
# 3. Currency: valuation basis, then aggregation
# ==============================================================================
hdr("3. Currency valuation and aggregation")
# Test whether foreign-currency (ME) balances are reported in original currency or already
# valued in soles at the month-end exchange rate. In soles, a constant-dollar loan changes by
# the exchange-rate return each month, so many ME relationships share ONE identical non-zero
# log change and almost none are exactly unchanged; soles loans show the opposite (a mass
# at exactly zero, no common non-zero value). Sol-denominated and ME relationships are
# followed at the product level over consecutive months with positive balances.
rel <- cr[credito_vigente > 0 & !is.na(producto), .(bal = sum(credito_vigente)),
          by = .(identificador, entidad, producto, moneda_codigo, fecha_reporte)]
rel[, idx := match(fecha_reporte, cr_dates)]
setorder(rel, identificador, entidad, producto, moneda_codigo, idx)
rel[, `:=`(lbal = shift(bal), lidx = shift(idx)), by = .(identificador, entidad, producto, moneda_codigo)]
rg <- rel[!is.na(lbal) & idx - lidx == 1L]
rg[, lg := round(log(bal) - log(lbal), 6)]
vt <- rg[, {
  nz <- lg[lg != 0]; tt <- if (length(nz)) max(table(nz)) else 0L
  .(n = .N, share_zero = mean(lg == 0), share_modal_nonzero = tt / .N)
}, by = .(moneda_codigo, fecha_reporte)]
vs <- vt[, .(months = .N, pairs = sum(n), share_exact_zero = weighted.mean(share_zero, n),
             median_share_modal_nonzero = median(share_modal_nonzero)), by = moneda_codigo][order(moneda_codigo)]
vs[, currency := fcase(moneda_codigo == "1", "MN (soles)", moneda_codigo == "2", "ME (foreign)", default = "VAC-indexed")]
print(vs[, .(moneda_codigo, currency, months, pairs, share_exact_zero = round(share_exact_zero, 3),
             median_share_modal_nonzero = round(median_share_modal_nonzero, 3))])
write_val(vs, "currency_valuation_test")
ME_IN_SOLES <- with(vs[moneda_codigo == "2"], share_exact_zero < 0.02 && median_share_modal_nonzero > 0.10) &&
               with(vs[moneda_codigo == "1"], share_exact_zero > 0.05 && median_share_modal_nonzero < 0.05)
cat(sprintf("\nValuation-basis verdict: ME balances %s.\n",
            if (ME_IN_SOLES) paste0("are valued in soles at the month-end SBS exchange rate (a common-valuation basis, as the Manual de ",
                                    "Contabilidad Cap. I C.6 prescribes): MN, ME and VAC balances can be summed")
            else "are NOT shown to be in soles"))

# Currency-specific intermediate dataset (kept regardless of the verdict)
cur_agg <- cr[!is.na(credit_line), .(balance = if (all(is.na(credito_vigente))) NA_real_ else sum(credito_vigente, na.rm = TRUE),
                                      n_rows = .N),
              by = .(entidad, identificador, credit_line, moneda_codigo, fecha_reporte)]
setorder(cur_agg, entidad, identificador, credit_line, moneda_codigo, fecha_reporte)
fwrite(cur_agg, file.path(PROC, "credit_balances_by_currency.csv.gz"))
cat(sprintf("Currency-specific intermediate: %s rows -> data/processed/credit_balances_by_currency.csv.gz\n", fmt(nrow(cur_agg))))
if (!ME_IN_SOLES) {
  cat("BLOCKER: no documented exchange rate in the project; cross-currency aggregation not performed.\n")
  sink(); close(log_con); stop("ME valuation basis not established; see log")
}

# ---- aggregate: entidad x firm x credit_line x month (products and currencies summed) ----
excl_na <- cr[is.na(credit_line)]
cat(sprintf("Missing producto: %s rows (%s firm-bank-months) excluded from both regression samples.\n",
            fmt(nrow(excl_na)), fmt(uniqueN(excl_na[, .(entidad, identificador, fecha_reporte)]))))
shock_cols <- paste0("bank_shock_", MEASURES)
agg <- cr[!is.na(credit_line), c(list(
  balance = if (all(is.na(credito_vigente))) NA_real_ else sum(credito_vigente, na.rm = TRUE),   # all-missing stays missing
  n_products = uniqueN(producto), n_raw_rows = .N,
  bal_me = sum(credito_vigente[moneda_codigo == "2"], na.rm = TRUE)),
  lapply(.SD, function(v) { stopifnot(uniqueN(v, na.rm = FALSE) == 1L); v[1L] })),
  by = .(entidad, identificador, credit_line, fecha_reporte), .SDcols = c("bank_standardized", shock_cols)]
agg[, me_balance_share := fifelse(!is.na(balance) & balance > 0, bal_me / balance, NA_real_)]
cat(sprintf("Aggregated to %s entidad x firm x credit_line x month rows (from %s product-currency rows).\n",
            fmt(nrow(agg)), fmt(sum(agg$n_raw_rows))))
cat(sprintf("  all-missing balance groups: %d (left missing, never set to zero); negative: %d; zero: %s\n",
            sum(is.na(agg$balance)), sum(agg$balance < 0, na.rm = TRUE), fmt(sum(agg$balance == 0, na.rm = TRUE))))

# ==============================================================================
# 4. Loan growth
# ==============================================================================
hdr("4. Loan growth")
agg[, idx := match(fecha_reporte, cr_dates)]
setorder(agg, entidad, identificador, credit_line, idx)
agg[, `:=`(bal_lag = shift(balance), idx_lag = shift(idx)), by = .(entidad, identificador, credit_line)]
agg[, gap_months := idx - idx_lag]
# One reason per relationship-month, in this order of precedence.
agg[, growth_status := fcase(
  is.na(idx_lag),                                  "first_observation",
  gap_months > 1L,                                 "reporting_gap",
  is.na(balance) | is.na(bal_lag),                 "missing_balance",
  balance < 0 | bal_lag < 0,                       "negative_balance",
  balance == 0 | bal_lag == 0,                     "zero_balance",
  default =                                        "valid")]
agg[growth_status == "valid", loan_growth := log(balance) - log(bal_lag)]
agg[, zero_detail := fcase(growth_status != "zero_balance", NA_character_,
                           balance == 0 & bal_lag == 0, "zero at t and t-1",
                           balance == 0,                "zero at t",
                           default =                    "zero at t-1")]

ex <- agg[, .(relationship_months = .N,
              first_observation = sum(growth_status == "first_observation"),
              reporting_gap = sum(growth_status == "reporting_gap"),
              missing_balance = sum(growth_status == "missing_balance"),
              negative_balance = sum(growth_status == "negative_balance"),
              zero_balance = sum(growth_status == "zero_balance"),
              valid_growth = sum(growth_status == "valid")), by = credit_line][order(-credit_line)]
ex <- rbind(ex, cbind(credit_line = NA, ex[, lapply(.SD, sum), .SDcols = -"credit_line"]))
ex[, credit_line := fifelse(is.na(credit_line), "all", fifelse(credit_line, "credit_line", "non_credit_line"))]
print(as.data.frame(ex), row.names = FALSE)
cat("\n  zero-balance exclusions by endpoint:\n"); print(agg[growth_status == "zero_balance", .N, by = .(credit_line, zero_detail)][order(credit_line, zero_detail)])
cat("\n  reporting-gap length (scheduled months between consecutive observations):\n")
print(agg[growth_status == "reporting_gap", .(n = .N), by = .(gap_months = pmin(gap_months, 12L))][order(gap_months)])
last_obs <- agg[, .(exit = idx == max(idx) & idx < length(cr_dates)), by = .(entidad, identificador, credit_line)]
cat(sprintf("\n  relationships whose last observation is before the sample end (exits, not in the growth sample): %s of %s\n",
            fmt(sum(last_obs$exit)), fmt(nrow(last_obs))))
cat(sprintf("  non-missing growth: min %.3f, median %.4f, max %.3f (no winsorizing or trimming)\n",
            min(agg$loan_growth, na.rm = TRUE), median(agg$loan_growth, na.rm = TRUE), max(agg$loan_growth, na.rm = TRUE)))
cat("  NOTE: the log-growth sample covers CONTINUING positive-balance relationships; it does not measure loan entry or exit.\n")
cat(sprintf("  valid growth observations with a foreign-currency component: %.1f%% (balance-weighted ME share %.1f%%); ME growth embeds the exchange-rate revaluation.\n",
            100 * mean(agg[growth_status == "valid", me_balance_share > 0], na.rm = TRUE),
            100 * agg[growth_status == "valid", sum(bal_me) / sum(balance)]))
write_val(ex, "growth_exclusions")
fwrite(agg[, .(entidad, bank_standardized, identificador, credit_line, fecha_reporte, balance, bal_lag, gap_months,
               growth_status, loan_growth, n_products, me_balance_share,
               bank_shock_gen_min_legal, bank_shock_gen_marginal)],
       file.path(PROC, "credit_line_growth_panel.csv.gz"))

# ==============================================================================
# 5. Regressions
# ==============================================================================
hdr("5. Regressions")
# Wild cluster bootstrap-t, null imposed (WCR; Cameron, Gelbach & Miller 2008, with Webb 2014
# six-point weights when there are fewer than 12 clusters, Rademacher otherwise). Implemented
# directly because fwildclusterboot/boottest are not installed. With one regressor and a single
# absorbed firm-by-date effect, Frisch-Waugh makes the bootstrap exact: demean y and x within
# firm-date cells, impose beta = 0 (restricted residuals = demeaned y), rebuild y* = w_g * e,
# re-demean, and recompute beta* and its cluster-robust t.
wcr_boot <- function(y, x, cell, cl, B, seed) {
  set.seed(seed)
  K <- max(cell); nk <- tabulate(cell, K); G <- max(cl)
  dm <- function(v) v - (as.numeric(rowsum(v, cell, reorder = TRUE)) / nk)[cell]
  xd <- dm(x); yd <- dm(y); sxx <- sum(xd^2)
  tstat <- function(yy_d) {
    b <- sum(xd * yy_d) / sxx
    sc <- as.numeric(rowsum(xd * (yy_d - b * xd), cl, reorder = TRUE))
    c(b, b / (sqrt(sum(sc^2)) / sxx))
  }
  obs <- tstat(yd)
  webb <- G < 12L
  wv <- c(-sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2))
  ts <- numeric(B)
  for (b in seq_len(B)) {
    w <- if (webb) sample(wv, G, replace = TRUE) else sample(c(-1, 1), G, replace = TRUE)
    ts[b] <- tstat(dm(w[cl] * yd))[2]
  }
  list(t_cr0 = obs[2], p = mean(abs(ts) >= abs(obs[2]) - 1e-12), weights = if (webb) "Webb six-point" else "Rademacher", G = G)
}

fit_one <- function(measure, cl_flag) {
  v <- paste0("bank_shock_", measure)
  d <- agg[credit_line == cl_flag & !is.na(loan_growth) & !is.na(get(v)),
           .(entidad, identificador, fecha_reporte, loan_growth, bank_shock = get(v))]
  n_start <- nrow(d)
  d[, n_banks_cell := uniqueN(entidad), by = .(identificador, fecha_reporte)]
  d <- d[n_banks_cell >= 2L]
  m <- feols(loan_growth ~ bank_shock | identificador^fecha_reporte, data = d, cluster = ~entidad, notes = FALSE)
  n_used <- nobs(m)
  # estimation sample as fixest used it
  keep <- if (!is.null(m$obs_selection$obsRemoved)) setdiff(seq_len(nrow(d)), abs(m$obs_selection$obsRemoved)) else seq_len(nrow(d))
  e <- d[keep]
  stopifnot(nrow(e) == n_used)
  e[, cell := .GRP, by = .(identificador, fecha_reporte)]
  e[, clid := match(entidad, sort(unique(entidad)))]
  wb <- wcr_boot(e$loan_growth, e$bank_shock, e$cell, e$clid, N_BOOT, SEED)
  se_fx <- se(m)[["bank_shock"]]
  # sanity: the t-statistic used inside the bootstrap equals fixest's unadjusted CR0 t-statistic
  se_cr0 <- se(m, ssc = ssc(adj = FALSE, cluster.adj = FALSE))[["bank_shock"]]
  stopifnot(abs(abs(coef(m)[["bank_shock"]] / wb$t_cr0) / se_cr0 - 1) < 1e-6)
  bank_tab <- merge(e[, .(n_obs = .N, n_firms = uniqueN(identificador)), by = entidad],
                    unique(agg[, .(entidad, bank_standardized)]), by = "entidad")[order(-n_obs)]
  bank_tab[, `:=`(measure = measure, credit_line = cl_flag)]
  list(model = m, boot = wb, sample = e, banks = bank_tab,
       stats = data.table(
         measure = measure, credit_line = cl_flag,
         coef = coef(m)[["bank_shock"]], se_cluster = se_fx, se_cluster_cr0 = se_cr0,
         p_cluster_t_G1 = pvalue(m)[["bank_shock"]], p_wild_cluster_boot = wb$p,
         n_obs = n_used, n_before_multibank_filter = n_start,
         n_dropped_by_multibank_filter = n_start - nrow(d), n_dropped_by_fixest = nrow(d) - n_used,
         n_banks = uniqueN(e$entidad), n_clusters = wb$G, n_firms = uniqueN(e$identificador),
         n_firm_date_cells = uniqueN(e$cell), n_cells_zero_shock_variation = {
           z <- e[, .(r = max(bank_shock) - min(bank_shock)), by = cell]; sum(z$r == 0) },
         r2 = r2(m, "r2"), r2_within = r2(m, "wr2"), mean_y = mean(e$loan_growth), sd_shock = sd(e$bank_shock),
         boot_weights = wb$weights))
}

fits <- list()
for (j in MEASURES) for (cl_flag in c(TRUE, FALSE)) {
  key <- paste(j, if (cl_flag) "credit_line" else "other_credit", sep = "|")
  cat(sprintf("\n-- %s, credit_line == %s --\n", j, cl_flag))
  fits[[key]] <- fit_one(j, cl_flag)
  print(summary(fits[[key]]$model))
}
reg <- rbindlist(lapply(fits, `[[`, "stats"))
write_val(reg, "regression_summary")
cat("\nRegression summary:\n")
print(reg[, .(measure, credit_line, coef = signif(coef, 4), se = signif(se_cluster, 4), p_t = round(p_cluster_t_G1, 3),
              p_wild = round(p_wild_cluster_boot, 3), n_obs, n_banks, n_firms, cells = n_firm_date_cells,
              cells_no_var = n_cells_zero_shock_variation)])
cat("\nSample construction per regression:\n")
print(reg[, .(measure, credit_line, n_before_multibank_filter, n_dropped_by_multibank_filter, n_dropped_by_fixest, n_obs)])
reg_banks <- rbindlist(lapply(fits, `[[`, "banks"))
write_val(reg_banks, "regression_banks")
cat("\nBanks (= clusters) in each estimation sample, with observations:\n")
print(dcast(reg_banks, bank_standardized ~ measure + credit_line, value.var = "n_obs", fill = 0L))
cat(sprintf("\nfixest small-sample factor on the clustered SE (relative to CR0): %s\n",
            paste(sprintf("%.2f", reg$se_cluster / reg$se_cluster_cr0), collapse = ", ")))
few <- reg[n_clusters < FEW_CLUSTERS]
if (nrow(few)) cat(sprintf("\nLIMITED-CLUSTER FLAG: %d of %d regressions have fewer than %d bank clusters (range %d-%d); conventional clustered inference is unreliable, so wild-cluster-bootstrap p-values (%s weights, B = %d) are reported next to the t(G-1) p-values.\n",
                           nrow(few), nrow(reg), FEW_CLUSTERS, min(reg$n_clusters), max(reg$n_clusters),
                           paste(unique(reg$boot_weights), collapse = "/"), N_BOOT))

# ==============================================================================
# 6. LaTeX table
# ==============================================================================
hdr("6. LaTeX table")
mods <- list(fits[["gen_min_legal|credit_line"]]$model, fits[["gen_min_legal|other_credit"]]$model,
             fits[["gen_marginal|credit_line"]]$model, fits[["gen_marginal|other_credit"]]$model)
ord <- c("gen_min_legal|credit_line", "gen_min_legal|other_credit", "gen_marginal|credit_line", "gen_marginal|other_credit")
rg <- reg[match(ord, paste(measure, fifelse(credit_line, "credit_line", "other_credit"), sep = "|"))]
pf <- function(p) formatC(p, format = "f", digits = 3)
extra <- list(
  "^Banks (= bank clusters)"         = formatC(rg$n_banks, format = "d"),
  "^Firms"                           = formatC(rg$n_firms, format = "d", big.mark = ","),
  "^Firm-by-date cells"              = formatC(rg$n_firm_date_cells, format = "d", big.mark = ","),
  "^Wild cluster bootstrap $p$-value" = pf(rg$p_wild_cluster_boot),
  "^Mean of loan growth"             = formatC(rg$mean_y, format = "f", digits = 3)
)
dict <- c(bank_shock = "Bank shock", loan_growth = "Loan growth",
          "identificador^fecha_reporte" = "Firm $\\times$ date")
notes <- paste0(
  "\\textit{Notes:} The dependent variable is the log change in a firm's credit balance (\\textit{cr\\'edito vigente}) at a bank between ",
  "consecutive month-ends, $\\log B_{bit}-\\log B_{bi,t-1}$, so it measures growth among continuing relationships with strictly positive balances at ",
  "both dates; loan entry and exit are not captured. Balances are summed across products and currencies within a bank--firm--credit-line status ",
  "(foreign-currency balances are reported in soles at the month-end exchange rate, so growth includes exchange-rate revaluation). ",
  "Columns (1) and (3) use credit lines (credit cards, current accounts, letters of credit, guarantees, multi-lines, other lines); columns (2) and (4) use all other credit. ",
  "Because the balance is performing drawn credit, letters of credit, guarantees, multi-lines and other lines have a zero balance in every record, so the credit-line samples consist of credit cards and current accounts. ",
  "The bank shock is $s^{mn}_{b,0}\\Delta r^{mn}_{t}+s^{me}_{b,0}\\Delta r^{me}_{t}$, where $\\Delta r$ is the month-end change, in percentage points, of the ",
  "statutory minimum legal reserve rate (columns 1--2) or marginal reserve rate (columns 3--4) on domestic (mn) and foreign (me) currency obligations ",
  "(positive = tighter), and $s_{b,0}$ are the bank's fixed ", format(BASELINE_DATE, "%B %Y"), " deposit shares in each currency. ",
  "The marginal-rate shock is undefined when the domestic regime is a single rate, which shortens columns (3)--(4). ",
  "Each regression keeps firm-by-date cells with at least two banks and absorbs firm-by-date fixed effects. ",
  "Standard errors, in parentheses, are clustered by bank; with few clusters the $p$-value from a wild cluster bootstrap ",
  "(null imposed, ", paste(unique(reg$boot_weights), collapse = " / "), " weights, ", format(N_BOOT, big.mark = ","), " draws) is reported. ",
  "* $p<0.10$, ** $p<0.05$, *** $p<0.01$ from the conventional clustered $t(G-1)$ test.")
tex_file <- file.path(TABS, "tab_encaje_loan_growth.tex")
etable(mods, tex = TRUE, file = tex_file, replace = TRUE, dict = dict, tpt = TRUE, notes = notes,
       title = "Reserve-requirement shocks and loan growth: credit lines versus other credit",
       label = "tab:encaje_loan_growth",
       headers = list("^Shock measure" = list("Minimum legal rate" = 2, "Marginal rate" = 2),
                      "^Credit type" = rep(c("Credit lines", "Other credit"), 2)),
       fitstat = ~ n + r2 + wr2, extralines = extra, style.tex = style.tex("aer"),
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", digits.stats = 3,
       postprocess.tex = function(x) sub("(Shock measure[^\n]*\\\\ *\n)", "\\1\\\\cmidrule(lr){2-3} \\\\cmidrule(lr){4-5}\n", x),
       fontsize = "small", placement = "!h", depvar = FALSE)
cat("Wrote ", tex_file, "\n", sep = "")

hdr("Session")
print(sessionInfo())
sink(); close(log_con)
