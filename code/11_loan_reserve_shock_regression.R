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
#   the entidad -> bank_standardized crosswalk, aggregates credit to firm x bank x
#   credit-line status x month, and then merges that panel to the shocks through it:
#   panel row -> entidad -> bank_standardized -> shock.
#
# INPUTS (never modified)
#   data/raw/muestra_anonima_base2015.parquet        credit, firm x bank x product x currency x month
#   data/processed/bank_encaje_shocks.csv            bank shocks, unique by bank_standardized x fecha_reporte (10_reserve_shocks.R)
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
stopifnot(inherits(cr$fecha_reporte, "Date"), is.numeric(cr$credito_vigente),
          !anyNA(cr))     # the extract is complete in every column used (no missing product, firm, bank, currency, date or balance)
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
# 2. Product classification
# ==============================================================================
hdr("2. Product classification")
# Normalize whitespace and case only; product meanings unchanged.
cr[, producto := tolower(trimws(gsub("\\s+", " ", as.character(producto))))]
stopifnot(all(cr$producto != ""))
unknown <- setdiff(CREDIT_LINE_PRODUCTS, unique(cr$producto))
if (length(unknown)) stop("credit-line products absent from the data: ", paste(unknown, collapse = ", "))
cr[, credit_line := producto %in% CREDIT_LINE_PRODUCTS]

pc <- cr[, .(n_rows = .N, n_firms = uniqueN(identificador), sum_credito_vigente = sum(credito_vigente),
             n_positive_rows = sum(credito_vigente > 0),
             share_positive_rows = mean(credito_vigente > 0)), by = .(producto, credit_line)][order(-credit_line, -n_rows)]
write_val(pc, "product_classification")
cat("Product classification (credit_line = TRUE for the six listed products):\n")
print(pc[, .(producto, credit_line, n_rows, n_firms, n_positive_rows, share_positive_rows = round(share_positive_rows, 3))], nrows = 50)
cat(sprintf("NOTE: credito_vigente is identically zero for %s; credit-line growth therefore rests on %s.\n",
            paste(pc[credit_line == TRUE & n_positive_rows == 0, producto], collapse = ", "),
            paste(pc[credit_line == TRUE & n_positive_rows > 0, producto], collapse = " and ")))
cat(sprintf("Rows: credit_line TRUE %s | FALSE %s.\n", fmt(sum(cr$credit_line)), fmt(sum(!cr$credit_line))))
cat(sprintf("credito_vigente: negative %s | zero %s (%.1f%%) | positive %s\n",
            fmt(sum(cr$credito_vigente < 0)), fmt(sum(cr$credito_vigente == 0)),
            100 * mean(cr$credito_vigente == 0), fmt(sum(cr$credito_vigente > 0))))

# Entity statistics (computed before the merge, on the raw credit rows)
ent_stats <- cr[, .(n_firms = uniqueN(identificador), n_obs = .N,
                    first_date = min(fecha_reporte), last_date = max(fecha_reporte),
                    sum_credito_vigente = sum(credito_vigente)), by = entidad]

# ==============================================================================
# 3. Currency, aggregation and merge with shocks
# ==============================================================================
hdr("3. Currency, aggregation and merge with shocks")

# ---- aggregate: entidad x firm x credit_line x month (products and currencies summed) ----
agg <- cr[, .(
  balance = sum(credito_vigente), n_products = uniqueN(producto), n_raw_rows = .N,
  bal_me = sum(credito_vigente[moneda_codigo == "2"])),
  by = .(entidad, identificador, credit_line, fecha_reporte)]
agg[, me_balance_share := fifelse(balance > 0, bal_me / balance, NA_real_)]   # undefined (NA) when the balance is zero
cat(sprintf("Aggregated to %s entidad x firm x credit_line x month rows (from %s product-currency rows).\n",
            fmt(nrow(agg)), fmt(sum(agg$n_raw_rows))))
cat(sprintf("  negative balances: %d; zero balances: %s\n", sum(agg$balance < 0), fmt(sum(agg$balance == 0))))

# ---- merge shocks: panel row -> entidad -> bank_standardized -> shock --------------
# Both steps are many-to-one: the crosswalk is unique by entidad and the shock table is unique by
# (bank_standardized, fecha_reporte) (both asserted), so no panel row can be multiplied or lost.
shock_cols <- paste0("bank_shock_", MEASURES)
n_agg <- nrow(agg)
agg <- merge(agg, rcc[, .(entidad, bank_standardized)], by = "entidad", all.x = TRUE, sort = FALSE)
stopifnot(nrow(agg) == n_agg)
agg <- merge(agg, shocks[, c("bank_standardized", "fecha_reporte", "s_mn_0", shock_cols), with = FALSE],
             by = c("bank_standardized", "fecha_reporte"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(agg) == n_agg)

# Entity coverage and the reason each unmatched code is excluded
cov <- merge(ent_stats, rcc[, .(entidad, bank_raw, tipo, estado, bank_standardized, unmapped_note)], by = "entidad", all.x = TRUE)
cov <- merge(cov, unique(shocks[, .(bank_standardized, s_mn_0, s_me_0)]), by = "bank_standardized", all.x = TRUE)
cov[, status := fcase(
  !entidad %in% rcc$entidad,           "excluded: code not in RCC list",
  is.na(bank_standardized),            paste0("excluded: no hub id, ", unmapped_note),
  is.na(s_mn_0),                       "excluded: mapped to a bank without valid baseline deposit shares",
  default =                            "included: shock available")]
setorder(cov, -sum_credito_vigente)
write_val(cov[, .(entidad, bank_raw, tipo, estado, bank_standardized, n_firms, n_obs, first_date, last_date,
                  sum_credito_vigente, s_mn_0, s_me_0, status)], "entity_coverage")
stopifnot(setequal(cov[status == "included: shock available", entidad], agg[!is.na(s_mn_0), unique(entidad)]))   # coverage agrees with the merge
cat("\nEntity coverage (credit sample), by status:\n")
print(cov[, .(entidades = .N, firms_obs_rows = sum(n_obs), share_of_balance = round(sum(sum_credito_vigente) / sum(cov$sum_credito_vigente), 4)), by = status])
cat("\nBanks with a shock (included):\n")
print(cov[status == "included: shock available", .(entidad, bank_standardized, n_firms, n_obs, s_mn_0 = round(s_mn_0, 3), s_me_0 = round(s_me_0, 3))])
cat("\nLargest excluded entities by balance:\n")
print(head(cov[status != "included: shock available", .(entidad, bank_raw, status, n_firms, share = round(sum_credito_vigente / sum(cov$sum_credito_vigente), 4))], 12))

# ==============================================================================
# 4. Loan growth
# ==============================================================================
hdr("4. Loan growth")
agg[, idx := match(fecha_reporte, cr_dates)]
setorder(agg, entidad, identificador, credit_line, idx)
agg[, `:=`(bal_lag = shift(balance),
           gap     = idx - shift(idx)),
    by = .(entidad, identificador, credit_line)]

# Valid growth requires consecutive months and positive balances at t and t-1.
# Excluded: first observation, reporting gaps, missing, negative or zero balances.
agg[, valid_growth := fcoalesce(gap == 1L & balance > 0 & bal_lag > 0, FALSE)]
agg[(valid_growth), loan_growth := log(balance) - log(bal_lag)]

fwrite(agg[, .(entidad, bank_standardized, identificador, credit_line, fecha_reporte,
               balance, bal_lag, valid_growth, loan_growth,
               n_products, me_balance_share,
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
