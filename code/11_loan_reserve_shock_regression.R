# ==============================================================================
# 11_loan_reserve_shock_regression.R -- Khwaja-Mian-style reduced-form loan-growth
# regressions on the bank reserve-requirement shocks built in code/10_reserve_shocks.R
# ==============================================================================
# Run from the project root:   Rscript code/11_loan_reserve_shock_regression.R
# Requires:                    Rscript code/10_reserve_shocks.R and
#                              Rscript code/11a_credit_classification.R (run first)
#
# DESIGN
#   Credit rows are classified as credit lines or term loans in 11a_credit_classification.R
#   The outcome is log growth between two consecutive month-ends of a firm's position at a bank:
#     credit lines (main):        total commitment = credito_vigente + linea_no_utilizada
#     credit lines (robustness):  drawn credit     = credito_vigente
#     term loans:                 drawn credit     = credito_vigente
#
#   The shocks are keyed by bank_standardized (the project's hub key); the credit
#   file identifies banks by the RCC entity code (`entidad`). This script builds
#   the entidad -> bank_standardized crosswalk, aggregates credit to firm x bank x
#   credit-line status x month, and then merges that panel to the shocks through it:
#   panel row -> entidad -> bank_standardized -> shock.
#
# INPUTS (never modified)
#   data/processed/credit_classified_v2.parquet      classified credit rows, v2 sample (11a_credit_classification.R)
#   data/processed/bank_encaje_shocks.csv            bank shocks, unique by bank_standardized x fecha_reporte (10_reserve_shocks.R)
#   data/documentation/RCC_empresas_supervisadas_2026.xls   RCC entity code -> name (tab-delimited text
#                                                    saved with an .xls extension, ISO-8859-1)
#   data/documentation/sbs_entity_codes_2013.csv     SBS 2013 entity-code table (from INSTRUCTIVO_RO-ROPE 2013.pdf);
#                                                    names for codes missing from the 2026 list
#   data/documentation/bank_map_manual.csv           curated name -> bank_standardized hub map
#
# OUTPUTS
#   data/processed/crosswalks/xwalk_rcc.csv          entidad code -> bank_standardized (project spoke format + code)
#   data/processed/credit_line_growth_panel.csv.gz   relationship-month panel (entidad x firm x credit_class x month) with growth and shocks
#   output/tables/tab_encaje_loan_growth.tex         main table: credit-line commitment growth vs term-loan growth (all lenders)
#   output/tables/tab_encaje_loan_growth_banks.tex   same table, commercial banks only
#   output/tables/tab_encaje_loan_growth_drawn.tex   robustness: credit-line drawn-credit growth
#   output/validation/encaje_shock_*.csv             diagnostics (coverage, classification, exclusions, regressions)
#   output/logs/11_loan_reserve_shock_regression.log this script's console log
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(fwildclusterboot)
})
source("code/00_sbs_utils.R")   # canon_bank(), standardize_banks(), read_bank_map(); loads dplyr/stringr, hardens the locale

# ---- parameters ---------------------------------------------------------------
CREDIT_PQ  <- "data/processed/credit_classified_v2.parquet"
SHOCKS_CSV <- "data/processed/bank_encaje_shocks.csv"
RCC_LIST   <- "data/documentation/RCC_empresas_supervisadas_2026.xls"
SBS_2013   <- "data/documentation/sbs_entity_codes_2013.csv"    # extracted from INSTRUCTIVO_RO-ROPE 2013.pdf
PROC  <- "data/processed"
VALID <- "output/validation"
TABS  <- "output/tables"
LOGF  <- "output/logs/11_loan_reserve_shock_regression.log"

# Shock measures regressed (columns of bank_encaje_shocks.csv: bank_shock_<measure>); the table
# layout (two panels of columns) assumes exactly these two.
MEASURES <- c("gen_min_legal", "gen_marginal")

REG_CLASSES <- c("credit_line", "term_loan")    # 11a classes used; "undefined" and "ignored" are dropped

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
cr <- as.data.table(read_parquet(CREDIT_PQ, col_select = c("fecha_reporte", "identificador", "entidad", "moneda_codigo",
                                                           "producto", "credito_vigente", "linea_no_utilizada", "credit_class")))
cat(sprintf("Credit file: %s rows, %s firms, %d entidad codes, %d dates.\n", fmt(nrow(cr)),
            fmt(uniqueN(cr$identificador)), uniqueN(cr$entidad), uniqueN(cr$fecha_reporte)))
stopifnot(inherits(cr$fecha_reporte, "Date"), is.numeric(cr$credito_vigente), is.numeric(cr$linea_no_utilizada),
          !anyNA(cr))     # the extract is complete in every column used (no missing product, firm, bank, currency, date, balance or class)
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

# Codes of entities that closed before 2026 come from the SBS 2013 entity-code table. They are added
# only for codes absent from the 2026 list: where a code appears in both, the 2026 name wins, so codes
# later reused or renamed (e.g. 72 Crediscotia -> Santander Consumer, 156 CRAC Chavin -> CRAC Raiz) keep
# their current identity -- successions are not handled.
old <- fread(SBS_2013, encoding = "UTF-8")
stopifnot(identical(names(old), c("tipo_sbs", "entidad", "nombre")), !anyDuplicated(old$entidad))
SBS_TIPO <- c(B = "Banco", C = "Caja Municipal de Ahorro y Crédito", R = "Caja Rural de Ahorro y Crédito",
              F = "Financiera", E = "Edpyme")
old <- old[!entidad %in% rcc$entidad,
           .(codigo = sprintf("%05d", entidad), tipo = fcoalesce(SBS_TIPO[tipo_sbs], paste0("SBS type ", tipo_sbs)),
             estado = "Not in 2026 list (SBS 2013 table)", bank_raw = nombre, entidad)]
rcc <- rbind(rcc, old)
stopifnot(!anyDuplicated(rcc$entidad), !anyDuplicated(rcc$bank_raw))

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
                                                                "caja / cooperativa: RCC name not in the bank map",
  default =                                                     "not in Reporte or any bulletin crosswalk")]
rcc <- merge(rcc, as.data.table(bank_types(manual)), by = "bank_standardized", all.x = TRUE, sort = FALSE)
fwrite(rcc[order(entidad), .(entidad, bank_raw, tipo, estado, bank_standardized, entity_type, review_required,
                             unmapped_note, in_credit_sample)],
       file.path(PROC, "crosswalks", "xwalk_rcc.csv"))
cat(sprintf("RCC list: %d codes (%s). Mapped to a hub bank_standardized: %d; unmapped: %d.\n",
            nrow(rcc), paste(names(table(rcc$estado)), table(rcc$estado), sep = "=", collapse = ", "),
            sum(!is.na(rcc$bank_standardized)), sum(is.na(rcc$bank_standardized))))
cat(sprintf("Credit sample: %d distinct entidad codes; %d are in the 2026/2013 entity lists, %d are not: %s\n",
            length(credit_entidades), sum(credit_entidades %in% rcc$entidad), sum(!credit_entidades %in% rcc$entidad),
            paste(sort(setdiff(credit_entidades, rcc$entidad)), collapse = ", ")))
# a code maps to one bank; several codes MAY share a bank (none do here)
dupb <- rcc[!is.na(bank_standardized), .N, by = bank_standardized][N > 1]
cat(sprintf("Hub banks reached by more than one RCC code: %s\n",
            if (nrow(dupb)) paste(dupb$bank_standardized, collapse = ", ") else "none"))

# ==============================================================================
# 2. Credit classification (from 11a_credit_classification.R)
# ==============================================================================
hdr("2. Credit classification")

N_FIRMS_SAMPLE <- uniqueN(cr$identificador)     # full sample size, reported in the table notes
cat(sprintf("Keeping classes %s; dropping %s rows of other classes.\n", paste(REG_CLASSES, collapse = ", "),
            fmt(sum(!cr$credit_class %in% REG_CLASSES))))
cr <- cr[credit_class %in% REG_CLASSES]

# Entity statistics (computed before the merge, on the raw credit rows)
ent_stats <- cr[, .(n_firms = uniqueN(identificador), n_obs = .N,
                    first_date = min(fecha_reporte), last_date = max(fecha_reporte),
                    sum_credito_vigente = sum(credito_vigente)), by = entidad]

# ==============================================================================
# 3. Currency, aggregation and merge with shocks
# ==============================================================================
hdr("3. Currency, aggregation and merge with shocks")

# ---- aggregate: entidad x firm x credit_class x month (products and currencies summed) ----
agg <- cr[, .(
  balance = sum(credito_vigente), unused = sum(linea_no_utilizada),
  n_products = uniqueN(producto), n_raw_rows = .N,
  bal_me = sum(credito_vigente[moneda_codigo == "2"]),
  com_me = sum((credito_vigente + linea_no_utilizada)[moneda_codigo == "2"])),
  by = .(entidad, identificador, credit_class, fecha_reporte)]
agg[, commitment := balance + unused]
agg[, `:=`(me_balance_share    = fifelse(balance > 0, bal_me / balance, NA_real_),          # NA when the base is zero
           me_commitment_share = fifelse(commitment > 0, com_me / commitment, NA_real_))]
cat(sprintf("Aggregated to %s entidad x firm x credit_class x month rows (from %s product-currency rows).\n",
            fmt(nrow(agg)), fmt(sum(agg$n_raw_rows))))
cat(sprintf("  negative balances: %d; zero balances: %s; zero commitments: %s\n", sum(agg$balance < 0),
            fmt(sum(agg$balance == 0)), fmt(sum(agg$commitment == 0))))

# ---- merge shocks: panel row -> entidad -> bank_standardized -> shock --------------
# Both steps are many-to-one: the crosswalk is unique by entidad and the shock table is unique by
# (bank_standardized, fecha_reporte) (both asserted), so no panel row can be multiplied or lost.
shock_cols <- paste0("bank_shock_", MEASURES)
n_agg <- nrow(agg)
agg <- merge(agg, rcc[, .(entidad, bank_standardized, entity_type)], by = "entidad", all.x = TRUE, sort = FALSE)
stopifnot(nrow(agg) == n_agg)
agg <- merge(agg, shocks[, c("bank_standardized", "fecha_reporte", "s_mn_0", shock_cols), with = FALSE],
             by = c("bank_standardized", "fecha_reporte"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(agg) == n_agg)

# Entity coverage and the reason each unmatched code is excluded
cov <- merge(ent_stats, rcc[, .(entidad, bank_raw, tipo, estado, bank_standardized, unmapped_note)], by = "entidad", all.x = TRUE)
cov <- merge(cov, unique(shocks[, .(bank_standardized, s_mn_0, s_me_0)]), by = "bank_standardized", all.x = TRUE)
cov[, status := fcase(
  !entidad %in% rcc$entidad,           "excluded: code not in the 2026 or 2013 entity lists",
  is.na(bank_standardized),            paste0("excluded: no hub id, ", unmapped_note),
  is.na(s_mn_0),                       "excluded: mapped to a bank without valid baseline deposit shares",
  default =                            "included: shock available")]
setorder(cov, -sum_credito_vigente)
write_val(cov[, .(entidad, bank_raw, tipo, estado, bank_standardized, n_firms, n_obs, first_date, last_date,
                  sum_credito_vigente, s_mn_0, s_me_0, status)], "entity_coverage")
stopifnot(setequal(cov[status == "included: shock available", entidad], agg[!is.na(s_mn_0), unique(entidad)]))   # coverage agrees with the merge
cat("\nEntity coverage (credit sample), by status:\n")
print(cov[, .(entidades = .N, firms_obs_rows = sum(n_obs), share_of_balance = round(sum(sum_credito_vigente) / sum(cov$sum_credito_vigente), 4)), by = status])
cat("\nInstitutions with a shock (included):\n")
print(cov[status == "included: shock available", .(entidad, bank_standardized, tipo, n_firms, n_obs, s_mn_0 = round(s_mn_0, 3), s_me_0 = round(s_me_0, 3))])
cat("\nLargest excluded entities by balance:\n")
print(head(cov[status != "included: shock available", .(entidad, bank_raw, status, n_firms, share = round(sum_credito_vigente / sum(cov$sum_credito_vigente), 4))], 12))

# ==============================================================================
# 4. Loan growth
# ==============================================================================
hdr("4. Loan growth")
agg[, idx := match(fecha_reporte, cr_dates)]
setorder(agg, entidad, identificador, credit_class, idx)
agg[, `:=`(bal_lag = shift(balance),
           com_lag = shift(commitment),
           gap     = idx - shift(idx)),
    by = .(entidad, identificador, credit_class)]

# Valid growth requires consecutive months and positive values at t and t-1 (separately for drawn
# credit and commitments). Excluded: first observation, reporting gaps, zero or negative values.
agg[, valid_growth        := fcoalesce(gap == 1L & balance > 0 & bal_lag > 0, FALSE)]
agg[, valid_commit_growth := fcoalesce(gap == 1L & commitment > 0 & com_lag > 0, FALSE)]
agg[(valid_growth),        loan_growth   := log(balance) - log(bal_lag)]
agg[(valid_commit_growth), commit_growth := log(commitment) - log(com_lag)]
print(agg[, .(rows = .N, valid_drawn = sum(valid_growth), valid_commitment = sum(valid_commit_growth)), by = credit_class])

fwrite(agg[, .(entidad, bank_standardized, entity_type, identificador, credit_class, fecha_reporte,
               balance, unused, commitment, bal_lag, com_lag, valid_growth, valid_commit_growth,
               loan_growth, commit_growth, n_products, me_balance_share, me_commitment_share,
               bank_shock_gen_min_legal, bank_shock_gen_marginal)],
       file.path(PROC, "credit_line_growth_panel.csv.gz"))

# ==============================================================================
# 5. Regressions
# ==============================================================================
hdr("5. Regressions")
# Using Wild cluster bootstrap for small number of clusters (Cameron, Gelbach and Miller, 2008; Roodman et al., 2019)
fit_one <- function(measure, cls, depvar, smp) {
  v <- paste0("bank_shock_", measure)
  fit_dat <- agg[credit_class == cls & !is.na(get(depvar)) & !is.na(get(v)) &
                   (smp == "all" | entity_type %in% SAMPLE_TYPES[[smp]]),
           .(entidad, identificador, fecha_reporte, growth = get(depvar), bank_shock = get(v))]
  # keep firm-date cells with at least two lending institutions (within the sample)
  fit_dat <- fit_dat[, if (uniqueN(entidad) >= 2L) .SD, by = .(identificador, fecha_reporte)]
  fit_dat[, cell := .GRP, by = .(identificador, fecha_reporte)]
  # boottest() re-evaluates the model's `data` argument in the global environment
  assign("fit_dat", fit_dat, envir = globalenv())

  m <- feols(growth ~ bank_shock | cell, data = fit_dat, cluster = ~entidad, notes = FALSE)
  
  G <- fitstat(m, "g")[[1]]
  set.seed(SEED); dqrng::dqset.seed(SEED)
  bt <- boottest(m, param = "bank_shock", clustid = "entidad", fe = "cell", B = N_BOOT, type = if (G < 12L) "webb" else "rademacher")

  list(model = m,
       stats = data.table(sample = smp, measure = measure, credit_class = cls, depvar = depvar,
                          coef = coef(m)[["bank_shock"]], se_cluster = se(m)[["bank_shock"]],
                          p_cluster_t_G1 = pvalue(m)[["bank_shock"]], p_wild_cluster_boot = bt$p_val,
                          n_obs = nobs(m), n_banks = G, n_clusters = G, n_firms = uniqueN(fit_dat$identificador),
                          n_firm_date_cells = uniqueN(fit_dat$cell), r2_within = r2(m, "wr2"),
                          mean_y = mean(fitted(m) + resid(m))))
}

# specifications: credit-line commitment growth (main), term-loan drawn growth, credit-line drawn growth (robustness)
SPECS <- data.table(spec   = c("cl_commit", "term_loan", "cl_drawn"),
                    cls    = c("credit_line", "term_loan", "credit_line"),
                    depvar = c("commit_growth", "loan_growth", "loan_growth"))
# samples: every institution with a shock vs commercial banks (banca multiple) only
SAMPLE_TYPES <- list(banks = "banca_multiple")
SAMPLES <- c("all", "banks")
fits <- list()
for (smp in SAMPLES) for (j in MEASURES) for (k in seq_len(nrow(SPECS))) {
  key <- paste(smp, j, SPECS$spec[k], sep = "|")
  cat(sprintf("\n-- sample %s: %s, %s, %s --\n", smp, j, SPECS$cls[k], SPECS$depvar[k]))
  fits[[key]] <- fit_one(j, SPECS$cls[k], SPECS$depvar[k], smp)
  fits[[key]]$stats[, spec := SPECS$spec[k]]
  print(summary(fits[[key]]$model))
}
reg <- rbindlist(lapply(fits, `[[`, "stats"))
write_val(reg, "regressions")
cat("\nRegression summary:\n")
print(reg[, .(sample, measure, spec, coef = signif(coef, 4), se = signif(se_cluster, 4), p_t = round(p_cluster_t_G1, 3),
              p_wild = round(p_wild_cluster_boot, 3), n_obs, n_banks, n_firms)])
few <- reg[n_clusters < FEW_CLUSTERS]
if (nrow(few)) cat(sprintf("\nLIMITED-CLUSTER FLAG: %d of %d regressions have fewer than %d bank clusters; wild-cluster-bootstrap p-values are reported next to the t(G-1) p-values.\n",
                           nrow(few), nrow(reg), FEW_CLUSTERS))

# ==============================================================================
# 6. LaTeX table
# ==============================================================================
hdr("6. LaTeX tables")
pf <- function(p) formatC(p, format = "f", digits = 3)
dict <- c(bank_shock = "Bank shock", cell = "Firm $\\times$ date")

write_table <- function(keys, file, headers, title, label, notes, postprocess = identity) {
  rg <- reg[match(keys, paste(sample, measure, spec, sep = "|"))]
  stopifnot(!anyNA(rg$coef))
  extra <- list(
    "^Lenders (= clusters)"             = formatC(rg$n_banks, format = "d"),
    "^Firms"                            = formatC(rg$n_firms, format = "d", big.mark = ","),
    "^Firm-by-date cells"               = formatC(rg$n_firm_date_cells, format = "d", big.mark = ","),
    "^Wild cluster bootstrap $p$-value" = pf(rg$p_wild_cluster_boot),
    "^Mean of dependent variable"       = formatC(rg$mean_y, format = "f", digits = 3))
  etable(lapply(fits[keys], `[[`, "model"), tex = TRUE, file = file, replace = TRUE, dict = dict, tpt = TRUE,
         notes = notes, title = title, label = label, headers = headers,
         fitstat = ~ n + r2 + wr2, extralines = extra, style.tex = style.tex("aer"),
         signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", digits.stats = 3,
         postprocess.tex = postprocess, fontsize = "small", placement = "!h", depvar = FALSE)
  cat("Wrote ", file, "\n", sep = "")
}

sample_note <- c(
  all   = "Lenders are all institutions with a reserve shock: commercial banks, cajas municipales and rurales, and financieras. ",
  banks = "Lenders are commercial banks (\\textit{banca m\\'ultiple}) only; firm-by-date cells need at least two banks. ")
common_notes <- paste0(
  "Credit is classified with a manual product table: credit cards, current-account advances, guarantees, letters of credit, credits to be settled, ",
  "multi-line facilities and other lines are credit lines; leasing, lease-back, real-estate, mortgage, pawn, income-property and RFA credit are term loans; ",
  "\\textit{pr\\'estamos} are credit lines when reported as revolving, term loans when non-revolving, and, when the revolving status is not identified, ",
  "credit lines if the row carries an unused line and term loans otherwise. Products whose nature is ambiguous (overdrafts, factoring, discounting, ",
  "foreign-trade finance, project finance, other credits, credit to consolidable entities) are excluded from all columns. ",
  "Amounts are summed across products and currencies within a bank--firm--credit-type cell ",
  "(foreign-currency amounts are reported in soles at the month-end exchange rate, so growth includes exchange-rate revaluation). ",
  "Growth is computed for continuing relationships with strictly positive amounts at both dates; entry and exit are not captured. ",
  "The bank shock is $s^{mn}_{b,0}\\Delta r^{mn}_{t}+s^{me}_{b,0}\\Delta r^{me}_{t}$, where $\\Delta r$ is the month-end change, in percentage points, of the ",
  "statutory minimum legal reserve rate or marginal reserve rate on domestic (mn) and foreign (me) currency obligations ",
  "(positive = tighter), and $s_{b,0}$ are the bank's fixed ", format(BASELINE_DATE, "%B %Y"), " deposit shares in each currency. ",
  "The marginal-rate shock is undefined when the domestic regime is a single rate, which shortens the marginal-rate columns. ",
  "Each regression keeps firm-by-date cells with at least two lenders and absorbs firm-by-date fixed effects. ",
  "Sample: RCC sample of ", fmt(N_FIRMS_SAMPLE), " firms, ", format(min(cr_dates), "%B %Y"), " to ", format(max(cr_dates), "%B %Y"), ". ",
  "Standard errors, in parentheses, are clustered by lender; with few clusters the $p$-value from a wild cluster bootstrap ",
  "(null imposed, ", format(N_BOOT, big.mark = ","), " draws) is reported. ",
  "* $p<0.10$, ** $p<0.05$, *** $p<0.01$ from the conventional clustered $t(G-1)$ test.")

# main table (all lenders) and the same layout for banks only
main_notes <- paste0("\\textit{Notes:} The dependent variable is the log change between consecutive month-ends of a firm's position at a lender. ",
                     "For credit lines (columns 1 and 3) it is the total commitment, drawn credit (\\textit{cr\\'edito vigente}) plus the unused line ",
                     "(\\textit{l\\'inea no utilizada}); for term loans (columns 2 and 4) it is drawn credit. ")
for (smp in SAMPLES) write_table(
  keys = paste(smp, c("gen_min_legal|cl_commit", "gen_min_legal|term_loan", "gen_marginal|cl_commit", "gen_marginal|term_loan"), sep = "|"),
  file = file.path(TABS, if (smp == "all") "tab_encaje_loan_growth.tex" else "tab_encaje_loan_growth_banks.tex"),
  headers = list("^Shock measure" = list("Minimum legal rate" = 2, "Marginal rate" = 2),
                 "^Credit type"   = rep(c("Credit lines", "Term loans"), 2),
                 "^Outcome"       = rep(c("Commitment", "Drawn"), 2)),
  title = paste0("Reserve-requirement shocks and credit growth: credit lines versus term loans",
                 if (smp == "banks") " (commercial banks only)" else ""),
  label = if (smp == "all") "tab:encaje_loan_growth" else "tab:encaje_loan_growth_banks",
  notes = paste0(main_notes, sample_note[[smp]], common_notes),
  postprocess = function(x) sub("(Shock measure[^\n]*\\\\ *\n)", "\\1\\\\cmidrule(lr){2-3} \\\\cmidrule(lr){4-5}\n", x))

# robustness: credit-line drawn growth
write_table(
  keys = c("all|gen_min_legal|cl_drawn", "all|gen_marginal|cl_drawn"),
  file = file.path(TABS, "tab_encaje_loan_growth_drawn.tex"),
  headers = list("^Shock measure" = c("Minimum legal rate", "Marginal rate")),
  title = "Reserve-requirement shocks and drawn credit-line balances (robustness)",
  label = "tab:encaje_loan_growth_drawn",
  notes = paste0("\\textit{Notes:} The dependent variable is the log change between consecutive month-ends of a firm's drawn credit-line balance ",
                 "(\\textit{cr\\'edito vigente}) at a lender, excluding the unused part of the line; the main specification instead uses ",
                 "total commitments (drawn plus unused). ", sample_note[["all"]], common_notes))

hdr("Session")
print(sessionInfo())
sink(); close(log_con)
