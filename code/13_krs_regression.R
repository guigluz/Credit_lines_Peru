# ==============================================================================
# 13_krs_regression.R -- Kashyap, Rajan and Stein (2002) regressions for Peruvian banks:
# commitment ratio (COMRAT) on deposit funding (DEPRAT)
# ==============================================================================
# Run from the project root:   Rscript code/13_krs_regression.R
#
# DESIGN
#   KRS (2002) show that banks funded with more transaction deposits also hold more
#   unused loan commitments: COMRAT = beta * DEPRAT + e, with
#     COMRAT = U / (U + L)    U = unused credit lines, L = loans
#     DEPRAT = transaction deposits / total deposits
#   Two independent measurements of COMRAT are used.
#
#   A. Bulletin approach (main). Bank x month panel, 2010-07 to 2022-09.
#      U  = total indirect credit in SBS cuadro B-2322 (total_indirect_thousand_pen): guarantees
#           (avales, cartas fianza), letters of credit, bank acceptances and unused lines / credit
#           granted but not disbursed (account 7205), all borrowers. The sample starts in 2010-07,
#           when unused lines entered the B-2322 total (Res. SBS 11356-2008).
#      L, deposits by type, assets, equity = SBS Series Estadisticas (Reporte), which ends 2022-09.
#      Banca Multiple only: B-2322 has no other institution type.
#
#   B. Credit-registry (RCC) approach (alternative). The firm sample in
#      muestra_anonima_base2015_v2.parquet was drawn in 2015, so 2015 is the only year in which
#      its bank aggregates represent banks' loan portfolios to firms. Per bank x month of 2015,
#      U = sum of no_utilizado_total (account 7205) and L = sum of credito_utilizado over the
#      sampled firms. This is a COMMERCIAL-portfolio COMRAT (firms only). DEPRAT, assets and
#      equity come from the Reporte.
#      No correction is made for banks that report few or no unused lines at firm level in
#      2015 (e.g. BCP); section 6 logs both COMRAT measures by bank (the bulletin one also covers consumer lending).
#
# INPUTS (never modified)
#   data/raw/Reporte_SistemaFinanciero.csv            SBS Series Estadisticas, institution x month,
#                                                     2001-01..2022-09, miles de S/ (UTF-8 with BOM)
#   data/processed/crosswalks/xwalk_reporte.csv       Reporte name -> bank_standardized, entity_type
#   data/processed/b2322_unused_credit_panel.csv      indirect credit, bank x month (02_clean_indirect.R)
#   data/processed/b2358_branch_panel.csv.gz          deposits by type, branch x month (02_clean_deposits.R);
#                                                     used only to cross-check the Reporte deposits
#   data/raw/muestra_anonima_base2015_v2.parquet      RCC firm sample (amounts in soles)
#   data/processed/crosswalks/xwalk_rcc.csv           RCC entidad code -> bank_standardized (11_*.R)
#
# OUTPUTS
#   data/processed/krs_bank_panel.csv                 bulletin panel (approach A)
#   data/processed/krs_rcc_2015_panel.csv             RCC bank x month panel, 2015 (approach B)
#   output/tables/tab_krs_comrat_deprat.tex           Table 1: COMRAT on DEPRAT (with size columns)
#   output/tables/tab_krs_dep_assets.tex              Table 2: commitment measures on deposits / assets
#   output/tables/tab_krs_post2018.tex                Table 2b: post-2018 sample
#   output/tables/tab_krs_rcc_2015.tex                Table 3: RCC 2015 approach
#   output/figures/fig_krs_scatter.pdf                bank-level COMRAT vs DEPRAT, both approaches
#   output/figures/fig_krs_deposit_mix_context.pdf    deposit mix vs size, all institution types, 2015
#   output/logs/13_krs_regression.log                 this script's console log
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(fwildclusterboot)
  library(ggplot2)
  library(ggrepel)
})
source("code/00_palette.R")     # LANCET, lancet_n()

# ---- parameters ---------------------------------------------------------------
REPORTE_CSV <- "data/raw/Reporte_SistemaFinanciero.csv"
XW_REPORTE  <- "data/processed/crosswalks/xwalk_reporte.csv"
XW_RCC      <- "data/processed/crosswalks/xwalk_rcc.csv"
B2322_CSV   <- "data/processed/b2322_unused_credit_panel.csv"
B2358_CSV   <- "data/processed/b2358_branch_panel.csv.gz"
RCC_PQ      <- "data/raw/muestra_anonima_base2015_v2.parquet"
PROC <- "data/processed"
TABS <- "output/tables"
FIGS <- "output/figures"
LOGF <- "output/logs/13_krs_regression.log"

START_YM     <- 201007L   # first month in which B-2322 indirect credit includes unused lines (Res. SBS 11356-2008)
SPLIT_YM     <- 201805L   # last month before the 2018-06 jump in reported unused lines
POST_START_YM <- 201806L  # first month of the post-2018 table (after the 2018-06 reporting jump)
RCC_YEAR     <- 2015L     # sampling year of the RCC firm sample
GUARANTEE_PRODUCTS <- c("avales_fianzas", "cartas_credito")   # guarantee / LC facilities

N_BOOT <- 9999L
SEED   <- 20261117L

for (d in c(PROC, TABS, FIGS, dirname(LOGF))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
if (!interactive()) { log_con <- file(LOGF, open = "wt"); sink(log_con, split = TRUE) }   # log only under Rscript: an interactive run that stops early would leave the sink open

hdr <- function(s) cat("\n", strrep("=", 78), "\n", s, "\n", strrep("=", 78), "\n", sep = "")
fmt <- function(x) format(x, big.mark = ",", trim = TRUE)
ym_of <- function(d) as.integer(format(d, "%Y%m"))
options(width = 160)

# ==============================================================================
# 1. Reporte: assets, loans, deposits by type, equity (all institutions)
# ==============================================================================
hdr("1. Reporte (Series Estadisticas)")
rep <- fread(REPORTE_CSV, encoding = "UTF-8", na.strings = c("", "NA", "-"))
names(rep)[1] <- sub("^﻿", "", names(rep)[1])
stopifnot(ncol(rep) == 15L, names(rep)[1:3] == c("Entidad", "Fecha", "Total Activo"))
setnames(rep, c("bank_raw", "fecha", "assets", "loans", "loans_mn", "loans_me", "dep_total", "dep_mn",
                "dep_me", "equity", "dep_vista", "dep_ahorro", "dep_plazo", "liq_mn", "liq_me"))
rep[, date := as.Date(fecha, "%d/%m/%Y")]
stopifnot(!anyNA(rep$date), all(as.integer(format(rep$date + 1, "%d")) == 1L))   # month-ends
rep[, ym := ym_of(date)]
num_cols <- setdiff(names(rep), c("bank_raw", "fecha", "date", "ym"))
rep[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

xw_rep <- fread(XW_REPORTE, encoding = "UTF-8")
miss <- setdiff(unique(rep$bank_raw), xw_rep$bank_raw)
if (length(miss)) stop("Reporte names missing from xwalk_reporte: ", paste(miss, collapse = "; "))
rep <- merge(rep, xw_rep[, .(bank_raw, bank_standardized, entity_type)], by = "bank_raw", all.x = TRUE)
dup <- rep[, .N, by = .(bank_standardized, ym)][N > 1]
if (nrow(dup)) stop("Reporte not unique by bank_standardized x month after the crosswalk (", nrow(dup), " cells)")
cat(sprintf("Reporte: %s rows, %d raw names -> %d institutions, %s to %s.\n", fmt(nrow(rep)),
            uniqueN(rep$bank_raw), uniqueN(rep$bank_standardized), format(min(rep$date)), format(max(rep$date))))
print(rep[, .(institutions = uniqueN(bank_standardized), rows = .N,
              rows_with_vista = sum(!is.na(dep_vista))), by = entity_type])

# Deposit identity: vista + ahorro + plazo = total (vista blank for institutions without demand deposits)
chk <- rep[!is.na(dep_total), abs(fcoalesce(dep_vista, 0) + dep_ahorro + dep_plazo - dep_total) > 2]
cat(sprintf("Deposit identity vista + ahorro + plazo = total fails in %d of %s rows.\n", sum(chk), fmt(length(chk))))
REPORTE_END <- max(rep$date)

# ==============================================================================
# 2. Bulletin panel (approach A): B-2322 indirect credit + Reporte
# ==============================================================================
hdr("2. Bulletin panel (B-2322 + Reporte)")
b22 <- fread(B2322_CSV)[row_kind == "bank"]
b22[, ym := ym_of(as.Date(report_date))]
stopifnot(!anyDuplicated(b22[, .(bank_standardized, ym)]))
b22 <- b22[ym >= START_YM, .(bank_standardized, ym, unused = total_indirect_thousand_pen,
                             total_suspect = as.logical(total_suspect))]

rep_b <- rep[entity_type == "banca_multiple" & ym >= START_YM]
win_b22 <- b22[ym <= ym_of(REPORTE_END)]
cat(sprintf("B-2322 bank-months %s to %s: %s; matched to a Reporte bank-month: %s (%.1f%%).\n",
            START_YM, ym_of(REPORTE_END), fmt(nrow(win_b22)),
            fmt(nrow(merge(win_b22, rep_b[, .(bank_standardized, ym)]))),
            100 * nrow(merge(win_b22, rep_b[, .(bank_standardized, ym)])) / nrow(win_b22)))
um <- win_b22[!rep_b, on = .(bank_standardized, ym)][, .N, by = bank_standardized]
if (nrow(um)) { cat("  B-2322 bank-months without a Reporte row:\n"); print(um) }
um2 <- rep_b[!win_b22, on = .(bank_standardized, ym)][, .N, by = bank_standardized]
if (nrow(um2)) { cat("  Reporte bank-months without a B-2322 row:\n"); print(um2) }

pan <- merge(rep_b, b22, by = c("bank_standardized", "ym"))
n0 <- nrow(pan)
drop_log <- function(cond, why) {
  k <- pan[, which(cond)]
  cat(sprintf("  drop %-62s %5d\n", why, length(k)))
  if (length(k)) pan <<- pan[-k]
}
cat(sprintf("Merged bank-months: %s\n", fmt(n0)))
drop_log(pan$total_suspect %in% TRUE, "B-2322 printed total suspect (ICBC placeholder totals)")
drop_log(is.na(pan$unused), "total indirect credit not reported (NA, not zero)")
drop_log(!(pan$loans > 0) | is.na(pan$loans), "loans missing or zero")
drop_log(!(pan$dep_total > 0) | is.na(pan$dep_total) | is.na(pan$dep_vista), "deposits missing or zero")
drop_log(!(pan$assets > 0) | is.na(pan$assets), "assets missing or zero")

make_vars <- function(dt) {
  dt[, `:=`(
    comrat              = unused / (unused + loans),
    commit_to_assets    = (unused + loans) / assets,
    unused_to_assets    = unused / assets,
    deprat_vista        = dep_vista / dep_total,
    deprat_vista_ahorro = (dep_vista + dep_ahorro) / dep_total,
    dep_to_assets       = dep_total / assets,
    log_assets          = log(assets),
    equity_to_assets    = equity / assets)]
}
make_vars(pan)
ratio_cols <- c("comrat", "deprat_vista", "deprat_vista_ahorro", "dep_to_assets", "equity_to_assets")
for (v in ratio_cols) {
  out <- pan[!(get(v) >= 0 & get(v) <= 1), .N]
  cat(sprintf("  %-20s outside [0, 1]: %d\n", v, out))
}
cat(sprintf("  commit_to_assets range: [%.3f, %.3f]; unused_to_assets range: [%.3f, %.3f] (can exceed 1: indirect credit is off balance sheet)\n",
            min(pan$commit_to_assets), max(pan$commit_to_assets), min(pan$unused_to_assets), max(pan$unused_to_assets)))
cat(sprintf("Estimation panel: %s bank-months, %d banks, %d months (%d to %d).\n", fmt(nrow(pan)),
            uniqueN(pan$bank_standardized), uniqueN(pan$ym), min(pan$ym), max(pan$ym)))
pan[, mdate := ym]   # month FE as an integer id (boottest cannot handle Date-class FEs)
BANK_LEVELS <- sort(unique(c(rep_b$bank_standardized)))
pan[, `:=`(bank = bank_standardized, bank_id = match(bank_standardized, BANK_LEVELS))]
setorder(pan, bank, ym)

cat("\nCoverage and means by bank:\n")
print(pan[, .(months = .N, first = min(ym), last = max(ym), comrat = round(mean(comrat), 3),
              deprat_vista = round(mean(deprat_vista), 3), deprat_va = round(mean(deprat_vista_ahorro), 3),
              assets_bn = round(mean(assets) / 1e6, 1)), by = bank][order(-assets_bn)])

fwrite(pan[, .(bank_standardized, ym, date, unused, loans, assets, equity, dep_total, dep_vista, dep_ahorro,
               dep_plazo, comrat, commit_to_assets, unused_to_assets, deprat_vista, deprat_vista_ahorro,
               dep_to_assets, log_assets, equity_to_assets)],
       file.path(PROC, "krs_bank_panel.csv"))

# ---- cross-check Reporte deposits against B-2358 branch sums -------------------
b58 <- fread(B2358_CSV, select = c("report_date", "bank_standardized", "dep_vista_total",
                                    "dep_ahorro_total", "dep_plazo_total", "dep_total"))
b58 <- b58[, lapply(.SD, sum, na.rm = TRUE), by = .(bank_standardized, ym = ym_of(as.Date(report_date))),
           .SDcols = c("dep_vista_total", "dep_ahorro_total", "dep_plazo_total", "dep_total")]
cx <- merge(pan[, .(bank_standardized, ym, dep_vista, dep_ahorro, dep_plazo, dep_total)], b58,
            by = c("bank_standardized", "ym"), suffixes = c("", "_b58"))
rel <- function(a, b) abs(a - b) / pmax(abs(a), 1)
cat(sprintf("\nReporte vs B-2358 deposits (%s matched bank-months): share within 0.5%% -- vista %.3f | ahorro %.3f | plazo %.3f | total %.3f\n",
            fmt(nrow(cx)), cx[, mean(rel(dep_vista, dep_vista_total) < 0.005)], cx[, mean(rel(dep_ahorro, dep_ahorro_total) < 0.005)],
            cx[, mean(rel(dep_plazo, dep_plazo_total) < 0.005)], cx[, mean(rel(dep_total, dep_total_b58) < 0.005)]))
cx[, dev_vista := rel(dep_vista, dep_vista_total)]
if (cx[dev_vista >= 0.005, .N]) {
  cat("  Banks with vista deviations >= 0.5% (count of months, median deviation):\n")
  print(cx[dev_vista >= 0.005, .(months = .N, median_dev = round(median(dev_vista), 4)), by = bank_standardized])
}

# ---- descriptive statistics (between / within) --------------------------------
panel_stats <- function(dt, var) {
  x <- dt[!is.na(get(var))]
  data.table(Variable = var, Mean = mean(x[[var]]), SD = sd(x[[var]]),
             SD_between = sd(x[, mean(get(var)), by = bank]$V1),
             SD_within  = sd(x[, get(var) - mean(get(var)), by = bank]$V1),
             p25 = quantile(x[[var]], 0.25), Median = median(x[[var]]), p75 = quantile(x[[var]], 0.75),
             N = nrow(x), N_banks = uniqueN(x$bank))
}
desc <- rbindlist(lapply(c("comrat", "commit_to_assets", "unused_to_assets", "deprat_vista",
                           "deprat_vista_ahorro", "dep_to_assets", "log_assets", "equity_to_assets"),
                         panel_stats, dt = pan))
cat("\nDescriptive statistics (bulletin panel):\n")
print(desc, digits = 3)

# ==============================================================================
# 3. Regression helper (clustered SEs + wild cluster bootstrap)
# ==============================================================================
# boottest() re-evaluates the model's `data` argument in the global environment, so each
# fit assigns its estimation sample to `fit_dat` there (same approach as script 11).
fit_krs <- function(fml, dat, param, cluster = TRUE, boot_fe = NULL) {
  vars <- all.vars(fml)
  fd <- dat[complete.cases(dat[, ..vars])]
  assign("fit_dat", fd, envir = globalenv())
  if (cluster) {
    m <- feols(fml, data = fit_dat, cluster = ~bank_id, notes = FALSE)
    G <- fitstat(m, "g")[[1]]
    # boottest() cannot parse two-way FE formulas; bootstrap an equivalent model with the
    # month FE entered as dummies (identical coefficient on `param`).
    mb <- m
    f_txt <- paste(deparse(fml), collapse = "")
    if (grepl("\\| *mdate *\\+ *bank_id", f_txt)) {
      mb <- feols(as.formula(sub("\\| *mdate *\\+ *bank_id", "+ factor(mdate) | bank_id", f_txt)),
                  data = fit_dat, cluster = ~bank_id, notes = FALSE)
      stopifnot(abs(coef(mb)[[param]] - coef(m)[[param]]) < 1e-8)
      boot_fe <- "bank_id"
    }
    set.seed(SEED); dqrng::dqset.seed(SEED)
    bt <- boottest(mb, param = param, clustid = "bank_id", fe = boot_fe, B = N_BOOT,
                   type = if (G <= 12L) "webb" else "rademacher")
    p_boot <- bt$p_val
  } else {
    m <- feols(fml, data = fit_dat, vcov = "hetero", notes = FALSE)
    G <- uniqueN(fd$bank); p_boot <- NA_real_
  }
  attr(m, "krs") <- data.table(param = param, coef = coef(m)[[param]], se = se(m)[[param]],
                               p = pvalue(m)[[param]], p_boot = p_boot, n = nobs(m), banks = G,
                               mean_y = mean(fd[[vars[1]]]))
  m
}
stats_of <- function(ms) rbindlist(lapply(ms, attr, "krs"))
pf <- function(p) ifelse(is.na(p), "", formatC(p, format = "f", digits = 3))
extra_lines <- function(st, boot = TRUE) {
  l <- list("^Banks" = formatC(st$banks, format = "d"),
            "^Mean of dep. variable" = formatC(st$mean_y, format = "f", digits = 3))
  if (boot) l[["^Wild cluster bootstrap $p$-value"]] <- pf(st$p_boot)
  l
}
dict <- c(deprat = "DEPRAT", deprat_vista = "DEPRAT (vista)", deprat_vista_ahorro = "DEPRAT (vista + ahorro)",
          dep_to_assets = "Deposits / assets", log_assets = "Log assets", equity_to_assets = "Equity / assets",
          comrat = "COMRAT", commit_to_assets = "(U + L) / assets", unused_to_assets = "U / assets",
          comrat_rcc = "COMRAT (RCC)", comrat_rcc_ex_guar = "COMRAT (RCC, ex guarantees)",
          mdate = "Month", bank_id = "Bank", "(Intercept)" = "Constant")
src_note <- paste0(
  "$U$ is total indirect credit (SBS cuadro B-2322): guarantees, letters of credit, bank acceptances, and unused credit lines and credit granted but not disbursed; ",
  "$L$ is direct credit; deposits by type, total assets and equity are from SBS Series Estad\\'isticas. ",
  "COMRAT $= U/(U+L)$. DEPRAT (vista) is demand deposits over total deposits; DEPRAT (vista + ahorro) adds savings deposits. ")
se_note <- paste0(
  "Standard errors clustered by bank in parentheses; with few clusters the $p$-value of a wild cluster bootstrap ",
  "(null imposed, Rademacher weights, ", fmt(N_BOOT), " draws, or full enumeration when fewer distinct draws exist) on the first regressor shown is also reported. ",
  "* $p<0.10$, ** $p<0.05$, *** $p<0.01$ from the conventional clustered test.")
# etable() in fixest 0.11.2 evaluates several arguments non-standardly and fails when they
# are passed through a wrapper; build the call with every argument inlined as a value.
write_tab <- function(mods, file, title, label, notes, headers, extra, cmid = NULL, depvar = FALSE, order = NULL) {
  args <- list(tex = TRUE, file = file, replace = TRUE, dict = dict, tpt = TRUE, notes = notes,
               title = title, label = label, fitstat = ~ n + r2 + wr2, extralines = extra,
               style.tex = style.tex("aer"), signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
               digits = "r3", digits.stats = 3, fontsize = "small", placement = "!h", depvar = depvar)
  if (!is.null(headers)) args$headers <- headers
  if (!is.null(order)) args$order <- order
  if (!is.null(cmid)) {
    pat <- cmid$pattern; rule <- cmid$rule
    args$postprocess.tex <- function(x) sub(pat, paste0("\\1", rule, "\n"), x)
  }
  e <- new.env(parent = globalenv()); assign(".mods", unname(mods), envir = e)
  eval(as.call(c(list(quote(fixest::etable), quote(.mods)), args)), envir = e)
  cat("Wrote ", file, "\n", sep = "")
}
sample_txt <- sprintf("Banca M\\'ultiple, monthly, %s to %s (%d banks, %s bank-months).",
                      format(min(pan$date), "%B %Y"), format(max(pan$date), "%B %Y"),
                      uniqueN(pan$bank), fmt(nrow(pan)))

# ==============================================================================
# 4. Table 1: COMRAT on DEPRAT (with size columns)
# ==============================================================================
hdr("4. Table 1: COMRAT on DEPRAT")
t1 <- list()
for (dv in c("deprat_vista", "deprat_vista_ahorro")) {
  d <- copy(pan)[, deprat := get(dv)]
  t1[[paste(dv, 1)]] <- fit_krs(comrat ~ deprat, d, "deprat")
  t1[[paste(dv, 3)]] <- fit_krs(comrat ~ deprat + log_assets + equity_to_assets | mdate, d, "deprat", boot_fe = "mdate")
  t1[[paste(dv, 4)]] <- fit_krs(comrat ~ deprat | mdate + bank_id, d, "deprat", boot_fe = "bank_id")
}
# Size columns: log assets alone (month FE), and DEPRAT (vista) + log assets (month + bank FE)
d <- copy(pan)[, deprat := deprat_vista]
t1[["size 1"]] <- fit_krs(comrat ~ log_assets | mdate, d, "log_assets", boot_fe = "mdate")
t1[["size 2"]] <- fit_krs(comrat ~ deprat + log_assets | mdate + bank_id, d, "deprat", boot_fe = "bank_id")
# Column order: size alone; demand-deposit DEPRAT (pooled, controls, bank FE, bank FE + size);
# demand + savings DEPRAT (pooled, controls, bank FE)
t1 <- t1[c("size 1", "deprat_vista 1", "deprat_vista 3", "deprat_vista 4", "size 2",
           "deprat_vista_ahorro 1", "deprat_vista_ahorro 3", "deprat_vista_ahorro 4")]
s1 <- stats_of(t1); s1[, model := names(t1)]
print(s1[, .(model, coef = signif(coef, 4), se = signif(se, 3), p = round(p, 3), p_boot = round(p_boot, 3), n, banks)])

cat("\nSubsample before the 2018-06 jump in reported unused lines (DEPRAT vista):\n")
d_pre <- copy(pan[ym <= SPLIT_YM])[, deprat := deprat_vista]
for (f in list(comrat ~ deprat | mdate, comrat ~ deprat | mdate + bank_id)) {
  m <- feols(f, data = d_pre, cluster = ~bank_id, notes = FALSE)
  cat(sprintf("  %-32s coef %8.4f  se %7.4f  p %.3f  N %s  banks %d\n", deparse(f), coef(m)[["deprat"]],
              se(m)[["deprat"]], pvalue(m)[["deprat"]], fmt(nobs(m)), fitstat(m, "g")[[1]]))
}

write_tab(t1, file.path(TABS, "tab_krs_comrat_deprat.tex"),
  title = "Loan commitments and deposit funding: Kashyap, Rajan and Stein (2002) regressions",
  label = "tab:krs_comrat_deprat",
  notes = paste0("\\textit{Notes:} ", sample_txt, " The dependent variable is COMRAT. ", src_note,
                 "Column (1) regresses COMRAT on log assets alone. Columns (2)--(5) measure DEPRAT with demand deposits, columns (6)--(8) with demand plus savings deposits; ",
                 "column (5) adds log assets to the specification with month and bank fixed effects. ",
                 "The bootstrap $p$-value refers to log assets in column (1) and to DEPRAT elsewhere. ",
                 "Reported unused lines jump for some banks in June 2018 and March 2019 (reporting changes); month fixed effects absorb the common part. ",
                 se_note),
  headers = list("^DEPRAT definition" = list("None" = 1, "Vista / deposits" = 4, "(Vista + ahorro) / deposits" = 3)),
  extra = extra_lines(s1),
  cmid = list(pattern = "(DEPRAT definition[^\n]*\\\\ *\n)", rule = "\\\\cmidrule(lr){3-6} \\\\cmidrule(lr){7-9}"),
  order = c("^DEPRAT$", "^Log assets$", "^Equity", "^Constant$"),
  depvar = FALSE)

# ==============================================================================
# 5. Table 2: commitment measures on deposits / assets
# ==============================================================================
hdr("5. Table 2: deposits / assets")
t2 <- list()
for (y in c("comrat", "unused_to_assets")) {
  t2[[paste(y, 1)]] <- fit_krs(as.formula(paste(y, "~ dep_to_assets | mdate")), pan, "dep_to_assets", boot_fe = "mdate")
  t2[[paste(y, 2)]] <- fit_krs(as.formula(paste(y, "~ dep_to_assets | mdate + bank_id")), pan, "dep_to_assets", boot_fe = "bank_id")
}
s2 <- stats_of(t2); s2[, model := names(t2)]
print(s2[, .(model, coef = signif(coef, 4), se = signif(se, 3), p = round(p, 3), p_boot = round(p_boot, 3), n, banks)])
write_tab(t2, file.path(TABS, "tab_krs_dep_assets.tex"),
  title = "Loan commitments and the deposit share of assets",
  label = "tab:krs_dep_assets",
  notes = paste0("\\textit{Notes:} ", sample_txt, " ", src_note,
                 "$U$/assets can exceed one because indirect credit is off balance sheet. ", se_note),
  headers = list("^Dependent variable" = list("COMRAT" = 2, "$U$ / assets" = 2)),
  extra = extra_lines(s2),
  cmid = list(pattern = "(Dependent variable[^\n]*\\\\ *\n)", rule = "\\\\cmidrule(lr){2-3} \\\\cmidrule(lr){4-5}"),
  depvar = FALSE)

# ==============================================================================
# 5b. Table 2b: post-2018 sample
# ==============================================================================
hdr("5b. Table 2b: post-2018 sample")
post <- pan[ym >= POST_START_YM]
t2b <- list(
  fit_krs(comrat ~ deprat + log_assets | mdate + bank_id, copy(post)[, deprat := deprat_vista],        "deprat", boot_fe = "bank_id"),
  fit_krs(comrat ~ deprat + log_assets | mdate + bank_id, copy(post)[, deprat := deprat_vista_ahorro], "deprat", boot_fe = "bank_id"),
  fit_krs(comrat ~ dep_to_assets | mdate,                 post, "dep_to_assets", boot_fe = "mdate"),
  fit_krs(comrat ~ dep_to_assets | mdate + bank_id,       post, "dep_to_assets", boot_fe = "bank_id"),
  fit_krs(unused_to_assets ~ dep_to_assets | mdate,           post, "dep_to_assets", boot_fe = "mdate"),
  fit_krs(unused_to_assets ~ dep_to_assets | mdate + bank_id, post, "dep_to_assets", boot_fe = "bank_id"))
s2b <- stats_of(t2b)
print(s2b[, .(param, coef = signif(coef, 4), se = signif(se, 3), p = round(p, 3), p_boot = round(p_boot, 3), n, banks)])
post_txt <- sprintf("Banca M\\'ultiple, monthly, %s to %s (%d banks, %s bank-months).",
                    format(min(post$date), "%B %Y"), format(max(post$date), "%B %Y"),
                    uniqueN(post$bank), fmt(nrow(post)))
write_tab(t2b, file.path(TABS, "tab_krs_post2018.tex"),
  title = "Loan commitments and deposit funding after the 2018 reporting change",
  label = "tab:krs_post2018",
  notes = paste0("\\textit{Notes:} ", post_txt, " The sample starts after the June 2018 jump in reported unused lines. ", src_note,
                 "Column (1) measures DEPRAT with demand deposits and column (2) with demand plus savings deposits. ",
                 "$U$/assets can exceed one because indirect credit is off balance sheet. ", se_note),
  headers = list("^Dependent variable" = list("COMRAT" = 4, "$U$ / assets" = 2),
                 "^Funding measure"    = list("Vista" = 1, "Vista + ahorro" = 1, "Deposits / assets" = 4)),
  extra = extra_lines(s2b),
  cmid = list(pattern = "(Dependent variable[^\n]*\\\\ *\n)", rule = "\\\\cmidrule(lr){2-5} \\\\cmidrule(lr){6-7}"),
  order = c("^DEPRAT$", "^Deposits", "^Log assets$", "^Constant$"))

# ==============================================================================
# 6. Approach B: RCC firm sample, 2015
# ==============================================================================
hdr("6. RCC firm sample, 2015")
cr <- as.data.table(read_parquet(RCC_PQ, col_select = c("fecha_reporte", "identificador", "entidad", "producto",
                                                         "credito_utilizado", "no_utilizado_total")))
cat(sprintf("RCC sample: %s rows, %s firms, %s to %s.\n", fmt(nrow(cr)), fmt(uniqueN(cr$identificador)),
            format(min(cr$fecha_reporte)), format(max(cr$fecha_reporte))))
cat("NOTE: data/documentation/dictionary_muestra_anonima_base2015.xlsx documents v1 (1,000 firms); this v2 file is larger.\n")
cr <- cr[as.integer(format(fecha_reporte, "%Y")) == RCC_YEAR]
cr[, producto := as.character(producto)]
stopifnot(!anyNA(cr[, .(credito_utilizado, no_utilizado_total)]))
cat(sprintf("%d: %s rows, %s firms, %d entidad codes, %d months.\n", RCC_YEAR, fmt(nrow(cr)),
            fmt(uniqueN(cr$identificador)), uniqueN(cr$entidad), uniqueN(cr$fecha_reporte)))

xw_rcc <- fread(XW_RCC, encoding = "UTF-8")
cr <- merge(cr, xw_rcc[, .(entidad, bank_standardized, entity_type)], by = "entidad", all.x = TRUE)
ent <- cr[, .(firms = uniqueN(identificador), used_mn = sum(credito_utilizado) / 12 / 1e6),
          by = .(entidad, bank_standardized, entity_type)]
cat(sprintf("Share of %d sampled drawn credit at Banca Multiple codes: %.3f; excluded entities with the most credit:\n",
            RCC_YEAR, ent[entity_type %in% "banca_multiple", sum(used_mn)] / ent[, sum(used_mn)]))
print(head(ent[!entity_type %in% "banca_multiple"][order(-used_mn)], 8))
cr <- cr[entity_type %in% "banca_multiple"]

# Amounts are in soles; the Reporte is in thousands of soles.
rcc <- cr[, .(firms   = uniqueN(identificador),
              U       = sum(no_utilizado_total) / 1e3,
              U_exg   = sum(no_utilizado_total[!producto %in% GUARANTEE_PRODUCTS]) / 1e3,
              L       = sum(credito_utilizado) / 1e3),
          by = .(bank_standardized, ym = ym_of(fecha_reporte))]
rcc <- rcc[L > 0]
rcc <- merge(rcc, rep[entity_type == "banca_multiple",
                      .(bank_standardized, ym, assets, loans, equity, dep_total, dep_vista, dep_ahorro)],
             by = c("bank_standardized", "ym"), all.x = TRUE)
cat(sprintf("RCC bank-months: %d (%d banks); without a Reporte Banca Multiple match: %d (%s).\n", nrow(rcc),
            uniqueN(rcc$bank_standardized), rcc[is.na(assets), .N],
            paste(rcc[is.na(assets), unique(bank_standardized)], collapse = ", ")))
rcc <- rcc[!is.na(assets) & dep_total > 0]
rcc[, `:=`(comrat_rcc = U / (U + L), comrat_rcc_ex_guar = U_exg / (U_exg + L),
           deprat_vista = dep_vista / dep_total, deprat_vista_ahorro = (dep_vista + dep_ahorro) / dep_total,
           log_assets = log(assets), equity_to_assets = equity / assets, sample_share_L = L / loans)]
rcc[, `:=`(bank = bank_standardized, bank_id = match(bank_standardized, BANK_LEVELS))]
rcc[, mdate := ym]

# Bulletin COMRAT for the same bank-months (B-2322 U, Reporte L; before the estimation-sample drops)
bul15 <- merge(b22[ym %/% 100L == RCC_YEAR], rep_b[, .(bank_standardized, ym, loans)], by = c("bank_standardized", "ym"))
bul15[, comrat_bul := fifelse(total_suspect %in% TRUE, NA_real_, unused / (unused + loans))]
rcc <- merge(rcc, bul15[, .(bank_standardized, ym, U_bul = unused, comrat_bul)], by = c("bank_standardized", "ym"), all.x = TRUE)
fwrite(rcc[, .(bank_standardized, ym, firms, U, U_exg, L, loans, assets, dep_total, dep_vista, dep_ahorro, equity,
               comrat_rcc, comrat_rcc_ex_guar, comrat_bul, deprat_vista, deprat_vista_ahorro, log_assets,
               equity_to_assets, sample_share_L)],
       file.path(PROC, "krs_rcc_2015_panel.csv"))

cs <- rcc[, .(firms = max(firms), comrat_rcc = mean(comrat_rcc), comrat_rcc_ex_guar = mean(comrat_rcc_ex_guar),
              comrat_bul = mean(comrat_bul, na.rm = TRUE), deprat_vista = mean(deprat_vista),
              deprat_vista_ahorro = mean(deprat_vista_ahorro), log_assets = mean(log_assets),
              assets = mean(assets), equity_to_assets = mean(equity_to_assets),
              share_L = mean(sample_share_L), share_U = mean(U / U_bul, na.rm = TRUE), months = .N), by = bank]
cs[is.nan(comrat_bul), comrat_bul := NA_real_]
cs[is.nan(share_U) | is.infinite(share_U), share_U := NA_real_]
setorder(cs, -assets)
cat("\n2015 bank averages. share_L = sampled drawn credit / Reporte loans; share_U = sampled unused lines (7205) / B-2322 total indirect credit.\n")
cat("Banks reporting zero or near-zero unused lines at firm level are NOT corrected (by design).\n")
print(cs[, .(bank, firms, months, comrat_rcc = round(comrat_rcc, 3), comrat_rcc_exg = round(comrat_rcc_ex_guar, 3),
             comrat_bul = round(comrat_bul, 3), share_L = round(share_L, 3), share_U = round(share_U, 3),
             deprat_vista = round(deprat_vista, 3))])
cat(sprintf("Correlation of RCC and bulletin COMRAT across banks: %.3f (N = %d)\n",
            cs[!is.na(comrat_bul), cor(comrat_rcc, comrat_bul)], cs[!is.na(comrat_bul), .N]))

t4 <- list(
  fit_krs(comrat_rcc ~ deprat_vista, cs, "deprat_vista", cluster = FALSE),
  fit_krs(comrat_rcc ~ deprat_vista + log_assets, cs, "deprat_vista", cluster = FALSE),
  fit_krs(comrat_rcc ~ deprat_vista | mdate, rcc, "deprat_vista", boot_fe = "mdate"),
  fit_krs(comrat_rcc ~ deprat, copy(cs)[, deprat := deprat_vista_ahorro], "deprat", cluster = FALSE),
  fit_krs(comrat_rcc_ex_guar ~ deprat_vista, cs, "deprat_vista", cluster = FALSE))
s4 <- stats_of(t4)
print(s4[, .(param, coef = signif(coef, 4), se = signif(se, 3), p = round(p, 3), p_boot = round(p_boot, 3), n, banks)])
dict[["deprat"]] <- "DEPRAT (vista + ahorro)"
zero_banks <- cs[comrat_rcc < 0.01, bank]
write_tab(t4, file.path(TABS, "tab_krs_rcc_2015.tex"),
  title = "Loan commitments and deposit funding: credit-registry sample, 2015",
  label = "tab:krs_rcc_2015",
  notes = paste0("\\textit{Notes:} Banca M\\'ultiple, ", RCC_YEAR, ". $U$ and $L$ are summed over the firms in the ",
                 "credit-registry (RCC) sample drawn in ", RCC_YEAR, " (", fmt(uniqueN(cr$identificador)), " firms borrowing from ",
                 nrow(cs), " banks), so COMRAT $= U/(U+L)$ refers to the bank's lending to firms only. ",
                 "$U$ is account 7205 (unused lines and credit granted but not disbursed); unlike the bulletin measure it excludes outstanding guarantees and letters of credit. Column (5) also excludes unused guarantee and letter-of-credit facilities. ",
                 "DEPRAT, assets and equity are from SBS Series Estad\\'isticas. ",
                 "Columns (1), (2), (4) and (5) are cross-sections of ", RCC_YEAR, " bank averages with heteroskedasticity-robust (HC1) standard errors; ",
                 "column (3) pools bank-months with month fixed effects, standard errors clustered by bank and a wild cluster bootstrap $p$-value. ",
                 "Several banks report few or no unused lines for sampled firms (COMRAT below 1\\%: ",
                 gsub("_", "\\\\_", paste(zero_banks, collapse = ", ")),
                 "); no correction is made. * $p<0.10$, ** $p<0.05$, *** $p<0.01$."),
  headers = list("^Dependent variable" = list("COMRAT (RCC)" = 4, "Ex guar." = 1),
                 "^Sample" = list("Bank averages" = 2, "Bank-months" = 1, "Bank averages" = 2)),
  extra = extra_lines(s4),
  depvar = FALSE)

# ==============================================================================
# 7. Figures
# ==============================================================================
hdr("7. Figures")
base_theme <- theme_bw(base_size = 10, base_family = "serif") +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(colour = "grey90", linewidth = 0.3),
    panel.border       = element_rect(colour = "grey30", linewidth = 0.4),
    axis.title         = element_text(size = 8),
    axis.text          = element_text(size = 7),
    strip.background   = element_rect(fill = "grey92", colour = "grey30", linewidth = 0.4),
    strip.text         = element_text(size = 8, face = "bold"),
    legend.position    = "bottom",
    legend.title       = element_text(size = 7),
    legend.text        = element_text(size = 7),
    legend.margin      = margin(t = -4),
    plot.caption       = element_text(size = 7, hjust = 0, colour = "grey30"))
wrap_cap <- function(...) paste(strwrap(paste0(...), width = 150), collapse = "\n")
save_fig <- function(name, plot, height) {
  f <- file.path(FIGS, name)
  ggsave(f, plot, width = 7.2, height = height, device = grDevices::pdf, encoding = "ISOLatin1")
  stopifnot(file.exists(f), file.size(f) > 1000)
  cat(sprintf("Wrote %s (%s bytes)\n", f, fmt(file.size(f))))
}
LABELS <- c(bcp = "BCP", bbva_peru = "BBVA", interbank = "Interbank", scotiabank = "Scotiabank", bif = "BanBif",
            citibank = "Citibank", mibanco = "Mibanco", bancom = "Bancom", banco_pichincha = "Pichincha",
            banco_falabella = "Falabella", banco_ripley = "Ripley", banco_gnb = "GNB", alfin_banco = "Alfin",
            santander_peru = "Santander", banco_icbc = "ICBC", banco_cencosud = "Cencosud", banco_bci = "BCI",
            hsbc_peru = "HSBC", bank_of_china = "Bank of China", compartamos_banco = "Compartamos",
            santander_consumer = "Santander Consumer", deutsche_bank = "Deutsche")
lab_of <- function(x) fifelse(x %in% names(LABELS), LABELS[x], tools::toTitleCase(gsub("_", " ", x)))
BLUE <- LANCET[["navy"]]

# ---- Figure 1: bank-level scatter, both approaches -----------------------------
bm <- pan[, .(comrat = mean(comrat), deprat = mean(deprat_vista), assets = mean(assets)), by = bank]
ols_line <- function(d) { m <- feols(comrat ~ deprat, d, vcov = "hetero"); c(coef(m), se = se(m)[["deprat"]]) }
fa <- ols_line(bm)
fb <- ols_line(cs[, .(bank, comrat = comrat_rcc, deprat = deprat_vista, assets)])
pa <- sprintf("A. Bulletin (B-2322), %s-%s", format(min(pan$date), "%Y"), format(max(pan$date), "%Y"))
pb <- sprintf("B. Credit registry sample (firms only), %d", RCC_YEAR)
sc <- rbind(bm[, .(panel = pa, bank, comrat, deprat, assets)],
            cs[, .(panel = pb, bank, comrat = comrat_rcc, deprat = deprat_vista, assets)])
sc[, panel := factor(panel, levels = c(pa, pb))]
lines_dt <- data.table(panel = factor(c(pa, pb), levels = c(pa, pb)),
                       a = c(fa[[1]], fb[[1]]), b = c(fa[[2]], fb[[2]]))
p1 <- ggplot(sc, aes(deprat, comrat)) +
  geom_abline(data = lines_dt, aes(intercept = a, slope = b), colour = BLUE, linewidth = 0.5) +
  geom_point(aes(size = assets / 1e6), shape = 21, fill = BLUE, colour = "white", alpha = 0.85, stroke = 0.3) +
  geom_text_repel(aes(label = lab_of(bank)), family = "serif", size = 2.4, segment.colour = "grey60",
                  seed = 1, max.overlaps = Inf, min.segment.length = 0.2) +
  facet_wrap(~panel, nrow = 1) +
  scale_size(range = c(1.6, 7), name = "Average total assets (S/ bn)") +
  scale_x_continuous(labels = function(x) paste0(100 * x, "%")) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  labs(x = "DEPRAT: demand (vista) deposits / total deposits", y = "COMRAT: U / (U + L)",
       caption = wrap_cap(
         "Each point is a bank, averaged over the sample period (panel A: monthly bank averages; panel B: averages over ", RCC_YEAR, "). ",
         sprintf("Lines are unweighted OLS fits across banks: slope %.3f (robust SE %.3f) in panel A and %.3f (%.3f) in panel B. ",
                 fa[[2]], fa[["se"]], fb[[2]], fb[["se"]]),
         "Panel A: U is total indirect credit (guarantees, letters of credit, acceptances and unused lines) from SBS cuadro B-2322; loans and deposits from SBS Series Estadisticas. ",
         "Panel B: U is unused lines (account 7205) and L drawn credit, summed over the firms in the ", RCC_YEAR, " credit-registry sample; ",
         "several banks report almost no unused lines for these firms, which is not corrected.")) +
  base_theme
save_fig("fig_krs_scatter.pdf", p1, 4.6)

# ---- Figure 2: deposit mix vs size, all institution types, 2015 ------------------
ctx <- rep[ym %/% 100L == RCC_YEAR & dep_total > 0 & assets > 0,
           .(transact = mean((fcoalesce(dep_vista, 0) + dep_ahorro) / dep_total), assets = mean(assets)),
           by = .(bank_standardized, entity_type)]
TYPE_LAB <- c(banca_multiple = "Banks", empresa_financiera = "Financieras", caja_municipal = "Cajas municipales",
              caja_rural = "Cajas rurales", empresa_credito = "Edpymes / credit companies")
ctx[, type := factor(TYPE_LAB[entity_type], levels = TYPE_LAB)]
p3 <- ggplot(ctx, aes(assets / 1e6, transact, colour = type)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%"), limits = c(0, 1)) +
  scale_colour_manual(values = lancet_n(5), name = NULL) +
  labs(x = "Average total assets, S/ bn (log scale)", y = "(Vista + ahorro) / total deposits",
       caption = wrap_cap("Descriptive context only: unused credit lines are available for banks alone, so only banks enter the regressions. ",
                          "Each point is an institution averaged over ", RCC_YEAR, " (institutions without deposits are omitted). ",
                          "Non-bank institutions report no demand (vista) deposits. Source: SBS Series Estadisticas.")) +
  base_theme
save_fig("fig_krs_deposit_mix_context.pdf", p3, 4.2)

hdr("Session")
print(sessionInfo())
if (!interactive()) { sink(); close(log_con) }
