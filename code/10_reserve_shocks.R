# ==============================================================================
# 10_reserve_shocks.R -- Currency-exposure-weighted bank reserve-requirement
# (encaje) shocks, by bank_standardized
# ==============================================================================
# Run from the project root:   Rscript code/10_reserve_shocks.R
# Next step:                   code/11_loan_reserve_shock_regression.R
#
# DESIGN
#   Bank b's reserve-requirement exposure is its predetermined deposit
#   dollarization. For each statutory encaje parameter j,
#       bank_shock_{b,t,j} = s_{b,mn,0} * d_r_{mn,t,j} + s_{b,me,0} * d_r_{me,t,j}
#   where d_r is the month-t change (percentage points; + = tighter) of the rate
#   in force at month end, and s_{b,.,0} are the bank's deposit shares at
#   BASELINE_DATE (fixed, so they cannot respond to the shocks they weight).
#   Deposit shares are built for EVERY institution and month of the Reporte
#   (bank_deposit_shares.csv); the baseline is then picked from that panel.
#
#   Everything here is keyed by bank_standardized, the project's hub key: this
#   script never sees the credit data. Linking bank_standardized to the credit
#   file's entidad code (and the calendar of the credit data) is done in
#   code/11_loan_reserve_shock_regression.R.
#
# INPUTS (never modified)
#   data/processed/encaje_rates_monthly.csv         monthly BCRP reserve-requirement panel (09_encaje_series.R)
#   data/raw/Reporte(01-2001 - 09-2022).csv         deposits by institution and currency (S/ thousands)
#   data/processed/crosswalks/xwalk_reporte.csv     Reporte institution name -> bank_standardized (03_validate_direct.R)
#
# OUTPUTS
#   data/processed/bank_deposit_shares.csv           MN / ME deposits and shares, every institution x month, with validation flags
#   data/processed/bank_encaje_shocks.csv            bank shocks, unique by bank_standardized x fecha_reporte (month-end),
#                                                    every month of the rate panel, banks with valid baseline shares only.
#                                                    Months up to BASELINE_DATE are included for completeness but are not
#                                                    predetermined-share shocks; the regressions use the credit calendar only.
#   output/logs/10_reserve_shocks.log                this script's console log
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readr)
})

# Rscript inherits the shell locale (often "C"), which mangles accented names.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (loc in c("en_US.UTF-8", "C.UTF-8", "es_PE.UTF-8")) {
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
  }
}

# ---- parameters ---------------------------------------------------------------
ENCAJE_CSV  <- "data/processed/encaje_rates_monthly.csv"
REPORTE_CSV <- "data/raw/Reporte(01-2001 - 09-2022).csv"
XWALK_REP   <- "data/processed/crosswalks/xwalk_reporte.csv"
PROC  <- "data/processed"
LOGF  <- "output/logs/10_reserve_shocks.log"

# Baseline for the fixed deposit shares. December 2009 is the last month-end
# before (i) the first credit month (2010-09-30) and (ii) the 2010 tightening
# cycle (MN minimum legal rate 6 -> 9% between Jul and Oct 2010), so shares are
# predetermined with respect to every growth interval and every encaje change
# used in the regressions.
BASELINE_DATE <- as.Date("2009-12-31")

# Statutory encaje parameters with a numeric rate. gen_regime is categorical
# (single rate vs base + marginal): it has no percentage-point change, so it
# enters only as a diagnostic flag and through the NA pattern of gen_marginal.
MEASURES <- c("gen_min_legal", "gen_marginal")

TOL <- 1e-9

for (d in c(PROC, dirname(LOGF))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
log_con <- file(LOGF, open = "wt"); sink(log_con, split = TRUE)

hdr <- function(s) cat("\n", strrep("=", 78), "\n", s, "\n", strrep("=", 78), "\n", sep = "")
fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

# ==============================================================================
# 1. Deposit currency shares, every institution and month (Reporte, S/ thousands)
# ==============================================================================
hdr("1. Deposit currency shares (all months)")
raw_rep <- read_csv(REPORTE_CSV, locale = locale(encoding = "UTF-32LE"),
                    col_types = cols(.default = col_character()), name_repair = "minimal")
stopifnot(ncol(raw_rep) >= 8)
# Units: both deposit columns are headed "(Miles de S/)", i.e. thousands of soles.
# ME deposits are therefore already in soles -- no exchange-rate conversion, and MN + ME
# can be added directly. (Same finding as the header note in code/peru_dollarization.R.)
dep_hdr <- names(raw_rep)[6:8]
stopifnot(all(grepl("Miles de S/", dep_hdr, fixed = TRUE)))
cat("Deposit column headers: ", paste(dep_hdr, collapse = " | "), "\n", sep = "")
names(raw_rep)[1:8] <- c("entity", "fecha", "cred_total", "cred_mn", "cred_me", "dep_total", "dep_mn", "dep_me")
as_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(x))))
rep <- as.data.table(raw_rep)[, .(entity = { e <- trimws(entity); Encoding(e) <- "UTF-8"; e },
                                  date = as.Date(trimws(fecha), format = "%d/%m/%Y"),
                                  dep_total = as_num(dep_total), dep_mn = as_num(dep_mn), dep_me = as_num(dep_me))]
stopifnot(!anyNA(rep$date), !anyDuplicated(rep[, .(entity, date)]))

# Reporte name -> bank_standardized, from the project's Reporte crosswalk (the hub key)
xw <- fread(XWALK_REP, encoding = "UTF-8")
stopifnot(identical(names(xw), c("bank_raw", "bank_standardized", "review_required")),
          !anyDuplicated(xw$bank_raw), !anyNA(xw$bank_standardized))
miss <- setdiff(unique(rep$entity), xw$bank_raw)
if (length(miss)) stop("Reporte names missing from ", XWALK_REP, ": ", paste(miss, collapse = "; "),
                       " -- rerun code/03_validate_direct.R")
n_before <- nrow(rep)
rep <- merge(rep, xw[, .(entity = bank_raw, bank_standardized)], by = "entity")
stopifnot(nrow(rep) == n_before)                  # many-to-one: no Reporte row multiplied or lost
rep <- rep[!bank_standardized %in% c("__SYSTEM__", "__SYSTEM_FINANCIERAS__")]
# two Reporte names for one hub bank must never coexist in a month (would double count)
stopifnot(!anyDuplicated(rep[, .(bank_standardized, date)]))
cat(sprintf("Reporte: %d institution-months, %d institutions, %s to %s.\n", nrow(rep),
            uniqueN(rep$bank_standardized), format(min(rep$date)), format(max(rep$date))))

# Is MN + ME the whole deposit universe? Compare with the published total. The report
# is in WHOLE thousands of soles and each column is rounded separately, so the identity
# holds up to +/- 1 (thousand soles); larger relative gaps only occur on tiny balances.
both <- rep[!is.na(dep_mn) & !is.na(dep_me) & !is.na(dep_total) & dep_total > 0]
gap <- both$dep_mn + both$dep_me - both$dep_total
cat(sprintf("MN + ME - published total over %s institution-months: max |gap| = %.0f thousand soles, exact in %.1f%%, rounding-only (|gap| <= 1) in %.1f%%.\n",
            fmt(nrow(both)), max(abs(gap)), 100 * mean(gap == 0), 100 * mean(abs(gap) <= 1)))
stopifnot(max(abs(gap)) <= 1)       # MN + ME constitute the deposit universe (no third category)

# Shares for every institution-month, with the validation applied month by month.
# Missing deposits (edpymes, which take no deposits) stay missing: never treated as zero.
shares <- rep[, .(bank_standardized, reporte_name = entity, date, dep_mn, dep_me)]
shares[, denom := dep_mn + dep_me]
shares[, `:=`(s_mn = fifelse(!is.na(denom) & denom > 0, dep_mn / denom, NA_real_),
              s_me = fifelse(!is.na(denom) & denom > 0, dep_me / denom, NA_real_))]
shares[, reason := fcase(
  is.na(dep_mn) | is.na(dep_me),             "deposits missing (not deposit-taking)",
  dep_mn < 0 | dep_me < 0,                   "negative deposits",
  denom <= 0,                                "non-positive denominator",
  s_mn < -TOL | s_mn > 1 + TOL | s_me < -TOL | s_me > 1 + TOL, "share outside [0,1]",
  abs(s_mn + s_me - 1) > TOL,                "shares do not sum to one",
  default =                                  "valid")]
shares[, valid := reason == "valid"]
stopifnot(all(shares[valid == TRUE, abs(s_mn + s_me - 1) <= TOL]),
          all(shares[valid == TRUE, dep_mn >= 0 & dep_me >= 0 & denom > 0 & s_mn >= 0 & s_me >= 0 & s_mn <= 1 & s_me <= 1]))
setorder(shares, bank_standardized, date)
fwrite(shares[, .(bank_standardized, reporte_name, date, dep_mn_thousand_soles = dep_mn,
                  dep_me_thousand_soles = dep_me, s_mn, s_me, valid, reason)],
       file.path(PROC, "bank_deposit_shares.csv"))
cat(sprintf("Share panel: %s institution-months, %d institutions; valid shares in %s (%.1f%%).\n",
            fmt(nrow(shares)), uniqueN(shares$bank_standardized), fmt(sum(shares$valid)), 100 * mean(shares$valid)))
print(shares[, .N, by = reason][order(-N)])

# ---- baseline: pick BASELINE_DATE from the panel ------------------------------
hdr("1b. Baseline shares")
base <- shares[date == BASELINE_DATE]
sh <- merge(shares[, .(reporte_names = paste(unique(reporte_name), collapse = " / ")), by = bank_standardized],
            base[, .(bank_standardized, dep_mn, dep_me, s_mn_0 = s_mn, s_me_0 = s_me, row_reason = reason)],
            by = "bank_standardized", all.x = TRUE)
sh[, reason := fifelse(is.na(row_reason), "no Reporte row at baseline date", row_reason)]
sh[, valid := reason == "valid"]
setorder(sh, -valid, bank_standardized)
cat(sprintf("Baseline %s: %d Reporte institutions; valid shares for %d; invalid/absent for %d.\n",
            format(BASELINE_DATE), nrow(sh), sum(sh$valid), sum(!sh$valid)))
print(sh[, .N, by = reason])
cat("\nValid baseline shares (MN share of deposits):\n")
print(sh[valid == TRUE, .(bank_standardized, dep_mn = round(dep_mn), dep_me = round(dep_me),
                          s_mn_0 = round(s_mn_0, 4), s_me_0 = round(s_me_0, 4))], nrows = 100)

# ==============================================================================
# 2. Currency-specific reserve-requirement changes (percentage points)
# ==============================================================================
hdr("2. Reserve-requirement changes")
enc <- fread(ENCAJE_CSV)
enc[, date := as.Date(date)]
setorder(enc, date)
stopifnot(!anyDuplicated(enc$date), all(diff(enc$date) %in% 28:31))   # complete monthly calendar
need <- c(paste0(rep(c("mn", "me"), each = length(MEASURES) + 1), "_",
                 c(MEASURES, "gen_regime")))
stopifnot(all(need %in% names(enc)))
cat("Measures used: ", paste(need, collapse = ", "), "\n", sep = "")

# SCALE: rates are stored in PERCENT of reservable obligations (9 = 9%), per the
# codebook and the header of 09_encaje_series.R; so differences are already
# percentage points. Verified from the values, then no rescaling is applied.
for (v in grep("^(mn|me)_gen_(min_legal|marginal)$", names(enc), value = TRUE)) {
  r <- range(enc[[v]], na.rm = TRUE); cat(sprintf("  %-18s range %5.2f to %5.2f (percent)\n", v, r[1], r[2]))
  stopifnot(r[1] >= 0, r[2] <= 100, r[2] > 1)
}
# CARRY-FORWARD: each monthly value is "the rate in force on the LAST day of the month"
# (09_encaje_series.R), i.e. the circulars' rates stay in force until a later circular
# changes them. So a month without a circular is a genuine no-change month (delta = 0).
# Missing values are NOT filled: they are checked below and left as NA.
stopifnot(!anyNA(enc$mn_gen_min_legal), !anyNA(enc$me_gen_min_legal), !anyNA(enc$me_gen_marginal))
# gen_marginal (MN) exists only while the MN regime is "base + marginal"; under the single-rate
# regime there is no marginal rate (not applicable), and it stays NA rather than 0.
stopifnot(identical(is.na(enc$mn_gen_marginal), enc$mn_gen_regime == "single"))
stopifnot(identical(is.na(enc$me_gen_marginal), enc$me_gen_regime == "single"))

# The changes are computed here, in memory, straight from the rate panel (no separate file:
# it would only duplicate encaje_rates_monthly.csv).
dl <- enc[, .(date, mn_regime = mn_gen_regime, me_regime = me_gen_regime)]
for (j in MEASURES) for (cur in c("mn", "me")) {
  lvl <- enc[[paste0(cur, "_", j)]]
  # change = level(t) - level(t-1); NA whenever either endpoint is NA. Positive = tighter.
  set(dl, j = paste0("d_", cur, "_", j), value = lvl - shift(lvl))
}
dl[, `:=`(regime_switch_mn = mn_regime != shift(mn_regime), regime_switch_me = me_regime != shift(me_regime))]
# Month key: the panel's date is the first of the month and carries the month-END rate, so the
# shock is stamped with that month's last day (fecha_reporte, the credit data's date convention).
# Growth over (month-end t-1, month-end t] then uses delta_t = r_t - r_{t-1}: every change
# effective inside month t is in the interval's delta.
dl[, fecha_reporte := as.Date(sprintf("%04d-%02d-01", year(date) + (month(date) == 12L), month(date) %% 12L + 1L)) - 1L]
stopifnot(!anyDuplicated(dl$fecha_reporte), all(as.integer(format(dl$fecha_reporte + 1, "%d")) == 1L))
cat(sprintf("Rate panel: %d months, %s to %s.\n", nrow(dl), format(min(dl$fecha_reporte)), format(max(dl$fecha_reporte))))
for (j in MEASURES) for (cur in c("mn", "me")) {
  v <- dl[[paste0("d_", cur, "_", j)]]
  cat(sprintf("  d_%s_%-14s months with data %3d | zero %3d | tighter %2d | looser %2d | NA %3d\n",
              cur, j, sum(!is.na(v)), sum(v == 0, na.rm = TRUE), sum(v > 0, na.rm = TRUE),
              sum(v < 0, na.rm = TRUE), sum(is.na(v))))
}
cat(sprintf("  Regime switches: MN %d, ME %d (gen_regime is categorical: no pp change).\n",
            sum(dl$regime_switch_mn, na.rm = TRUE), sum(dl$regime_switch_me, na.rm = TRUE)))

# ==============================================================================
# 3. Bank shocks, unique by bank_standardized x fecha_reporte
# ==============================================================================
hdr("3. Bank shocks")
banks_ok <- sh[valid == TRUE, .(bank_standardized, s_mn_0, s_me_0)]
cat(sprintf("Banks with valid baseline shares: %d (of %d Reporte institutions).\n", nrow(banks_ok), nrow(sh)))
shocks <- CJ(bank_standardized = banks_ok$bank_standardized, fecha_reporte = dl$fecha_reporte)
shocks <- merge(shocks, banks_ok, by = "bank_standardized")
shocks <- merge(shocks, dl[, c("fecha_reporte", grep("^d_", names(dl), value = TRUE)), with = FALSE], by = "fecha_reporte")
for (j in MEASURES)   # NA propagates: an undefined delta gives an undefined shock, never a zero
  shocks[, (paste0("bank_shock_", j)) := s_mn_0 * get(paste0("d_mn_", j)) + s_me_0 * get(paste0("d_me_", j))]
shocks[, baseline_date := BASELINE_DATE]
setcolorder(shocks, c("bank_standardized", "fecha_reporte", "baseline_date", "s_mn_0", "s_me_0"))
setorder(shocks, bank_standardized, fecha_reporte)
stopifnot(!anyDuplicated(shocks[, .(bank_standardized, fecha_reporte)]))
fwrite(shocks, file.path(PROC, "bank_encaje_shocks.csv"))
cat(sprintf("Shock table: %s rows = %d banks x %d months; unique by (bank_standardized, fecha_reporte).\n",
            fmt(nrow(shocks)), uniqueN(shocks$bank_standardized), uniqueN(shocks$fecha_reporte)))
for (j in MEASURES) {
  v <- shocks[[paste0("bank_shock_", j)]]
  cat(sprintf("  bank_shock_%-14s non-missing %s | non-zero %s | NA %s | range [%.3f, %.3f] | sd %.3f\n",
              j, fmt(sum(!is.na(v))), fmt(sum(v != 0, na.rm = TRUE)), fmt(sum(is.na(v))),
              min(v, na.rm = TRUE), max(v, na.rm = TRUE), sd(v, na.rm = TRUE)))
}

hdr("Session")
print(sessionInfo())
sink(); close(log_con)
