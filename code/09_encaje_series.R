# ==============================================================================
# 09_encaje_series.R -- BCRP reserve-requirement (encaje) rates for Peru:
#   monthly panel, one column per rate category
# ==============================================================================
# The BCRP does not publish a rate series. It is rebuilt from the circulars
# "Disposiciones de encaje en moneda nacional / extranjera". The 186 circulars
# were located through the BCRP year pages and read in a browser, because
# bcrp.gob.pe returns a bot-detection stub to curl / httr.
#
# Inputs  (data/raw/bcrp_encaje/, hand-coded from the circular texts)
#   circulares_index.csv       every encaje circular 1996-2026: id, date, URL
#   encaje_events_coded.csv    one row per parameter taking a new value:
#                              circular, effective date, currency, category,
#                              rate type, tier, new value
#   encaje_noncoded_reasons.csv  circulars with no rate event + why
#
# Outputs
#   data/processed/encaje_rates_monthly.csv               wide monthly panel
#   data/documentation/encaje_rates_monthly_codebook.csv  one row per panel column
#
# Conventions
#   * Unit: percent of reservable obligations (pp for changes).
#   * Monthly value = rate in force on the LAST day of the month, so a change
#     effective on any day of a month is in that month's value. Mid-month
#     effective dates (8-Aug-2010, 12-Sep-2010) are listed in the README sheet.
#   * No rate differs by deposit type (vista / ahorro / plazo): every circular
#     applies one rate to all reservable deposits. Rates differ by currency
#     (MN / ME), by regime and by funding source (domestic general regime vs
#     foreign-source obligations). Checked by searching every circular for rates
#     tied to those instruments.
#   * "NA" in the events file = parameter ended (or is not a fixed number).
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

RAW  <- "data/raw/bcrp_encaje"
PROC <- "data/processed"
DOC  <- "data/documentation"
dir.create(PROC, showWarnings = FALSE, recursive = TRUE)

LAST_MONTH <- as.Date("2026-10-01")   # latest month with in-force data (script written 2026-10-08)

# ---- read ---------------------------------------------------------------------
idx <- fread(file.path(RAW, "circulares_index.csv"), colClasses = "character")
ev  <- fread(file.path(RAW, "encaje_events_coded.csv"), colClasses = "character")
nc  <- fread(file.path(RAW, "encaje_noncoded_reasons.csv"), colClasses = "character")

stopifnot(!anyDuplicated(idx$circular))
ev[, effective_date := as.Date(effective_date)]
stopifnot(!anyNA(ev$effective_date))

# every circular is either coded or has a stated reason, and nothing is orphaned
coded_ids <- setdiff(unique(ev$circular), "000-REGIME")
stopifnot(
  all(coded_ids %in% idx$circular),
  all(nc$circular %in% idx$circular),
  !any(nc$circular %in% coded_ids),
  all(idx$circular %in% c(coded_ids, nc$circular))
)

# ---- labels -------------------------------------------------------------------
cat_lab <- c(
  gen    = "General regime (all deposits and obligations)",
  res    = "Reserve composition",
  idx    = "Special regime: obligations indexed to FX / MN instruments",
  str    = "Special regime: structured deposits with FX options",
  forfin = "Special regime, foreign source: deposits and other obligations from foreign financial institutions",
  forcr  = "Special regime, foreign source: credits < 2-3 years from foreign financial entities / international bodies (new contracts)",
  fordrv = "Special regime, foreign source: rights derived from foreign credits",
  trade  = "Special regime, foreign source: external trade-finance credit lines",
  addl   = "Additional reserve regimes (flags)"
)
src_lab <- c(gen = "domestic (all depositors)", res = "domestic (all depositors)",
             idx = "any depositor (ME: foreign entities)", str = "domestic",
             forfin = "foreign", forcr = "foreign", fordrv = "foreign",
             trade = "foreign", addl = "n/a")
rt_lab <- c(
  min_legal = "Minimum legal reserve rate",
  general = "Single rate on general-regime obligations",
  marginal = "Marginal rate (on obligations above the base level)",
  avg_cap = "Cap on the average rate",
  min_avg = "Minimum average rate",
  base_cap = "Cap on the entity base rate",
  regime = "Regime type (single rate vs base + marginal)",
  base_addon_step_pp = "Add-on to the entity's implicit base rate (pp, flow)",
  deposit_min_pct = "Minimum current-account deposit at BCRP (% of reservable obligations)",
  rate = "Rate",
  exempt_limit_pct_pe = "Exempt limit (% of effective equity)",
  fx_derivatives_active = "Additional MN reserve on FX forwards/swaps active (1/0)",
  total_credit_active = "Additional ME reserve on total FX credit active (1/0)",
  vehicle_mortgage_credit_active = "Additional ME reserve on vehicle + mortgage FX credit active (1/0)",
  consumer_mortgage_credit_active = "Additional ME reserve on consumer + mortgage FX credit active (1/0)"
)
tier_lab <- c(all = "all", tier1 = "within limit (tier 1)", excess = "excess over limit",
              upto_cap = "up to cap")

# series identifier -> column name in the monthly panel
ev[, series := tolower(paste0(currency, "_", category, "_", rate_type,
                              fifelse(tier == "all", "", paste0("_", tier))))]
ev[, is_flow := grepl("_step_pp$", rate_type)]
ev[, is_cat  := rate_type == "regime"]
ev[, value_num := suppressWarnings(as.numeric(value))]
ev[, future := effective_date > LAST_MONTH + 31]

# ---- change log ---------------------------------------------------------------
setorder(ev, series, effective_date, circular)
ev[, old_value := shift(value_num), by = series]
ev[, old_cat   := shift(value),     by = series]
ev[, first_obs := seq_len(.N) == 1L, by = series]

ev[, change_type := fcase(
  is_flow,                         "add-on step (flow)",
  is_cat & first_obs,              "initial regime",
  is_cat,                          "regime change",
  first_obs & is.na(value_num),    "n/a",
  first_obs,                       "first observation",
  is.na(value_num),                "ended / not a fixed number",
  is.na(old_value),                "restarted",
  value_num > old_value,           "increase",
  value_num < old_value,           "decrease",
  default =                        "restated (no change)"
)]
ev[, change_pp := fcase(
  is_flow,                              value_num,
  is_cat | first_obs,                   NA_real_,
  is.na(value_num) | is.na(old_value),  NA_real_,
  default =                             value_num - old_value
)]

chg <- merge(ev, idx[, .(circular, issue_date, url)], by = "circular", all.x = TRUE, sort = FALSE)
chg <- chg[circular != "000-REGIME" & change_type != "restated (no change)"]
chg[, issue_date := as.Date(issue_date)]
setorder(chg, issue_date, effective_date, currency, category, rate_type, tier)

# ---- monthly panel ------------------------------------------------------------
months  <- seq(as.Date("1996-04-01"), LAST_MONTH, by = "month")
month_end <- seq(months[2], by = "month", length.out = length(months)) - 1L   # last day of each month
lev <- ev[!(is_flow) & circular != "000-REGIME" | (is_cat)]
lev <- lev[effective_date <= LAST_MONTH + 30]   # drop events not yet in force

value_at <- function(d, num = TRUE) {
  # last event with effective_date <= month end; NA before the first event
  k <- findInterval(as.numeric(month_end), as.numeric(d$effective_date))
  v <- if (num) d$value_num else d$value
  out <- rep(NA, length(month_end)); out[k > 0] <- v[k[k > 0]]
  out
}

panel <- data.table(date = months)
for (s in unique(lev$series)) {
  d <- lev[series == s][order(effective_date)]
  set(panel, j = s, value = value_at(d, num = !d$is_cat[1]))
}
setcolorder(panel, c("date", sort(setdiff(names(panel), "date"))))

# derived: rate applied to ALL general-regime obligations while the MN regime is a
# single rate (explicit statement where the circular gives one, otherwise the
# minimum legal rate, which the circular then applies to every obligation)
panel[, mn_gen_single_rate := fifelse(mn_gen_regime == "single",
                                       fcoalesce(mn_gen_general, mn_gen_min_legal), NA_real_)]
setcolorder(panel, c("date", sort(setdiff(names(panel), "date"))))

# ---- internal consistency -----------------------------------------------------
# (1) every level in the panel is reproduced by replaying the change log
#     (when two events fall in one month, the later one is the month-end value)
chk <- chg[!(is_flow) & !(is_cat) & !is.na(value_num) & !future]
chk <- chk[order(effective_date), .SD[.N], by = .(series, ym = format(effective_date, "%Y-%m"))]
for (s in unique(chk$series)) {
  d <- chk[series == s]
  stopifnot(all(abs(panel[[s]][match(d$ym, format(panel$date, "%Y-%m"))] - d$value_num) < 1e-9))
}
# (2) chain of old/new values is unbroken within each series
brk <- chg[!is_flow & !is_cat & !first_obs & !is.na(old_value) & !is.na(value_num)][
  , .(ok = all(!is.na(change_pp))), by = series]
stopifnot(all(brk$ok))
# (3) panel is monthly and complete
stopifnot(!anyDuplicated(panel$date), all(diff(panel$date) %in% 28:31))

# ---- write panel --------------------------------------------------------------
fwrite(panel, file.path(PROC, "encaje_rates_monthly.csv"))

# ---- codebook -----------------------------------------------------------------
dict <- unique(ev[circular != "000-REGIME",
                  .(series, category = cat_lab[category], funding_source = src_lab[category],
                    rate_type = rt_lab[rate_type], tier = tier_lab[tier])])
setorder(dict, series)
first_eff <- ev[circular != "000-REGIME", .(first_effective = min(effective_date)), by = series]
dict <- merge(dict, first_eff, by = "series", sort = FALSE)
dict[, first_month := format(first_effective, "%Y-%m")][, first_effective := NULL]

extra <- data.table(
  series = c("mn_gen_single_rate", "mn_gen_regime", "me_gen_regime"),
  category = cat_lab["gen"], funding_source = src_lab["gen"],
  rate_type = c("DERIVED: rate applied to all general-regime obligations while the MN regime is a single rate (explicit rate if stated, else the minimum legal rate)",
                rt_lab["regime"], rt_lab["regime"]),
  tier = "all", first_month = c("1996-04", "1996-04", "1996-04"))
dict <- rbind(dict, extra[!series %in% dict$series])
dict <- dict[series %in% names(panel)]      # future-dated series are not in the panel
setorder(dict, series)
fwrite(dict, file.path(DOC, "encaje_rates_monthly_codebook.csv"))

message(sprintf("panel: %d months x %d series | circulars: %d coded, %d without a rate event",
                nrow(panel), ncol(panel) - 1L, length(coded_ids), nrow(nc)))
