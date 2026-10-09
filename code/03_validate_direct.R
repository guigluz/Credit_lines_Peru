# ==============================================================================
# 03_validate_direct.R -- Reconcile parsed B-2343 against Series Estadisticas
# ==============================================================================
# Source A: data/processed/b2343_*.csv          (parsed SBS bulletin workbooks)
# Source B: data/raw/Reporte(01-2001 - 09-2022).csv  (Series Estadisticas extract)
#
# Source B carries ONLY total Creditos/Depositos by entity-month (split MN/ME).
# It has no modality breakdown, so bank x modality x month CANNOT be validated
# across sources. That grain is checked internally instead, and the gap is
# reported rather than papered over.
#
# All monetary values are in miles de S/ in BOTH sources.
#
# Usage:  Rscript code/03_validate_direct.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble)
})
source("code/00_sbs_utils.R")

PROC   <- "data/processed"
VALID  <- "output/validation"
LOG    <- "output/logs/03_validate_direct.log"
REPORTE<- "data/raw/Reporte(01-2001 - 09-2022).csv"
dir.create(VALID, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(LOG), recursive = TRUE, showWarnings = FALSE)

con <- file(LOG, open = "wt"); sink(con, split = TRUE); on.exit({sink(); close(con)}, add = TRUE)
cat("=== 03_validate_direct.R ===\n", format(Sys.time()), "\n\n")

# ---- tolerance policy --------------------------------------------------------
# Values are published as integers before 2013 and to 3 decimals afterwards,
# in miles de S/. Diagnostics are reported untruncated; nothing is rounded away.
TOL_EXACT <- 0.5     # miles S/ -- rounding to the nearest thousand soles
TOL_ABS   <- 1.0     # miles S/
TOL_REL   <- 1e-6    # relative
TOL_LOOSE <- 1e-4    # relative (0.01%)
TOL_SHARE <- 1e-6    # percentage points, for the share-sum identity
TOL_PP_EXACT <- 1e-6 # percentage points, for share-vs-share comparisons
TOL_PP_ABS   <- 1e-3 # percentage points

#' Compare two vectors under the explicit tolerance policy.
#' tol_exact/tol_abs are in the UNITS OF THE COMPARISON: miles de S/ for the
#' level comparisons, percentage points for the share comparisons.
compare_vals <- function(a, b, tol_exact = TOL_EXACT, tol_abs = TOL_ABS) {
  d     <- a - b
  ad    <- abs(d)
  denom <- pmax(abs(a), abs(b))
  rel   <- ifelse(denom > 0, ad / denom, ifelse(ad == 0, 0, NA_real_))
  ratio <- ifelse(b != 0 & !is.na(b), a / b, NA_real_)
  near  <- function(r, k) !is.na(r) & abs(r - k) / k <= 1e-3
  tibble(
    diff = d, abs_diff = ad, rel_diff = rel,
    pct_diff = ifelse(!is.na(b) & b != 0, 100 * d / b, NA_real_),
    ratio = ratio,
    match_exact = !is.na(ad) & ad <= tol_exact,
    match_abs   = !is.na(ad) & ad <= tol_abs,
    match_rel   = !is.na(rel) & rel <= TOL_REL,
    match_loose = !is.na(rel) & rel <= TOL_LOOSE,
    scale_flag  = case_when(
      near(ratio, 1000)  ~ "a_is_1000x_b",
      near(ratio, 1e-3)  ~ "b_is_1000x_a",
      near(ratio, 1e6)   ~ "a_is_1e6x_b",
      near(ratio, 1e-6)  ~ "b_is_1e6x_a",
      TRUE               ~ NA_character_)
  )
}

#' One summary row per grouping level. Reports counts, match shares and the
#' full distribution of differences -- max values are NOT rounded.
summarise_level <- function(df, level_name) {
  n <- nrow(df); comparable <- sum(!is.na(df$abs_diff))
  tibble(
    level = level_name,
    n_obs = n,
    n_comparable = comparable,
    n_excel_only = sum(is.na(df$value_b) & !is.na(df$value_a)),
    n_seriesest_only = sum(is.na(df$value_a) & !is.na(df$value_b)),
    n_match_exact = sum(df$match_exact, na.rm = TRUE),
    share_match_exact = ifelse(comparable > 0, sum(df$match_exact, na.rm = TRUE) / comparable, NA_real_),
    n_match_abs = sum(df$match_abs, na.rm = TRUE),
    share_match_abs = ifelse(comparable > 0, sum(df$match_abs, na.rm = TRUE) / comparable, NA_real_),
    n_match_rel = sum(df$match_rel, na.rm = TRUE),
    n_match_loose = sum(df$match_loose, na.rm = TRUE),
    share_match_loose = ifelse(comparable > 0, sum(df$match_loose, na.rm = TRUE) / comparable, NA_real_),
    n_mismatch = comparable - sum(df$match_loose, na.rm = TRUE),
    share_mismatch = ifelse(comparable > 0, 1 - sum(df$match_loose, na.rm = TRUE) / comparable, NA_real_),
    max_abs_diff = suppressWarnings(max(df$abs_diff, na.rm = TRUE)),
    mean_abs_diff = mean(df$abs_diff, na.rm = TRUE),
    median_abs_diff = median(df$abs_diff, na.rm = TRUE),
    max_rel_diff = suppressWarnings(max(df$rel_diff, na.rm = TRUE)),
    max_pct_diff = suppressWarnings(max(abs(df$pct_diff), na.rm = TRUE)),
    n_scale_flag = sum(!is.na(df$scale_flag))
  ) %>% mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_)))
}

# ---- Source A: parsed bulletin ----------------------------------------------
panel  <- read_csv(file.path(PROC, "b2343_long.csv"), show_col_types = FALSE)
totals <- read_csv(file.path(PROC, "b2343_total_credits.csv"), show_col_types = FALSE)
manual <- read_bank_map()

# ---- Source B: Series Estadisticas extract ----------------------------------
# UTF-32LE, no BOM. Columns are renamed BY POSITION: accented names are
# unreliable under a C locale (same approach as code/peru_dollarization.R).
srcB_raw <- read_csv(REPORTE, locale = readr::locale(encoding = "UTF-32LE"),
                     col_types = cols(.default = col_character()),
                     name_repair = "minimal")
stopifnot(ncol(srcB_raw) >= 8)
names(srcB_raw)[1:8] <- c("entity", "fecha", "cred_total", "cred_mn", "cred_me",
                          "dep_total", "dep_mn", "dep_me")

srcB <- srcB_raw %>%
  transmute(
    entity_raw  = str_squish(entity),
    report_date = as.Date(str_squish(fecha), format = "%d/%m/%Y"),
    cred_total  = sbs_num(cred_total),
    cred_mn     = sbs_num(cred_mn),
    cred_me     = sbs_num(cred_me)
  ) %>%
  filter(!is.na(report_date), !is.na(entity_raw), entity_raw != "")

bstd <- standardize_banks(unique(srcB$entity_raw), "series_estadisticas", manual)
# Minimal spoke for this dataset (this is the only script that standardizes the
# Series Estadisticas names). Non-bank entities have their own slugs in the bank map.
write_xwalk(bstd, "reporte")
srcB <- srcB %>% left_join(bstd %>% select(entity_raw = bank_raw, bank_standardized,
                                           review_required),
                           by = "entity_raw")

cat("--- Source B (Series Estadisticas) ---\n")
cat(sprintf("rows: %d | entities: %d | months: %d (%s .. %s)\n",
            nrow(srcB), n_distinct(srcB$entity_raw), n_distinct(srcB$report_date),
            min(srcB$report_date), max(srcB$report_date)))
# Universe = slugs present in Source A (the parsed bulletin, banca multiple plus
# __SYSTEM__). The test used to be is.na(bank_standardized); financieras, edpymes
# and cajas rurales now carry slugs too, so membership in Source A is the test.
in_universe <- srcB$bank_standardized %in% unique(na.omit(panel$bank_standardized))
excluded <- srcB %>% filter(!in_universe) %>% count(entity_raw, name = "n_months")
cat(sprintf("entities NOT in the banca multiple universe (excluded): %d\n", nrow(excluded)))
write_csv(excluded, file.path(VALID, "seriesest_entities_excluded.csv"))

srcB_bm <- srcB %>% filter(in_universe)
cat(sprintf("entities kept: %d\n\n", n_distinct(srcB_bm$bank_standardized)))

# ---- overlap window ----------------------------------------------------------
dates_a <- sort(unique(totals$report_date))
dates_b <- sort(unique(srcB_bm$report_date))
overlap <- intersect(dates_a, dates_b) %>% as.Date(origin = "1970-01-01") %>% sort()
cat(sprintf("Excel months   : %d (%s .. %s)\n", length(dates_a), min(dates_a), max(dates_a)))
cat(sprintf("Series months  : %d (%s .. %s)\n", length(dates_b), min(dates_b), max(dates_b)))
cat(sprintf("OVERLAP        : %d (%s .. %s)\n\n", length(overlap), min(overlap), max(overlap)))

# ==============================================================================
# LEVEL 1 -- bank x modality x month  (INTERNAL ONLY: Source B has no modality)
# ==============================================================================
shares <- panel %>%
  filter(!is_total_col, bank_standardized != "__SYSTEM__") %>%
  group_by(report_date, bank_raw, bank_standardized) %>%
  summarise(share_sum = sum(value_num, na.rm = TRUE),
            n_modalities = n(),
            n_missing = sum(is.na(value_num)),
            min_share = suppressWarnings(min(value_num, na.rm = TRUE)),
            max_share = suppressWarnings(max(value_num, na.rm = TRUE)),
            .groups = "drop") %>%
  left_join(totals %>% filter(bank_standardized != "__SYSTEM__") %>%
              select(report_date, bank_raw, total_credits_thousand_pen),
            by = c("report_date", "bank_raw")) %>%
  mutate(
    zero_credit_bank = is.na(total_credits_thousand_pen) | total_credits_thousand_pen == 0,
    share_sum_dev    = abs(share_sum - 100),
    share_sum_ok     = zero_credit_bank | share_sum_dev <= TOL_SHARE,
    share_range_ok   = zero_credit_bank |
                       (!is.na(min_share) & min_share >= -1e-6 & max_share <= 100 + 1e-6)
  )
write_csv(shares, file.path(VALID, "internal_modality_checks.csv"))

cat("--- LEVEL 1: bank x modality x month (internal) ---\n")
cat(sprintf("bank-months                    : %d\n", nrow(shares)))
cat(sprintf("zero-credit bank-months        : %d\n", sum(shares$zero_credit_bank)))
cat(sprintf("share-sum == 100 (<=%g pp)  : %d of %d active (%.4f%%)\n", TOL_SHARE,
            sum(shares$share_sum_ok & !shares$zero_credit_bank),
            sum(!shares$zero_credit_bank),
            100 * sum(shares$share_sum_ok & !shares$zero_credit_bank) / sum(!shares$zero_credit_bank)))
cat(sprintf("max |share sum - 100| (active) : %.10f pp\n",
            max(shares$share_sum_dev[!shares$zero_credit_bank], na.rm = TRUE)))
cat(sprintf("shares outside [0,100]         : %d bank-months\n",
            sum(!shares$share_range_ok, na.rm = TRUE)))
cat(sprintf("negative share cells           : %d\n", sum(panel$value_flag == "negative_share")))
cat(sprintf("float-noise cells (|v|<1e-6)   : %d\n\n", sum(panel$value_flag == "float_noise")))

# ==============================================================================
# LEVEL 2 -- bank x month  (CROSS-SOURCE: the primary test)
# ==============================================================================
a_bank <- totals %>%
  filter(bank_standardized != "__SYSTEM__", report_date %in% overlap) %>%
  group_by(report_date, bank_standardized) %>%
  summarise(value_a = sum(total_credits_thousand_pen, na.rm = TRUE),
            n_excel_rows = n(),
            scope_note = paste(unique(na.omit(scope_note)), collapse = "; "),
            file_format = first(file_format),
            review_required = any(review_required), .groups = "drop") %>%
  mutate(scope_note = na_if(scope_note, ""))

b_bank <- srcB_bm %>%
  filter(bank_standardized != "__SYSTEM__", report_date %in% overlap) %>%
  group_by(report_date, bank_standardized) %>%
  summarise(value_b = sum(cred_total, na.rm = TRUE), n_series_rows = n(), .groups = "drop")

recon_bank <- full_join(a_bank, b_bank, by = c("report_date", "bank_standardized")) %>%
  bind_cols(., compare_vals(.$value_a, .$value_b)) %>%
  mutate(report_year = as.integer(format(report_date, "%Y")),
         coverage = case_when(
           is.na(value_a) ~ "series_only",
           is.na(value_b) ~ "excel_only",
           TRUE ~ "both")) %>%
  arrange(report_date, bank_standardized)
write_csv(recon_bank, file.path(VALID, "recon_bank_month.csv"))

# ==============================================================================
# LEVEL 3 -- modality x month  (INTERNAL ONLY)
# ==============================================================================
# System-level modality shares, and the total row vs the sum of bank rows.
mod_month <- panel %>%
  filter(!is_total_col, report_date %in% dates_a) %>%
  mutate(kind = ifelse(bank_standardized == "__SYSTEM__", "published_total", "bank")) %>%
  group_by(report_date, modality_std, kind) %>%
  summarise(v = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = kind, values_from = v)

# Implied system share from bank rows:
#   implied_j = 100 * sum_b (share_bj/100 * total_b) / sum_b total_b
# A "-" cell means the bank does no business in that modality, so it contributes
# ZERO to the numerator but its credit MUST still count in the denominator.
# (Dropping those banks from the denominator inflates the implied share -- it
# put mortgages ~1.2pp too high in 2025-26 before this was corrected.)
bank_tot <- totals %>%
  filter(bank_standardized != "__SYSTEM__") %>%
  select(report_date, bank_raw, tot = total_credits_thousand_pen)

sys_denom <- bank_tot %>% group_by(report_date) %>%
  summarise(denom = sum(tot, na.rm = TRUE), .groups = "drop")

implied <- panel %>%
  filter(!is_total_col, bank_standardized != "__SYSTEM__") %>%
  left_join(bank_tot, by = c("report_date", "bank_raw")) %>%
  group_by(report_date, modality_std) %>%
  summarise(numer = sum(coalesce(value_num, 0) / 100 * tot, na.rm = TRUE), .groups = "drop") %>%
  left_join(sys_denom, by = "report_date") %>%
  mutate(implied_share = ifelse(denom > 0, 100 * numer / denom, NA_real_)) %>%
  select(report_date, modality_std, implied_share)

recon_mod <- mod_month %>%
  left_join(implied, by = c("report_date", "modality_std")) %>%
  bind_cols(., compare_vals(.$published_total, .$implied_share,
                            tol_exact = TOL_PP_EXACT, tol_abs = TOL_PP_ABS)) %>%
  rename(value_a = published_total, value_b = implied_share) %>%
  mutate(report_year = as.integer(format(report_date, "%Y")))
write_csv(recon_mod, file.path(VALID, "recon_modality_month.csv"))

# ==============================================================================
# LEVEL 4 -- system x month  (CROSS-SOURCE)
# ==============================================================================
# 2001 workbooks carry NO "TOTAL BANCA MULTIPLE" row, so the system total is
# recomputed as the sum of bank rows for those months and labelled as such.
a_sys_published <- totals %>% filter(bank_standardized == "__SYSTEM__") %>%
  select(report_date, published = total_credits_thousand_pen)
a_sys_summed <- totals %>% filter(bank_standardized != "__SYSTEM__") %>%
  group_by(report_date) %>%
  summarise(summed = sum(total_credits_thousand_pen, na.rm = TRUE), .groups = "drop")

a_sys <- full_join(a_sys_published, a_sys_summed, by = "report_date") %>%
  mutate(value_a = coalesce(published, summed),
         system_source = ifelse(is.na(published), "summed_bank_rows", "published_total_row"),
         published_vs_summed_diff = published - summed)

b_sys <- srcB_bm %>% filter(bank_standardized == "__SYSTEM__") %>%
  select(report_date, value_b = cred_total)

recon_sys <- full_join(a_sys, b_sys, by = "report_date") %>%
  filter(report_date %in% overlap) %>%
  bind_cols(., compare_vals(.$value_a, .$value_b)) %>%
  mutate(report_year = as.integer(format(report_date, "%Y"))) %>%
  arrange(report_date)
write_csv(recon_sys, file.path(VALID, "recon_system_month.csv"))

# ==============================================================================
# LEVEL 5 -- pooled summary
# ==============================================================================
summary_levels <- bind_rows(
  summarise_level(recon_bank %>% filter(coverage == "both"), "2. bank x month (cross-source, matched entities) [miles S/]"),
  summarise_level(recon_bank, "2b. bank x month (cross-source, incl. one-sided coverage) [miles S/]"),
  summarise_level(recon_mod %>% filter(!is.na(value_a), !is.na(value_b)), "3. modality x month (internal: published vs implied share) [percentage points]"),
  summarise_level(recon_sys, "4. system x month (cross-source) [miles S/]")
) %>%
  add_row(level = "1. bank x modality x month (cross-source) -- NOT POSSIBLE: Series Estadisticas has no modality breakdown",
          n_obs = 0L, n_comparable = 0L, .before = 1)
write_csv(summary_levels, file.path(VALID, "summary_by_level.csv"))

cat("--- LEVEL 2: bank x month (cross-source) ---\n")
print(as.data.frame(recon_bank %>% count(coverage)), row.names = FALSE)
cat("\n")
print(as.data.frame(summary_levels %>%
  select(level, n_comparable, share_match_exact, share_match_loose, share_mismatch,
         max_abs_diff, mean_abs_diff, median_abs_diff, max_rel_diff)), row.names = FALSE)

# ---- discrepancy attribution -------------------------------------------------
attrib <- function(df, var) {
  df %>% filter(coverage == "both") %>%
    group_by(across(all_of(var))) %>%
    summarise(n = n(),
              n_mismatch = sum(!match_loose, na.rm = TRUE),
              share_mismatch = mean(!match_loose, na.rm = TRUE),
              max_abs_diff = max(abs_diff, na.rm = TRUE),
              max_rel_diff = max(rel_diff, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(share_mismatch), desc(max_rel_diff)) %>%
    mutate(dimension = var, key = as.character(.data[[var]])) %>%
    select(dimension, key, n, n_mismatch, share_mismatch, max_abs_diff, max_rel_diff)
}
attribution <- bind_rows(
  attrib(recon_bank, "bank_standardized"),
  attrib(recon_bank, "report_year"),
  attrib(recon_bank, "file_format"),
  attrib(recon_bank %>% mutate(has_scope = !is.na(scope_note)), "has_scope")
)
write_csv(attribution, file.path(VALID, "discrepancy_attribution.csv"))

cat("\n--- discrepancy concentration (bank x month, mismatch = outside 1e-4 relative) ---\n")
print(as.data.frame(attribution %>% filter(n_mismatch > 0) %>%
        arrange(dimension, desc(share_mismatch)) %>% head(30)), row.names = FALSE)

# ---- attribution test: is the system gap just the sum of the bank gaps? ------
# If the cross-source difference is a RESTATEMENT of particular banks (rather
# than a units, scope or parsing problem), then the system-level gap should
# equal the sum of the bank-level gaps in the same months, and agreement
# outside those months should be near-perfect. Both are tested here.
# Classify mismatches before locating the window. A gap of <= TOL_ABS
# (1 mil S/) is a precision artefact: the Series Estadisticas extract stores
# integers while the bulletin carries three decimals from 2013 on, so tiny
# balances can differ by one unit. Those are not evidence of a data problem.
recon_bank <- recon_bank %>%
  mutate(mismatch_kind = case_when(
    coverage != "both" ~ "one_sided_coverage",
    match_loose        ~ NA_character_,
    abs_diff <= TOL_ABS ~ "rounding_precision",
    TRUE                ~ "material"))
write_csv(recon_bank, file.path(VALID, "recon_bank_month.csv"))

n_round <- sum(recon_bank$mismatch_kind == "rounding_precision", na.rm = TRUE)
n_mat   <- sum(recon_bank$mismatch_kind == "material", na.rm = TRUE)
cat(sprintf("\nmismatches: %d material, %d rounding/precision (<= %g miles S/)\n",
            n_mat, n_round, TOL_ABS))

mismatch_months <- recon_bank %>% filter(mismatch_kind == "material") %>%
  distinct(report_date) %>% pull(report_date) %>% sort()

if (length(mismatch_months)) {
  win_lo <- min(mismatch_months); win_hi <- max(mismatch_months)
  in_win <- function(d) d >= win_lo & d <= win_hi

  bank_gap <- recon_bank %>% filter(coverage == "both", in_win(report_date)) %>%
    group_by(report_date) %>% summarise(bank_diff_sum = sum(diff, na.rm = TRUE), .groups = "drop")

  additivity <- recon_sys %>% filter(in_win(report_date)) %>%
    select(report_date, system_source, system_diff = diff) %>%
    left_join(bank_gap, by = "report_date") %>%
    mutate(residual = system_diff - bank_diff_sum)
  write_csv(additivity, file.path(VALID, "revision_window_analysis.csv"))

  culprits <- recon_bank %>% filter(coverage == "both", in_win(report_date)) %>%
    group_by(bank_standardized) %>%
    summarise(n_months = n(), n_mismatch = sum(mismatch_kind == "material", na.rm = TRUE),
              total_diff = sum(diff, na.rm = TRUE), .groups = "drop") %>%
    filter(n_mismatch > 0) %>% arrange(desc(abs(total_diff)))

  outside <- recon_bank %>% filter(coverage == "both", !in_win(report_date))

  cat(sprintf("\n--- discrepancy window: %s .. %s ---\n", win_lo, win_hi))
  cat(sprintf("banks accounting for every mismatch in the window: %s\n",
              paste(culprits$bank_standardized, collapse = ", ")))
  print(as.data.frame(culprits), row.names = FALSE)
  cat(sprintf("system gap vs sum of bank gaps: max |residual| = %.6g miles S/ over %d months\n",
              max(abs(additivity$residual), na.rm = TRUE), nrow(additivity)))
  cat(sprintf("OUTSIDE the window: n=%d, exact=%.4f%%, within 1e-4 rel=%.4f%%, max |diff|=%.6g miles S/\n",
              nrow(outside), 100 * mean(outside$match_exact, na.rm = TRUE),
              100 * mean(outside$match_loose, na.rm = TRUE),
              max(outside$abs_diff, na.rm = TRUE)))
}

# ---- manual-review queue -----------------------------------------------------
review_queue <- bind_rows(
  read_csv(file.path(XWALK_DIR, "xwalk_b2343.csv"), show_col_types = FALSE) %>%
    left_join(panel %>% group_by(bank_raw) %>%
                summarise(first_seen = min(report_date), last_seen = max(report_date),
                          n_months = n_distinct(report_date), .groups = "drop"),
              by = "bank_raw") %>%
    arrange(desc(review_required), bank_standardized, first_seen) %>%
    filter(review_required | is.na(bank_standardized)) %>%
    transmute(item = "bank_name_mapping", key = bank_raw,
              detail = paste0("std=", coalesce(bank_standardized, "<none>"),
                              "; ", first_seen, "..", last_seen, "; n=", n_months),
              severity = "high"),
  read_csv(file.path(VALID, "parse_diagnostics.csv"), show_col_types = FALSE) %>%
    filter(status != "ok") %>%
    transmute(item = "unparsed_file", key = source_file, detail = message, severity = "high"),
  recon_bank %>% filter(coverage == "both", !match_loose) %>%
    transmute(item = "bank_month_mismatch",
              key = paste(bank_standardized, report_date),
              detail = sprintf("excel=%.3f series=%.3f abs=%.3f rel=%.3e%s",
                               value_a, value_b, abs_diff, rel_diff,
                               ifelse(is.na(scale_flag), "", paste0(" scale=", scale_flag))),
              severity = ifelse(rel_diff > 1e-2, "high", "medium")),
  recon_bank %>% filter(coverage != "both") %>%
    transmute(item = paste0("coverage_", coverage),
              key = paste(bank_standardized, report_date),
              detail = sprintf("excel=%s series=%s",
                               ifelse(is.na(value_a), "NA", format(value_a)),
                               ifelse(is.na(value_b), "NA", format(value_b))),
              severity = "medium"),
  shares %>% filter(!zero_credit_bank, !share_sum_ok) %>%
    transmute(item = "share_sum_off_100", key = paste(bank_raw, report_date),
              detail = sprintf("sum=%.10f dev=%.3e pp", share_sum, share_sum_dev),
              severity = "low")
)
write_csv(review_queue, file.path(VALID, "review_queue.csv"))
cat("\n--- review queue ---\n")
print(as.data.frame(review_queue %>% count(item, severity)), row.names = FALSE)

cat("\nOutputs written to output/validation/.\nDone.\n")
