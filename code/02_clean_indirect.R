# ==============================================================================
# 02_clean_indirect.R -- Parse SBS cuadro B-2322 into a tidy long panel and
#   build the bank panel of UNUSED CREDIT LINES and its component types.
#   "Estructura de los Creditos Indirectos"
# ==============================================================================
# Structure is located by CONTENT ANCHORS, never by fixed row/column indices.
# Every structural fact below was verified by scanning all 306 workbooks
# (Enero 2001 - Junio 2026), applying the defect checklist from the B-2343 pass:
#
#   * .XLS files are legacy BIFF to Ago-2013 and OOXML from Set-2013 (same break
#     as B-2343). Handled upstream by sbs_normalize_ext().
#   * sheet count is 1 or 3; sheet name varies -> picked by title anchor
#   * header row is 5, 6 or 7 -> found by the "EMPRESAS" anchor
#   * title appears in mixed case and ALL CAPS
#   * units label is "(En porcentaje)" and once "( En porcentajes )"
#   * the 2001 workbooks carry NO "TOTAL BANCA MULTIPLE" row (10 files)
#   * units wording changed "nuevos soles" -> "soles"
#   * no wrong-workbook substitutions were found (unlike B-2343 Nov-2010), and
#     no two months share a sha256
#
# *** THE STRUCTURAL BREAK THAT MATTERS FOR THIS PROJECT ***
# The column "Lineas de credito no utilizadas y creditos concedidos no
# desembolsados" DOES NOT EXIST before Julio 2010. It appears in 2010-07 together
# with an asterisk on the total, footnoted to Res. SBS 11356-2008 (Reglamento
# para la Evaluacion y Clasificacion del Deudor y la Exigencia de Provisiones).
# The system total jumps from S/ 23.75 bn (2010-06) to S/ 63.80 bn (2010-07)
# purely because of this redefinition. Consequently:
#   - unused credit lines are available only from 2010-07 onward (192 months);
#   - "Total Creditos Indirectos" is NOT comparable across 2010-06/2010-07.
# Both facts are carried in the output as `has_unused_lines` and
# `definition_regime` rather than left for the analyst to rediscover.
#
# Units: the table is (En porcentaje). Type columns are percentage SHARES of
# each bank's total indirect credit; ONLY the last column is a level, in
# miles de S/. Levels below are therefore DERIVED as share/100 * total.
#
# Usage:  Rscript code/02_clean_indirect.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble)
})
source("code/00_sbs_utils.R")

CODE     <- "B-2322"
MANIFEST <- "data/raw/sbs_boletin/manifest_B-2322.csv"
PROC     <- "data/processed"
VALID    <- "output/validation"
DOC      <- "data/documentation"
LOG      <- "output/logs/02_clean_indirect.log"
for (d in c(PROC, VALID, DOC, dirname(LOG))) dir.create(d, recursive = TRUE, showWarnings = FALSE)

con <- file(LOG, open = "wt"); sink(con, split = TRUE); on.exit({sink(); close(con)}, add = TRUE)
cat("=== 02_clean_indirect.R ===\n", format(Sys.time()), "\n\n")

UNUSED_LINES_FROM <- as.Date("2010-07-31")   # first month the column exists

# ---- anchors -----------------------------------------------------------------
# Anchored on "INDIRECTOS" so the lookalike cuadros are not picked up by mistake
# (B-2309 "Estructura de Creditos Directos e Indirectos segun Categoria de
# Riesgo" and B-230802 both begin "ESTRUCTURA DE ... CREDITOS ... INDIRECTOS").
TITLE_RX  <- "^ESTRUCTURA DE LOS CREDITOS INDIRECTOS"
UNITS_RX  <- "^\\(\\s*EN PORCENTAJE"
HDR_RX    <- "^EMPRESAS$"
TOTROW_RX <- "^TOTAL(\\s|$)"
NOTE_RX   <- "^(NOTA|FUENTE|N O T A|\\*|[0-9]+/)"

# ---- indirect-credit type standardization -----------------------------------
TYPE_MAP <- tribble(
  ~key,                     ~type_std,                ~type_label_en,
  "AVALES",                 "avales",                 "Guarantees (avales)",
  "CARTAS FIANZAS",         "cartas_fianza",          "Standby letters of guarantee",
  "CARTAS FIANZA",          "cartas_fianza",          "Standby letters of guarantee",
  "CARTAS DE CREDITO",      "cartas_credito",         "Letters of credit",
  "ACEPTACIONES BANCARIAS", "aceptaciones_bancarias", "Bankers' acceptances",
  "LINEAS DE CREDITO NO UTILIZADAS Y CREDITOS CONCEDIDOS NO DESEMBOLSADOS",
                            "lineas_no_utilizadas",   "Unused credit lines and undisbursed committed credit",
  "TOTAL CREDITOS INDIRECTOS", "__TOTAL__",           "Total indirect credit (level)"
)
TYPE_ORDER <- c("avales", "cartas_fianza", "cartas_credito",
                "aceptaciones_bancarias", "lineas_no_utilizadas")

# Comparability caveats for these columns (see the header block for the full
# account of the 2010-07 redefinition):
#   lineas_no_utilizadas : TARGET SERIES; column absent before 2010-07.
#   __TOTAL__            : level in miles de S/; NOT comparable across
#                          2010-06/2010-07 (system total S/23.75bn -> S/63.80bn).
#                          Units wording changed "nuevos soles" -> "soles".
#   all share columns    : the denominator changes definition in 2010-07, so the
#                          shares are NOT comparable across that break either.

#' Strip footnote markers and the trailing units parenthetical from a header
#' label. Returns list(key, footnote_ref).
clean_type_label <- function(raw) {
  x <- norm_txt(raw)
  fn <- str_extract_all(x, "[0-9]+/|\\*")[[1]]
  x  <- str_squish(str_replace_all(x, "[0-9]+/|\\*", ""))
  # "(EN MILES DE [NUEVOS ]SOLES)" -- and one file omits the closing paren
  x  <- str_squish(str_remove(x, "\\(?\\s*EN MILES DE.*$"))
  list(key = x, footnote_ref = paste(fn, collapse = " "))
}

# ---- per-file parser ---------------------------------------------------------
parse_one <- function(path, url, report_year, report_month, file_format, sha256) {
  fname <- basename(path)
  diag <- tibble(
    source_file = fname, report_year = report_year, report_month = report_month,
    status = "ok", message = NA_character_, sheet_name = NA_character_,
    n_sheets = NA_integer_, title_row = NA_integer_, header_row = NA_integer_,
    units_label = NA_character_, date_cell_raw = NA_character_,
    report_date = as.Date(NA), date_matches_filename = NA,
    n_banks = 0L, n_types = 0L, has_total_row = NA, has_unused_lines = NA,
    n_cells = 0L, n_cells_missing = 0L, n_cells_unparsed = 0L
  )
  fail <- function(msg, status = "error") { diag$status <- status; diag$message <- msg
                                            list(diag = diag, data = NULL) }

  sheets <- try(readxl::excel_sheets(path), silent = TRUE)
  if (inherits(sheets, "try-error")) return(fail("excel_sheets() failed"))
  diag$n_sheets <- length(sheets)

  pick <- try(sbs_pick_sheet(path, TITLE_RX), silent = TRUE)
  if (inherits(pick, "try-error"))
    return(fail(paste0("table not found: ", str_squish(as.character(pick))), "wrong_table"))
  g <- pick$grid; diag$sheet_name <- pick$sheet

  tr <- sbs_find_cells(g, TITLE_RX); diag$title_row <- tr$row[1]
  table_title <- str_squish(tr$value[1])

  ur <- sbs_find_cells(g, UNITS_RX)
  if (!nrow(ur)) return(fail("units row '(En porcentaje)' not found"))
  diag$units_label <- str_squish(ur$value[1])

  date_raw <- NA_character_
  if (ur$row[1] > diag$title_row + 1L) {
    cand <- g[(diag$title_row + 1L):(ur$row[1] - 1L), 1]
    cand <- cand[!is.na(cand) & str_squish(cand) != ""]
    if (length(cand)) date_raw <- str_squish(as.character(cand[1]))
  }
  diag$date_cell_raw <- date_raw
  rdate    <- sbs_parse_date(date_raw)
  expected <- month_end(report_year, report_month)
  diag$date_matches_filename <- !is.na(rdate) &&
    format(rdate, "%Y-%m") == format(expected, "%Y-%m")
  diag$report_date <- if (is.na(rdate)) expected else rdate

  hd <- sbs_find_cells(g, HDR_RX) %>% filter(col == 1L)
  if (!nrow(hd)) return(fail("header anchor 'Empresas' not found"))
  hrow <- hd$row[1]; diag$header_row <- hrow

  hdr_vals <- unlist(g[hrow, ], use.names = FALSE)
  lab_cols <- which(!is.na(hdr_vals) & str_squish(hdr_vals) != "")
  lab_cols <- lab_cols[lab_cols > 1L]
  if (!length(lab_cols)) return(fail("no type labels right of 'Empresas'"))

  cleaned <- map(hdr_vals[lab_cols], clean_type_label)
  cols <- tibble(
    col_index    = lab_cols,
    type_raw     = str_squish(as.character(hdr_vals[lab_cols])),
    key          = map_chr(cleaned, "key"),
    footnote_ref = map_chr(cleaned, "footnote_ref")
  ) %>% left_join(TYPE_MAP, by = "key")

  if (any(is.na(cols$type_std)))
    return(fail(paste0("unmapped type label(s): ",
                       paste(unique(cols$type_raw[is.na(cols$type_std)]), collapse = " | "))))

  diag$n_types <- sum(cols$type_std != "__TOTAL__")
  diag$has_unused_lines <- "lineas_no_utilizadas" %in% cols$type_std

  first <- norm_txt(g[[1]])
  body  <- seq.int(hrow + 1L, nrow(g))
  note_at <- body[!is.na(first[body]) & str_detect(first[body], NOTE_RX)]
  last_row <- if (length(note_at)) min(note_at) - 1L else nrow(g)
  body <- body[body <= last_row]
  body <- body[!is.na(first[body]) & first[body] != ""]
  if (!length(body)) return(fail("empty data block"))

  is_total <- str_detect(first[body], TOTROW_RX)
  diag$has_total_row <- any(is_total)
  diag$n_banks <- sum(!is_total)

  long <- map_dfr(seq_len(nrow(cols)), function(k) {
    j <- cols$col_index[k]; txt <- as.character(g[[j]])[body]
    tibble(row_index = body, col_index = j,
           bank_raw = str_squish(as.character(g[[1]])[body]),
           is_total_row = is_total,
           type_raw = cols$type_raw[k], type_std = cols$type_std[k],
           footnote_ref = cols$footnote_ref[k],
           is_total_col = cols$type_std[k] == "__TOTAL__",
           value_text = txt, value_num = sbs_num(txt))
  })

  blankish <- is.na(long$value_text) | str_squish(long$value_text) %in% c("", "-", "–", "—")
  diag$n_cells          <- nrow(long)
  diag$n_cells_missing  <- sum(blankish)
  diag$n_cells_unparsed <- sum(!blankish & is.na(long$value_num))

  long <- long %>% mutate(
    cuadro = CODE, report_year = report_year, report_month = report_month,
    report_date = diag$report_date, sheet_name = diag$sheet_name,
    table_title = table_title, units_label = diag$units_label,
    source_file = fname, source_url = url, source_sha256 = sha256,
    file_format = file_format,
    value_flag = case_when(
      blankish                                              ~ "missing_marker",
      is.na(value_num)                                      ~ "unparsed",
      !is_total_col & abs(value_num) < 1e-6 & value_num != 0 ~ "float_noise",
      !is_total_col & value_num < 0                          ~ "negative_share",
      TRUE                                                   ~ "ok"))

  list(diag = diag, data = long)
}

# ---- run ---------------------------------------------------------------------
mf <- read_csv(MANIFEST, show_col_types = FALSE) %>% filter(available)
cat(sprintf("Parsing %d workbooks...\n\n", nrow(mf)))
out <- map(seq_len(nrow(mf)), function(i)
  parse_one(mf$dest[i], mf$url[i], mf$report_year[i], mf$report_month[i],
            mf$detected_format[i], mf$sha256[i]))

diagnostics <- map_dfr(out, "diag")
panel_raw   <- map_dfr(out, "data")

cat("--- parse status ---\n"); print(as.data.frame(count(diagnostics, status)), row.names = FALSE)
if (any(diagnostics$status != "ok")) {
  cat("\nNon-ok files:\n")
  print(as.data.frame(diagnostics %>% filter(status != "ok") %>%
        select(source_file, status, message)), row.names = FALSE)
}

# ---- entity standardization (shares the B-2343 crosswalk) --------------------
manual <- read_csv(file.path(DOC, "bank_map_manual.csv"), show_col_types = FALSE)
bank_std <- standardize_banks(unique(panel_raw$bank_raw), "excel_B-2322", manual)

panel <- panel_raw %>%
  left_join(bank_std %>% select(bank_raw, bank_standardized, scope_note, bank_footnote,
                                mapping_method, mapping_confidence, review_required),
            by = "bank_raw") %>%
  # has_unused_lines is read off each workbook's actual header, not assumed from
  # a hardcoded date, so the flag stays correct if SBS ever changes the table
  # again. UNUSED_LINES_FROM is asserted against the data below rather than
  # trusted.
  left_join(diagnostics %>% select(source_file, has_unused_lines), by = "source_file") %>%
  mutate(bank_id = bank_standardized,
         definition_regime = ifelse(has_unused_lines,
                                    "post_res_sbs_11356_2008", "pre_res_sbs_11356_2008"))

stopifnot(identical(sort(unique(panel$report_date[panel$has_unused_lines])),
                    sort(unique(panel$report_date[panel$report_date >= UNUSED_LINES_FROM]))))

panel_out <- panel %>%
  select(cuadro, report_year, report_month, report_date, sheet_name, table_title,
         units_label, source_file, source_url, source_sha256, file_format,
         row_index, col_index, bank_raw, bank_standardized, bank_id, scope_note,
         bank_footnote, mapping_method, mapping_confidence, review_required,
         type_raw, type_std, footnote_ref, is_total_row, is_total_col,
         has_unused_lines, definition_regime, value_text, value_num, value_flag) %>%
  arrange(report_date, row_index, col_index)

write_csv(panel_out, file.path(PROC, "b2322_long.csv"))

# ---- crosswalks --------------------------------------------------------------
write_csv(
  panel_out %>% group_by(bank_raw) %>%
    summarise(bank_standardized = first(bank_standardized), bank_id = first(bank_id),
              scope_note = first(scope_note), bank_footnote = first(bank_footnote),
              source = "excel_B-2322", first_seen = min(report_date),
              last_seen = max(report_date), n_months = n_distinct(report_date),
              mapping_method = first(mapping_method),
              mapping_confidence = first(mapping_confidence),
              review_required = first(review_required), .groups = "drop") %>%
    arrange(desc(review_required), bank_standardized, first_seen),
  file.path(PROC, "xwalk_bank_b2322.csv"))

# ==============================================================================
# The deliverable: bank panel of unused credit and its types
# ==============================================================================
totals <- panel_out %>% filter(is_total_col) %>%
  select(report_date, report_year, report_month, bank_raw,
         total_indirect_thousand_pen = value_num, total_text = value_text)

shares <- panel_out %>% filter(!is_total_col) %>%
  select(report_date, bank_raw, type_std, value_num)

wide_share <- shares %>%
  pivot_wider(names_from = type_std, values_from = value_num,
              names_glue = "share_{type_std}_pct")
for (v in paste0("share_", TYPE_ORDER, "_pct"))
  if (!v %in% names(wide_share)) wide_share[[v]] <- NA_real_

meta <- panel_out %>%
  distinct(report_date, bank_raw, bank_standardized, bank_id, scope_note,
           is_total_row, has_unused_lines, definition_regime, review_required,
           file_format, source_file)

unused_panel <- meta %>%
  left_join(totals, by = c("report_date", "bank_raw")) %>%
  left_join(wide_share, by = c("report_date", "bank_raw")) %>%
  mutate(
    # Levels are DERIVED (share/100 * published total), not published directly.
    across(all_of(paste0("share_", TYPE_ORDER, "_pct")),
           ~ .x / 100 * total_indirect_thousand_pen,
           .names = "level_{.col}"),
    share_sum_pct = rowSums(across(all_of(paste0("share_", TYPE_ORDER, "_pct"))), na.rm = TRUE)
  ) %>%
  rename_with(~ str_replace(str_remove(.x, "_pct$"), "^level_share_", "level_"),
              starts_with("level_share_")) %>%
  rename_with(~ paste0(.x, "_thousand_pen"), starts_with("level_")) %>%
  mutate(
    unused_lines_thousand_pen = level_lineas_no_utilizadas_thousand_pen,
    unused_lines_share_pct    = share_lineas_no_utilizadas_pct,
    zero_indirect_bank        = is.na(total_indirect_thousand_pen) | total_indirect_thousand_pen == 0,
    share_sum_dev_pp          = abs(share_sum_pct - 100),
    share_sum_ok              = zero_indirect_bank | share_sum_dev_pp <= 1e-6,
    row_kind                  = ifelse(is_total_row, "system_total", "bank")
  ) %>%
  select(report_date, report_year, report_month, bank_raw, bank_standardized, bank_id,
         scope_note, row_kind, definition_regime, has_unused_lines,
         total_indirect_thousand_pen,
         unused_lines_thousand_pen, unused_lines_share_pct,
         starts_with("level_"), starts_with("share_"),
         share_sum_pct, share_sum_dev_pp, share_sum_ok, zero_indirect_bank,
         review_required, file_format, source_file, total_text) %>%
  arrange(report_date, row_kind, bank_standardized)

# ---- month-level reconciliation: published TOTAL row vs sum of bank rows ------
# SBS publishes both, so they must agree. Where they do not, at least one printed
# bank level is wrong -- and because the type columns are SHARES, a wrong total
# corrupts every derived level for that bank. The gap is carried on every row so
# a derived level can never be used without seeing whether its month reconciles.
#
# Known case: B. ICBC's "Total Creditos Indirectos" cell is a hardcoded 100 from
# its first appearance (2015-06) through 2019-10, becoming a real value only in
# 2019-11. Its SHARES are internally consistent (they sum to 100), so the defect
# is invisible unless the total row is reconciled. Nothing is auto-corrected:
# `implied_total_if_sole_cause` is offered for review only.
month_recon <- unused_panel %>%
  group_by(report_date) %>%
  summarise(
    sum_bank_total_thousand_pen = sum(total_indirect_thousand_pen[row_kind == "bank"], na.rm = TRUE),
    published_system_total_thousand_pen =
      first(total_indirect_thousand_pen[row_kind == "system_total"]),
    .groups = "drop") %>%
  mutate(
    total_gap_thousand_pen = published_system_total_thousand_pen - sum_bank_total_thousand_pen,
    total_reconciles = is.na(total_gap_thousand_pen) | abs(total_gap_thousand_pen) <= 1)

unused_panel <- unused_panel %>%
  left_join(month_recon, by = "report_date") %>%
  mutate(
    # A bank-month is suspect when its month fails to reconcile AND the bank's
    # own printed total is negligible next to the unexplained gap.
    total_suspect = row_kind == "bank" & !total_reconciles &
                    !is.na(total_indirect_thousand_pen) &
                    abs(total_indirect_thousand_pen) < 0.01 * abs(total_gap_thousand_pen),
    implied_total_if_sole_cause = ifelse(total_suspect,
                                         total_indirect_thousand_pen + total_gap_thousand_pen,
                                         NA_real_))

write_csv(month_recon, file.path(VALID, "b2322_month_total_reconciliation.csv"))

write_csv(unused_panel, file.path(PROC, "b2322_unused_credit_panel.csv"))

write_csv(diagnostics, file.path(VALID, "parse_diagnostics_b2322.csv"))

# ==============================================================================
# Manual-review queue
# ==============================================================================
dup_hash <- mf %>% count(sha256, name = "n_months_sharing_file") %>% filter(n_months_sharing_file > 1)

# ---- structural-break / coverage detectors on the target series --------------
# A "-" in the unused-lines column is stored as NA, never as 0: for several banks
# it means "not reported yet" rather than "no unused lines". Reading it as zero
# would bias the level down, so both cases are surfaced here for manual review.
post <- unused_panel %>% filter(row_kind == "bank", has_unused_lines)

# (a) banks that only start reporting unused lines AFTER the column appeared
series_start <- post %>%
  filter(!is.na(unused_lines_thousand_pen)) %>%
  group_by(bank_standardized) %>% summarise(first_unused = min(report_date), .groups = "drop") %>%
  left_join(post %>% filter(!is.na(total_indirect_thousand_pen), total_indirect_thousand_pen > 0) %>%
              group_by(bank_standardized) %>%
              summarise(first_indirect = min(report_date), .groups = "drop"),
            by = "bank_standardized") %>%
  filter(!is.na(first_indirect), first_unused > first_indirect + 40)

# (b) bank-months where the bank reports indirect credit but no unused-lines value
missing_while_active <- post %>%
  filter(!is.na(total_indirect_thousand_pen), total_indirect_thousand_pen > 0,
         is.na(unused_lines_thousand_pen))

# (c) one-month level shifts in the target series (both months present)
shift <- post %>%
  filter(!is.na(unused_lines_thousand_pen)) %>%
  arrange(bank_standardized, report_date) %>%
  group_by(bank_standardized) %>%
  mutate(prev = lag(unused_lines_thousand_pen),
         prev_date = lag(report_date),
         d_abs = unused_lines_thousand_pen - prev,
         d_pct = 100 * (unused_lines_thousand_pen / prev - 1)) %>%
  ungroup() %>%
  filter(!is.na(prev), prev > 0,
         abs(d_pct) >= 50, abs(d_abs) >= 5e5,          # >=50% and >= S/0.5bn
         as.integer(report_date - prev_date) <= 40)    # consecutive months only

sys_shift <- unused_panel %>%
  filter(row_kind == "system_total", !is.na(unused_lines_thousand_pen)) %>%
  arrange(report_date) %>%
  mutate(prev = lag(unused_lines_thousand_pen), prev_date = lag(report_date),
         d_pct = 100 * (unused_lines_thousand_pen / prev - 1)) %>%
  filter(!is.na(prev), abs(d_pct) >= 25, as.integer(report_date - prev_date) <= 40)

review <- bind_rows(
  diagnostics %>% filter(status != "ok") %>%
    transmute(item = "unparsed_or_wrong_workbook", key = source_file,
              detail = message, severity = "high"),
  diagnostics %>% filter(!date_matches_filename) %>%
    transmute(item = "in_file_date_vs_filename", key = source_file,
              detail = paste0("cell=", date_cell_raw, " parsed=", report_date),
              severity = "high"),
  mf %>% filter(sha256 %in% dup_hash$sha256) %>%
    transmute(item = "duplicate_workbook_content", key = basename(dest),
              detail = paste0("sha256 shared with another month: ", sha256),
              severity = "high"),
  read_csv(file.path(PROC, "xwalk_bank_b2322.csv"), show_col_types = FALSE) %>%
    filter(review_required | is.na(bank_standardized)) %>%
    transmute(item = "bank_name_mapping", key = bank_raw,
              detail = paste0("std=", coalesce(bank_standardized, "<none>"), "; ",
                              first_seen, "..", last_seen, "; n=", n_months),
              severity = "high"),
  diagnostics %>% filter(!has_unused_lines) %>%
    transmute(item = "no_unused_lines_column", key = source_file,
              detail = "column absent: pre Res. SBS 11356-2008 definition (before 2010-07)",
              severity = "structural"),
  diagnostics %>% filter(!has_total_row) %>%
    transmute(item = "no_system_total_row", key = source_file,
              detail = "workbook has no TOTAL BANCA MULTIPLE row; system total must be summed from bank rows",
              severity = "medium"),
  unused_panel %>% filter(row_kind == "bank", !zero_indirect_bank, !share_sum_ok) %>%
    transmute(item = "share_sum_off_100", key = paste(bank_raw, report_date),
              detail = sprintf("sum=%.10f dev=%.3e pp", share_sum_pct, share_sum_dev_pp),
              severity = "low"),
  panel_out %>% filter(value_flag == "negative_share") %>%
    transmute(item = "negative_share", key = paste(bank_raw, report_date, type_std),
              detail = paste0("value=", value_text), severity = "low"),
  unused_panel %>% filter(row_kind == "bank", is.na(total_indirect_thousand_pen)) %>%
    transmute(item = "missing_total_indirect", key = paste(bank_raw, report_date),
              detail = paste0("total cell = ", coalesce(total_text, "<blank>"),
                              " (bank reports no indirect credit)"), severity = "low"),
  series_start %>%
    transmute(item = "unused_lines_series_starts_late", key = bank_standardized,
              detail = sprintf("reports indirect credit from %s but unused lines only from %s -- '-' before that means NOT REPORTED, not zero",
                               first_indirect, first_unused),
              severity = "high"),
  missing_while_active %>%
    transmute(item = "unused_lines_missing_while_active", key = paste(bank_raw, report_date),
              detail = sprintf("total indirect = %.3f but unused-lines cell is '-' (treated as NA, not 0)",
                               total_indirect_thousand_pen),
              severity = "medium"),
  shift %>%
    transmute(item = "unused_lines_level_shift_bank", key = paste(bank_standardized, report_date),
              detail = sprintf("%.0f -> %.0f miles S/ (%+.1f%%) in one month; check for a reporting change",
                               prev, unused_lines_thousand_pen, d_pct),
              severity = "high"),
  sys_shift %>%
    transmute(item = "unused_lines_level_shift_system", key = as.character(report_date),
              detail = sprintf("system unused lines %.0f -> %.0f miles S/ (%+.1f%%) in one month",
                               prev, unused_lines_thousand_pen, d_pct),
              severity = "high"),
  month_recon %>% filter(!total_reconciles) %>%
    transmute(item = "system_total_not_reconciling", key = as.character(report_date),
              detail = sprintf("published TOTAL %.3f vs sum of bank rows %.3f -> gap %.3f miles S/",
                               published_system_total_thousand_pen,
                               sum_bank_total_thousand_pen, total_gap_thousand_pen),
              severity = "high"),
  unused_panel %>% filter(total_suspect) %>%
    transmute(item = "bank_total_placeholder_suspect", key = paste(bank_raw, report_date),
              detail = sprintf("printed total = %s but the month's unexplained gap is %.3f; implied total if this bank is the sole cause = %.3f. ALL derived levels for this bank-month are unreliable.",
                               total_text, total_gap_thousand_pen, implied_total_if_sole_cause),
              severity = "high")
)
write_csv(review, file.path(VALID, "b2322_review_queue.csv"))

# ---- summary -----------------------------------------------------------------
banks_only <- unused_panel %>% filter(row_kind == "bank")
cat("\n--- panel ---\n")
cat(sprintf("long rows            : %d\n", nrow(panel_out)))
cat(sprintf("months parsed        : %d (%s .. %s)\n", n_distinct(panel_out$report_date),
            min(panel_out$report_date), max(panel_out$report_date)))
cat(sprintf("bank-months          : %d  (system-total rows: %d)\n",
            nrow(banks_only), sum(unused_panel$row_kind == "system_total")))
cat(sprintf("distinct bank_raw    : %d | unmatched: %d | review_required: %d\n",
            n_distinct(panel_out$bank_raw), sum(is.na(bank_std$bank_standardized)),
            sum(bank_std$review_required, na.rm = TRUE)))
cat(sprintf("type labels          : %d raw -> %d standardized\n",
            n_distinct(panel_out$type_raw), n_distinct(panel_out$type_std)))
cat(sprintf("date != filename     : %d files\n", sum(!diagnostics$date_matches_filename)))
cat(sprintf("duplicate workbooks  : %d\n", nrow(dup_hash)))

cat("\n--- unused credit lines coverage ---\n")
cat(sprintf("months WITHOUT the column : %d (%s .. %s)\n",
            sum(!diagnostics$has_unused_lines),
            min(diagnostics$report_date[!diagnostics$has_unused_lines]),
            max(diagnostics$report_date[!diagnostics$has_unused_lines])))
cat(sprintf("months WITH the column    : %d (%s .. %s)\n",
            sum(diagnostics$has_unused_lines),
            min(diagnostics$report_date[diagnostics$has_unused_lines]),
            max(diagnostics$report_date[diagnostics$has_unused_lines])))
cat(sprintf("bank-months with a non-missing unused-lines level: %d\n",
            sum(!is.na(banks_only$unused_lines_thousand_pen))))

cat("\n--- share-sum identity (bank rows, active banks) ---\n")
act <- banks_only %>% filter(!zero_indirect_bank)
cat(sprintf("exact (<=1e-6 pp): %d of %d (%.4f%%) | max dev: %.3e pp\n",
            sum(act$share_sum_ok), nrow(act), 100 * mean(act$share_sum_ok),
            max(act$share_sum_dev_pp, na.rm = TRUE)))

cat("\n--- published TOTAL row vs sum of bank rows ---\n")
cat(sprintf("months compared: %d | reconciling: %d | NOT reconciling: %d\n",
            sum(!is.na(month_recon$published_system_total_thousand_pen)),
            sum(month_recon$total_reconciles & !is.na(month_recon$published_system_total_thousand_pen)),
            sum(!month_recon$total_reconciles)))
cat(sprintf("max |gap|: %.3f miles S/\n", max(abs(month_recon$total_gap_thousand_pen), na.rm = TRUE)))
cat(sprintf("bank-months with a suspect printed total: %d\n", sum(unused_panel$total_suspect, na.rm = TRUE)))

cat("\n--- review queue ---\n")
print(as.data.frame(review %>% count(item, severity) %>% arrange(desc(severity), desc(n))),
      row.names = FALSE)
cat("\nOutputs written to data/processed/ and output/validation/.\nDone.\n")
