# ==============================================================================
# 02_clean_deposits.R -- parse SBS cuadro B-2358 into a tidy long table and
#   build the BRANCH (oficina) panel of deposits and direct credits.
#   "Depositos y Creditos por Oficina y Empresa Bancaria"
# ==============================================================================
# Everything structural is located by CONTENT ANCHOR, never by a fixed row or
# column index, because the layout drifts across vintages:
#
#   * the sheet NAME is arbitrary -> the sheet is picked by the title anchor
#   * TWO-ROW header: merged group labels ("Depositos a la Vista") on one row,
#     sub-labels ("M.N", "M.E.", "Total") on the next. The group row is
#     fill_right()-ed, then the two rows are pasted into one column key.
#   * the header row is found by the "Empresa" anchor in column 1
#   * columns to the right of the last labelled header column are padding and
#     are dropped. An unmapped column key is a HARD failure for the file:
#     reading an unrecognised column by position would silently mix up MN/ME or
#     deposits/credits.
#   * some vintages print a footnote marker on a header label with no footnote
#     text anywhere on the sheet -> stripped for matching, recorded in the
#     diagnostics rather than silently tolerated
#   * a few months also print a currency split of Total Depositos -> mapped as
#     optional value columns, NA in every month that does not print them
#   * body cells are REPEATED in the early vintages and MERGED in the later
#     ones, so Empresa and the three location columns are fill_down()-ed
#   * foreign branches print departamento = provincia = distrito = "Extranjero";
#     they are kept and flagged is_foreign
#   * where the vintage prints a "Total general" row it is reconciled against
#     the sum of the branch rows. That is the only end-to-end check that the
#     parse is COMPLETE -- a dropped branch block, a double-counted merged cell
#     or a mis-mapped value column all show up there -- so it runs every time.
#
# THE BRANCH KEY is bank_standardized + codigo_oficina. "Codigo Oficina" is
# per-bank, not global (the same code is used by several banks in a month), so
# the bank must be part of the key. Duplicate (bank, codigo, month) rows are
# SUMMED, never dropped, and listed in the review queue. Location is carried as
# a TIME-VARYING attribute: codes outlive relocations and naming changes, so a
# change is flagged (location_changed), never resolved automatically.
#
# Units: all monetary columns are levels in miles de S/.
#
# COVERAGE CAVEAT, printed to the log rather than left for the analyst to
# rediscover: the SBS note on this table reads "Informacion obtenida del Anexo
# N. 10: Depositos, Colocaciones y Personal por Oficina" and, in several
# vintages, "No se incluyen los Depositos judiciales y administrativos".
# Branch deposits summed to bank level therefore need NOT equal a bank's
# published total deposits.
#
# Outputs:
#   data/processed/b2358_long.csv.gz          cell-level long table
#   data/processed/b2358_branch_panel.csv.gz  the deliverable: branch x month
#   data/processed/xwalk_bank_b2358.csv       raw printed name -> standardized
#   output/validation/parse_diagnostics_b2358.csv
#   output/validation/b2358_month_total_reconciliation.csv
#   output/validation/b2358_review_queue.csv
# Both data tables are gzipped (readr and data.table read .csv.gz directly):
# the long table is ~2 GB and the panel ~115 MB as plain CSV. Per-workbook
# constants (sheet name, units label, source URL, sha256) are NOT repeated on
# every row -- they live once per file in the diagnostics and in the manifest,
# joinable on source_file. Neither table carries the bank-name mapping
# metadata: that is a property of the printed NAME, not of a branch-month, and
# lives once per spelling in the crosswalk.
#
# Usage:  Rscript code/02_clean_deposits.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble)
})
source("code/00_sbs_utils.R")

CODE     <- "B-2358"
MANIFEST <- "data/raw/sbs_boletin/manifest_B-2358.csv"
PROC     <- "data/processed"
VALID    <- "output/validation"
DOC      <- "data/documentation"
LOG      <- "output/logs/02_clean_deposits.log"
for (d in c(PROC, VALID, dirname(LOG))) dir.create(d, recursive = TRUE, showWarnings = FALSE)

con <- file(LOG, open = "wt"); sink(con, split = TRUE); on.exit({sink(); close(con)}, add = TRUE)
cat("=== 02_clean_deposits.R ===\n", format(Sys.time()), "\n\n")

# ---- anchors -----------------------------------------------------------------
TITLE_RX   <- "^DEPOSITOS Y CREDITOS POR OFICINA Y EMPRESA BANCARIA"
UNITS_RX   <- "^\\(\\s*EN MILES DE (NUEVOS )?SOLES"
HDR_RX     <- "^EMPRESAS?$"
# First column value that ends the data block. "Total general" is the system
# total row; the rest are notes and footnotes.
ENDROW_RX  <- "^(TOTAL GENERAL|TOTAL\\b|NOTA|N O T A|FUENTE|NO SE INCLUYEN|\\*|[0-9]+/)"
TOTGEN_RX  <- "^TOTAL GENERAL"
FOREIGN_RX <- "^EXTRANJERO$"

# ---- column standardization --------------------------------------------------
# Key = "<group label>|<sub label>" on the accent-stripped, uppercased header,
# with the group row filled right first.
COL_MAP <- tribble(
  ~key,                              ~field,             ~role,
  "EMPRESA|",                        "empresa",          "id",
  "UBICACION|DEPARTAMENTO",          "departamento",     "id",
  "UBICACION|PROVINCIA",             "provincia",        "id",
  "UBICACION|DISTRITO",              "distrito",         "id",
  "CODIGO OFICINA|",                 "codigo_oficina",   "id",
  "DEPOSITOS A LA VISTA|M.N",        "dep_vista_mn",     "value",
  "DEPOSITOS A LA VISTA|M.E.",       "dep_vista_me",     "value",
  "DEPOSITOS A LA VISTA|TOTAL",      "dep_vista_total",  "value",
  "DEPOSITOS DE AHORRO|M.N",         "dep_ahorro_mn",    "value",
  "DEPOSITOS DE AHORRO|M.E.",        "dep_ahorro_me",    "value",
  "DEPOSITOS DE AHORRO|TOTAL",       "dep_ahorro_total", "value",
  "DEPOSITOS A PLAZO|M.N",           "dep_plazo_mn",     "value",
  "DEPOSITOS A PLAZO|M.E.",          "dep_plazo_me",     "value",
  "DEPOSITOS A PLAZO|TOTAL",         "dep_plazo_total",  "value",
  "TOTAL DEPOSITOS|",                "dep_total",        "value",
  # A couple of months also print a currency split of Total Depositos, under a
  # SECOND "Total Depositos" group label. Carried rather than discarded, but
  # optional: most months do not have them.
  "TOTAL DEPOSITOS|M.N",             "dep_total_mn",     "value_optional",
  "TOTAL DEPOSITOS|M.E.",            "dep_total_me",     "value_optional",
  "CREDITOS DIRECTOS|M.N",           "cred_directos_mn", "value",
  "CREDITOS DIRECTOS|M.E.",          "cred_directos_me", "value",
  "TOTAL CREDITOS|",                 "cred_total",       "value"
)
REQUIRED_VALUE_FIELDS <- COL_MAP$field[COL_MAP$role == "value"]
VALUE_FIELDS <- COL_MAP$field[COL_MAP$role %in% c("value", "value_optional")]
ID_FIELDS    <- COL_MAP$field[COL_MAP$role == "id"]
LOC_FIELDS   <- c("departamento", "provincia", "distrito")

#' Strip footnote markers from a header label before it is used as a lookup key.
#' The marker carries no information but changes the string, so it is extracted
#' and recorded rather than silently tolerated.
clean_hdr_label <- function(x) {
  x <- norm_txt(x)
  marks <- str_extract_all(replace_na(x, ""), "[0-9]+/|\\*+")
  key   <- str_squish(str_replace_all(x, "[0-9]+/|\\*+", ""))
  list(key = key, marks = unique(unlist(marks)))
}

# ---- per-file parser ---------------------------------------------------------
parse_one <- function(path, report_year, report_month, file_format) {
  fname <- basename(path)
  diag <- tibble(
    source_file = fname, report_year = report_year, report_month = report_month,
    status = "ok", message = NA_character_, sheet_name = NA_character_,
    title_row = NA_integer_, header_row = NA_integer_,
    units_label = NA_character_, date_cell_raw = NA_character_,
    report_date = as.Date(NA), date_matches_filename = NA,
    n_cols = NA_integer_, has_codigo = NA, has_total_general = NA,
    header_footnotes = NA_character_, n_optional_value_cols = 0L,
    n_rows = 0L, n_banks = 0L, n_blank_codigo = 0L, n_foreign = 0L,
    n_cells = 0L, n_cells_missing = 0L, n_cells_unparsed = 0L
  )
  fail <- function(msg, status = "error") { diag$status <- status; diag$message <- msg
                                            list(diag = diag, data = NULL, total = NULL) }

  pick <- try(sbs_pick_sheet(path, TITLE_RX), silent = TRUE)
  if (inherits(pick, "try-error"))
    return(fail(paste0("table not found: ", str_squish(as.character(pick))), "wrong_table"))
  g <- pick$grid; diag$sheet_name <- pick$sheet; diag$n_cols <- ncol(g)

  diag$title_row <- sbs_find_cells(g, TITLE_RX)$row[1]

  ur <- sbs_find_cells(g, UNITS_RX)
  if (!nrow(ur)) return(fail("units row '(En miles de soles)' not found"))
  diag$units_label <- str_squish(ur$value[1])

  # The date cell sits between the title and the units label, in column 1.
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
  if (!nrow(hd)) return(fail("header anchor 'Empresa' not found"))
  hrow <- hd$row[1]; diag$header_row <- hrow

  # Two-row header: fill the merged group labels right, then pair with the
  # sub-label row.
  grp_c <- clean_hdr_label(unlist(g[hrow, ], use.names = FALSE))
  sub_c <- clean_hdr_label(unlist(g[hrow + 1L, ], use.names = FALSE))
  marks <- unique(c(grp_c$marks, sub_c$marks))
  diag$header_footnotes <- if (length(marks)) paste(marks, collapse = " ") else NA_character_
  # str_squish() turns an empty cell into "", which would defeat fill_right().
  grp <- fill_right(na_if(grp_c$key, ""))
  sub <- na_if(sub_c$key, "")
  # A group label only spans the columns it was merged over; anything to the
  # right of the last labelled column is padding, not a column of this table.
  last_lab <- suppressWarnings(max(which(!is.na(grp))))
  if (!is.finite(last_lab)) return(fail("header row is empty"))
  keys <- paste0(replace_na(grp, ""), "|", replace_na(sub, ""))[seq_len(last_lab)]

  cols <- tibble(col_index = seq_len(last_lab), key = keys) %>%
    left_join(COL_MAP, by = "key")
  if (any(is.na(cols$field)))
    return(fail(paste0("unmapped column key(s): ",
                       paste(unique(cols$key[is.na(cols$field)]), collapse = " | "))))
  if (!all(REQUIRED_VALUE_FIELDS %in% cols$field))
    return(fail(paste0("missing value column(s): ",
                       paste(setdiff(REQUIRED_VALUE_FIELDS, cols$field), collapse = ", "))))
  diag$n_optional_value_cols <- sum(cols$role == "value_optional", na.rm = TRUE)
  diag$has_codigo <- "codigo_oficina" %in% cols$field
  if (!diag$has_codigo)
    return(fail("no 'Codigo Oficina' column -- no branch identifier in this vintage",
                "no_branch_id"))

  cidx <- setNames(cols$col_index, cols$field)

  # ---- body block ------------------------------------------------------------
  first <- norm_txt(g[[1]])
  body  <- seq.int(hrow + 2L, nrow(g))
  end_at <- body[!is.na(first[body]) & str_detect(first[body], ENDROW_RX)]
  last_row <- if (length(end_at)) min(end_at) - 1L else nrow(g)

  totgen_row <- body[!is.na(first[body]) & str_detect(first[body], TOTGEN_RX)]
  diag$has_total_general <- length(totgen_row) > 0L

  body <- body[body <= last_row]
  if (!length(body)) return(fail("empty data block"))

  ids <- tibble(row_index = body)
  for (f in ID_FIELDS) ids[[f]] <- str_squish(as.character(g[[cidx[[f]]]])[body])
  # Merged / repeated labels: both vintages collapse to the same fill-down.
  for (f in c("empresa", LOC_FIELDS)) ids[[f]] <- fill_down(ids[[f]])
  ids$codigo_oficina <- na_if(ids$codigo_oficina, "")

  # Rows with no bank at all are padding below the last block, not data.
  ids <- ids[!is.na(ids$empresa), ]
  if (!nrow(ids)) return(fail("no bank rows in data block"))
  ids$is_foreign <- str_detect(replace_na(norm_txt(ids$departamento), ""), FOREIGN_RX)

  diag$n_rows         <- nrow(ids)
  diag$n_banks        <- n_distinct(ids$empresa)
  diag$n_blank_codigo <- sum(is.na(ids$codigo_oficina))
  diag$n_foreign      <- sum(ids$is_foreign)

  present_values <- intersect(VALUE_FIELDS, cols$field)
  long <- map_dfr(present_values, function(f) {
    txt <- as.character(g[[cidx[[f]]]])[ids$row_index]
    tibble(row_index = ids$row_index, col_index = cidx[[f]],
           bank_raw = ids$empresa, codigo_oficina = ids$codigo_oficina,
           departamento = ids$departamento, provincia = ids$provincia,
           distrito = ids$distrito, is_foreign = ids$is_foreign,
           field = f, value_text = txt, value_num = sbs_num(txt))
  })

  blankish <- is.na(long$value_text) | str_squish(long$value_text) %in% c("", "-", "–", "—")
  diag$n_cells          <- nrow(long)
  diag$n_cells_missing  <- sum(blankish)
  diag$n_cells_unparsed <- sum(!blankish & is.na(long$value_num))

  long <- long %>% mutate(
    cuadro = CODE, report_year = report_year, report_month = report_month,
    report_date = diag$report_date, source_file = fname, file_format = file_format,
    value_flag = case_when(
      blankish         ~ "missing_marker",
      is.na(value_num) ~ "unparsed",
      value_num < 0    ~ "negative_level",
      TRUE             ~ "ok"))

  # ---- published system total, where the vintage prints one ------------------
  total <- NULL
  if (diag$has_total_general) {
    tr1 <- min(totgen_row)
    total <- tibble(report_date = diag$report_date, source_file = fname,
                    field = present_values,
                    published_total = vapply(present_values,
                      function(f) sbs_num(as.character(g[[cidx[[f]]]])[tr1]), numeric(1),
                      USE.NAMES = FALSE))
  }

  list(diag = diag, data = long, total = total)
}

# ---- run ---------------------------------------------------------------------
mf <- read_csv(MANIFEST, show_col_types = FALSE) %>% filter(available)
cat(sprintf("Parsing %d workbooks...\n\n", nrow(mf)))
out <- map(seq_len(nrow(mf)), function(i)
  parse_one(mf$dest[i], mf$report_year[i], mf$report_month[i], mf$detected_format[i]))

diagnostics <- map_dfr(out, "diag")
panel_raw   <- map_dfr(out, "data")
totals_pub  <- map_dfr(out, "total")

cat("--- parse status ---\n"); print(as.data.frame(count(diagnostics, status)), row.names = FALSE)
if (any(diagnostics$status != "ok")) {
  cat("\nNon-ok files:\n")
  print(as.data.frame(diagnostics %>% filter(status != "ok") %>%
        select(source_file, status, message)), row.names = FALSE)
}
stopifnot(nrow(panel_raw) > 0)

# ---- entity standardization (shares the B-2343 / B-2322 crosswalk) -----------
# B-2358 prints entity names TRUNCATED TO 20 CHARACTERS ("B B V A BANCO CONTIN",
# "CREDITO", "BANCOM2"), unlike the other cuadros. The extra spellings live in
# the same manual map; anything still unmatched lands in the review queue rather
# than becoming a silent NA.
manual   <- read_csv(file.path(DOC, "bank_map_manual.csv"), show_col_types = FALSE)
bank_std <- standardize_banks(unique(panel_raw$bank_raw), "excel_B-2358", manual)

panel_out <- panel_raw %>%
  left_join(bank_std %>% select(bank_raw, bank_standardized, scope_note, bank_footnote,
                                mapping_method, mapping_confidence, review_required),
            by = "bank_raw") %>%
  # No separate bank_id: the B-2343 and B-2322 cleaners carry one, but it is a
  # verbatim copy of bank_standardized in both. Join to those panels on
  # bank_standardized.
  select(cuadro, report_year, report_month, report_date, source_file, file_format,
         row_index, col_index, bank_raw, bank_standardized,
         codigo_oficina, departamento, provincia, distrito, is_foreign,
         scope_note, bank_footnote, mapping_method, mapping_confidence,
         review_required, field, value_text, value_num, value_flag) %>%
  arrange(report_date, row_index, col_index)

write_csv(panel_out, file.path(PROC, "b2358_long.csv.gz"))

# ---- crosswalk: one row per printed spelling ---------------------------------
xwalk <- panel_raw %>%
  group_by(bank_raw) %>%
  summarise(first_seen = min(report_date), last_seen = max(report_date),
            n_months = n_distinct(report_date), .groups = "drop") %>%
  left_join(bank_std, by = "bank_raw") %>%
  transmute(bank_raw, bank_standardized, scope_note, bank_footnote,
            source = "excel_B-2358", first_seen, last_seen, n_months,
            mapping_method, mapping_confidence, review_required) %>%
  arrange(desc(review_required), bank_standardized, first_seen)
write_csv(xwalk, file.path(PROC, "xwalk_bank_b2358.csv"))

# ==============================================================================
# The deliverable: branch panel
# ==============================================================================
wide <- panel_out %>%
  select(report_date, report_year, report_month, row_index, bank_raw,
         bank_standardized, codigo_oficina, departamento, provincia,
         distrito, is_foreign, file_format, source_file,
         field, value_num) %>%
  pivot_wider(names_from = field, values_from = value_num)

# An optional field that no collected month carries would otherwise be absent
# from the panel entirely; it is created as NA so the schema does not depend on
# which months happened to be collected.
for (v in VALUE_FIELDS) if (!v %in% names(wide)) wide[[v]] <- NA_real_

# Rows without a codigo cannot be placed on a branch: kept out of the panel and
# surfaced in the review queue, not dropped silently.
no_code <- wide %>% filter(is.na(codigo_oficina))
wide    <- wide %>% filter(!is.na(codigo_oficina))

# Duplicate (bank, codigo, month) keys are SUMMED, never dropped: where this
# fires one row is a zero placeholder and the other carries the balances, so the
# sum is the branch. Flagged in the review queue either way.
dup_keys <- wide %>%
  count(bank_standardized, codigo_oficina, report_date, name = "n_source_rows") %>%
  filter(n_source_rows > 1)

branch_panel <- wide %>%
  group_by(bank_standardized, codigo_oficina, report_date) %>%
  summarise(
    report_year = first(report_year), report_month = first(report_month),
    bank_raw = first(bank_raw),
    departamento = first(departamento), provincia = first(provincia),
    distrito = first(distrito), is_foreign = any(is_foreign),
    across(all_of(VALUE_FIELDS), ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(branch_id = paste(bank_standardized, codigo_oficina, sep = "_"))

# ---- published-identity checks, internal to a single printed row -------------
# Computed every run but deliberately NOT written to the panel: they hold at
# machine precision, so as columns they would be six fields of zeros. Kept as a
# side table so that a month which DOES break an identity still reaches the
# review queue and the log instead of passing silently.
identity_checks <- branch_panel %>%
  transmute(
    branch_id, report_date,
    dep_total_dev     = dep_total - (dep_vista_total + dep_ahorro_total + dep_plazo_total),
    cred_total_dev    = cred_total - (cred_directos_mn + cred_directos_me),
    # Only defined for the months that print the currency split.
    dep_total_cur_dev = dep_total - (dep_total_mn + dep_total_me)) %>%
  mutate(
    dep_total_ok     = is.na(dep_total_dev)     | abs(dep_total_dev)     <= 1e-3,
    cred_total_ok    = is.na(cred_total_dev)    | abs(cred_total_dev)    <= 1e-3,
    dep_total_cur_ok = is.na(dep_total_cur_dev) | abs(dep_total_cur_dev) <= 1e-3)

# ---- location changes over the life of a branch ------------------------------
loc_hist <- branch_panel %>%
  arrange(branch_id, report_date) %>%
  group_by(branch_id) %>%
  transmute(report_date,
            loc = paste(departamento, provincia, distrito, sep = " / "),
            prev_loc = lag(loc),
            location_changed = !is.na(prev_loc) & loc != prev_loc) %>%
  ungroup()
branch_panel <- branch_panel %>%
  left_join(loc_hist %>% select(branch_id, report_date, location_changed),
            by = c("branch_id", "report_date"))

# ---- month reconciliation: published Total general vs sum of branch rows -----
# The verdict is NOT joined onto branch_panel. It is a month-level fact, and as
# a column on every row it would also read as "verified" for the months whose
# workbooks print no Total general row at all, where the flag is true only
# vacuously. Use b2358_month_total_reconciliation.csv, where a month with
# nothing to compare has published_total = NA and is visibly distinct from one
# that was checked.
month_recon <- branch_panel %>%
  group_by(report_date) %>%
  summarise(across(all_of(VALUE_FIELDS), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"),
            n_branches = n(), .groups = "drop") %>%
  pivot_longer(starts_with("sum_"), names_to = "field", values_to = "sum_branches") %>%
  mutate(field = str_remove(field, "^sum_")) %>%
  left_join(totals_pub %>% select(report_date, field, published_total),
            by = c("report_date", "field")) %>%
  mutate(gap = published_total - sum_branches,
         rel_gap = ifelse(is.na(published_total) | abs(published_total) == 0, NA_real_,
                          gap / abs(published_total)),
         reconciles = is.na(published_total) | abs(replace_na(rel_gap, 0)) <= 1e-9)

month_ok <- month_recon %>% group_by(report_date) %>%
  summarise(n_cols_compared = sum(!is.na(published_total)),
            month_reconciles = all(reconciles),
            max_abs_rel_gap = suppressWarnings(max(abs(rel_gap), na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(max_abs_rel_gap = ifelse(is.finite(max_abs_rel_gap), max_abs_rel_gap, NA_real_),
         # TRUE only where there was actually something to compare.
         month_verified = n_cols_compared > 0L & month_reconciles)

branch_panel <- branch_panel %>%
  select(report_date, report_year, report_month, branch_id, bank_raw,
         bank_standardized, codigo_oficina,
         departamento, provincia, distrito, is_foreign,
         all_of(VALUE_FIELDS),
         location_changed) %>%
  arrange(report_date, bank_standardized, codigo_oficina)

# The panel key must hold. A break here means the branch grain is not what the
# rest of the project assumes, so it stops the run.
stopifnot(nrow(branch_panel) ==
          n_distinct(paste(branch_panel$branch_id, branch_panel$report_date)))

# The panel carries no source_file / file_format column: exactly one workbook
# feeds each month, so both are functions of report_date. Recover them from
# parse_diagnostics_b2358.csv or the manifest, which also carry the URL, sha256
# and detected container format.
write_csv(branch_panel, file.path(PROC, "b2358_branch_panel.csv.gz"))
write_csv(month_recon %>% left_join(month_ok, by = "report_date"),
          file.path(VALID, "b2358_month_total_reconciliation.csv"))
write_csv(diagnostics, file.path(VALID, "parse_diagnostics_b2358.csv"))

# ==============================================================================
# Manual-review queue
# ==============================================================================
dup_hash <- mf %>% count(sha256, name = "n_months_sharing_file") %>%
  filter(n_months_sharing_file > 1)

# Branches that disappear and come back. A closure would be a trailing gap; an
# interior gap is either a reporting lapse or a reused code.
span <- branch_panel %>% distinct(report_date) %>% arrange(report_date) %>%
  mutate(t = row_number())
gaps <- branch_panel %>% select(branch_id, report_date) %>%
  left_join(span, by = "report_date") %>%
  arrange(branch_id, t) %>% group_by(branch_id) %>%
  summarise(first_t = min(t), last_t = max(t), n_obs = n(), .groups = "drop") %>%
  mutate(n_missing_interior = last_t - first_t + 1L - n_obs) %>%
  filter(n_missing_interior > 0)

loc_changes <- branch_panel %>% filter(location_changed) %>%
  left_join(loc_hist %>% select(branch_id, report_date, prev_loc, loc),
            by = c("branch_id", "report_date"))

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
  xwalk %>% filter(review_required | is.na(bank_standardized)) %>%
    transmute(item = "bank_name_mapping", key = bank_raw,
              detail = paste0("std=", coalesce(bank_standardized, "<none>"), "; ",
                              first_seen, "..", last_seen, "; n=", n_months),
              severity = "high"),
  no_code %>%
    transmute(item = "blank_codigo_oficina", key = paste(bank_raw, report_date),
              detail = paste0(departamento, " / ", provincia, " / ", distrito,
                              " -- row has no branch identifier and is EXCLUDED from the panel"),
              severity = "high"),
  dup_keys %>%
    transmute(item = "duplicate_bank_codigo_in_month",
              key = paste(bank_standardized, codigo_oficina, report_date),
              detail = sprintf("%d printed rows share this key; their values were SUMMED into one branch-month",
                               n_source_rows),
              severity = "high"),
  identity_checks %>% filter(!dep_total_ok) %>%
    transmute(item = "dep_total_identity_off", key = paste(branch_id, report_date),
              detail = sprintf("Total Depositos - (vista+ahorro+plazo) = %.6f miles S/", dep_total_dev),
              severity = "medium"),
  identity_checks %>% filter(!dep_total_cur_ok) %>%
    transmute(item = "dep_total_currency_identity_off", key = paste(branch_id, report_date),
              detail = sprintf("Total Depositos - (MN+ME) = %.6f miles S/", dep_total_cur_dev),
              severity = "medium"),
  diagnostics %>% filter(!is.na(header_footnotes)) %>%
    transmute(item = "header_footnote_marker", key = source_file,
              detail = paste0("marker(s) ", header_footnotes,
                              " on a column header, with no footnote text on the sheet; ",
                              "stripped for column matching and recorded here"),
              severity = "low"),
  diagnostics %>% filter(n_optional_value_cols > 0) %>%
    transmute(item = "extra_value_columns", key = source_file,
              detail = sprintf("%d column(s) beyond the standard table (Total Depositos split MN/ME); carried in the panel, NA in every other month",
                               n_optional_value_cols),
              severity = "low"),
  month_ok %>% filter(n_cols_compared > 0L, !month_reconciles) %>%
    transmute(item = "total_general_not_reconciling", key = as.character(report_date),
              detail = sprintf("branch sums vs published Total general: max |relative gap| = %.3e across %d value columns",
                               max_abs_rel_gap, n_cols_compared),
              severity = "high"),
  identity_checks %>% filter(!cred_total_ok) %>%
    transmute(item = "cred_total_identity_off", key = paste(branch_id, report_date),
              detail = sprintf("Total Creditos - (MN+ME) = %.6f miles S/", cred_total_dev),
              severity = "medium"),
  loc_changes %>%
    transmute(item = "branch_location_changed", key = paste(branch_id, report_date),
              detail = sprintf("%s -> %s; the code still identifies the branch, location is time-varying",
                               prev_loc, loc),
              severity = "medium"),
  panel_out %>% filter(value_flag == "negative_level") %>%
    transmute(item = "negative_level", key = paste(bank_raw, codigo_oficina, report_date, field),
              detail = paste0("value=", value_text), severity = "low"),
  panel_out %>% filter(value_flag == "unparsed") %>%
    transmute(item = "unparsed_cell", key = paste(bank_raw, codigo_oficina, report_date, field),
              detail = paste0("cell=", value_text), severity = "low"),
  gaps %>%
    transmute(item = "branch_series_interior_gap", key = branch_id,
              detail = sprintf("%d month(s) missing between first and last appearance (%d observed)",
                               n_missing_interior, n_obs),
              severity = "low")
)
write_csv(review, file.path(VALID, "b2358_review_queue.csv"))

# ---- summary -----------------------------------------------------------------
cat("\n--- panel ---\n")
cat(sprintf("long rows            : %d (b2358_long.csv.gz)\n", nrow(panel_out)))
cat(sprintf("months parsed        : %d (%s .. %s)\n", n_distinct(panel_out$report_date),
            min(panel_out$report_date), max(panel_out$report_date)))
cat(sprintf("branch-months        : %d | distinct branches: %d\n",
            nrow(branch_panel), n_distinct(branch_panel$branch_id)))
per_month <- as.integer(table(branch_panel$report_date))
cat(sprintf("branches per month   : min %d, median %d, max %d\n",
            min(per_month), as.integer(median(per_month)), max(per_month)))
cat(sprintf("foreign branch-months: %d\n", sum(branch_panel$is_foreign)))
cat(sprintf("distinct bank_raw    : %d | unmatched: %d | review_required: %d\n",
            n_distinct(panel_out$bank_raw), sum(is.na(bank_std$bank_standardized)),
            sum(bank_std$review_required, na.rm = TRUE)))
cat(sprintf("date != filename     : %d files\n", sum(!diagnostics$date_matches_filename)))
cat(sprintf("duplicate workbooks  : %d\n", nrow(dup_hash)))

cat("\n--- identifier validity: bank_standardized + codigo_oficina ---\n")
cat(sprintf("blank codigo (excluded from panel)            : %d rows\n", nrow(no_code)))
cat(sprintf("duplicate (bank, codigo, month) keys (summed) : %d\n", nrow(dup_keys)))
cat(sprintf("branch-months where printed location changes  : %d over %d branches\n",
            sum(branch_panel$location_changed, na.rm = TRUE),
            n_distinct(loc_changes$branch_id)))
cat(sprintf("branches with an interior gap in their series : %d\n", nrow(gaps)))

cat("\n--- published Total general vs sum of branch rows ---\n")
cat(sprintf("months printing a Total general row : %d of %d\n",
            sum(month_ok$n_cols_compared > 0L), nrow(month_ok)))
cat(sprintf("  reconciling: %d | NOT reconciling: %d | worst |relative gap|: %.3e\n",
            sum(month_ok$month_verified),
            sum(month_ok$n_cols_compared > 0L & !month_ok$month_reconciles),
            suppressWarnings(max(month_ok$max_abs_rel_gap, na.rm = TRUE))))
cat(sprintf("months with NO Total general row (nothing to compare): %d\n",
            sum(month_ok$n_cols_compared == 0L)))

cat("\n--- internal identities (checked every run; not persisted in the panel) ---\n")
cat(sprintf("Total Depositos = vista+ahorro+plazo : %d of %d\n",
            sum(identity_checks$dep_total_ok), nrow(identity_checks)))
cat(sprintf("Total Creditos  = MN+ME              : %d of %d\n",
            sum(identity_checks$cred_total_ok), nrow(identity_checks)))
n_cur <- sum(!is.na(identity_checks$dep_total_cur_dev))
cat(sprintf("Total Depositos = MN+ME              : %d of %d (where printed)\n",
            sum(identity_checks$dep_total_cur_ok & !is.na(identity_checks$dep_total_cur_dev)),
            n_cur))
cat(sprintf("worst |residual| across all three    : %.3e miles S/\n",
            max(abs(c(identity_checks$dep_total_dev, identity_checks$cred_total_dev,
                      identity_checks$dep_total_cur_dev)), na.rm = TRUE)))

cat("\n--- coverage caveat ---\n")
cat("SBS note on this table: figures come from Anexo N. 10 and, in several\n")
cat("vintages, EXCLUDE 'Depositos judiciales y administrativos'. Branch deposits\n")
cat("summed to bank level therefore need NOT equal published bank deposits.\n")

cat("\n--- review queue ---\n")
print(as.data.frame(review %>% count(item, severity) %>% arrange(desc(severity), desc(n))),
      row.names = FALSE)
cat("\nOutputs written to data/processed/ and output/validation/.\nDone.\n")
