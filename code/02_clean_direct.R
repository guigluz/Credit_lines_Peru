# ==============================================================================
# 02_clean_direct.R -- Parse SBS cuadro B-2343 into a tidy long panel
#   "Estructura de los Creditos Directos por Modalidad"
# ==============================================================================
# Structure is located by CONTENT ANCHORS, never by fixed row/column indices.
# Every structural assumption below was verified by scanning all 306 workbooks,
# not inferred from a sample:
#
#   * title row is 1 or 2       -> found by regex, not assumed
#   * header row is 5 or 6      -> found by the "EMPRESAS" anchor
#   * sheet name takes 10+ values (32, Hoja1, P032P, ESTR2, 33-Credt, ...)
#   * 2001 files carry NO "TOTAL BANCA MULTIPLE" row
#   * column ORDER changes (Factoring moves between 2009 and 2013)
#   * units label changed "nuevos soles" -> "soles"; 2001 says "( En porcentajes )"
#   * Nov-2010 serves the WRONG CUADRO (B-2389, transposed) under the B-2343 URL
#   * Jan/Apr-2024 headers read "BANCOM Exterior": SBS find-replaced
#     "Comercio"->"BANCOM" when renaming Banco de Comercio and corrupted the
#     modality header. Repaired in the modality crosswalk and flagged.
#
# Units: the table is (En porcentaje). Columns B..J are percentage SHARES of
# each bank's direct credit; ONLY the last column is a level, in miles de S/.
#
# Usage:  Rscript code/02_clean_direct.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble)
})
source("code/00_sbs_utils.R")

CODE      <- "B-2343"
MANIFEST  <- "data/raw/sbs_boletin/manifest_B-2343.csv"
PROC      <- "data/processed"
VALID     <- "output/validation"
DOC       <- "data/documentation"
LOG       <- "output/logs/02_clean_direct.log"
for (d in c(PROC, VALID, DOC, dirname(LOG))) dir.create(d, recursive = TRUE, showWarnings = FALSE)

con <- file(LOG, open = "wt"); sink(con, split = TRUE); on.exit({sink(); close(con)}, add = TRUE)
cat("=== 02_clean_direct.R ===\n", format(Sys.time()), "\n\n")

# ---- anchors -----------------------------------------------------------------
# Must require "POR MODALIDAD": the Nov-2010 URL serves B-2389 "...SEGUN TIPO Y
# MODALIDAD", whose title also begins "ESTRUCTURA DE LOS CREDITOS DIRECTOS".
# Without the tighter anchor that file would be silently mis-parsed.
TITLE_RX  <- "^ESTRUCTURA DE LOS CREDITOS DIRECTOS POR MODALIDAD"
UNITS_RX  <- "^\\(\\s*EN PORCENTAJE"
HDR_RX    <- "^EMPRESAS$"
TOTROW_RX <- "^TOTAL(\\s|$)"
NOTE_RX   <- "^(NOTA|FUENTE|N O T A|\\*|[0-9]+/)"

# ---- modality standardization ------------------------------------------------
# Keys are norm_txt() of the raw label with footnote markers stripped.
MODALITY_MAP <- tribble(
  ~key,                          ~modality_std,               ~modality_label_en,
  "CUENTAS CORRIENTES",          "cuentas_corrientes",        "Current accounts (overdrafts)",
  "TARJETAS DE CREDITO",         "tarjetas_credito",          "Credit cards",
  "DESCUENTOS",                  "descuentos",                "Discounts",
  "FACTORING",                   "factoring",                 "Factoring",
  "PRESTAMOS",                   "prestamos",                 "Loans",
  "ARRENDAMIENTO FINANCIERO",    "arrendamiento_financiero",  "Financial leasing",
  "HIPOTECARIOS PARA VIVIENDA",  "hipotecarios_vivienda",     "Residential mortgages",
  "COMERCIO EXTERIOR",           "comercio_exterior",         "Trade finance",
  "OTROS",                       "otros",                     "Other",
  "TOTAL CREDITOS",              "__TOTAL__",                 "Total direct credit (level)"
)

# Definitional caveats for these modalities (see the header block for context):
#   tarjetas_credito         : Res. SBS 5570-2019 -- from Jan-2021 non-revolving
#                              financings for card holders move to 'prestamos';
#                              the series is NOT comparable across Jan-2021.
#   prestamos                : from Jan-2021 absorbs those card financings.
#   comercio_exterior        : first appears 2003-11, so pre-2003-11 blanks are
#                              NOT zeros. Label corrupted to "BANCOM Exterior" in
#                              2024-01 and 2024-04 by an SBS find-replace when
#                              Banco de Comercio was renamed BANCOM; repaired in
#                              clean_modality_label().
#   descuentos               : recorded at net disbursed amount (footnote 1/).
#   arrendamiento_financiero : includes lease-back, net of unearned income (2/).
#   otros                    : residual -- creditos por liquidar, pignoraticios,
#                              structured finance, etc.
#   __TOTAL__                : LEVEL column, miles de S/. Units label changed
#                              "nuevos soles" -> "soles".

#' Strip footnote markers and repair known SBS label corruption.
#' Returns a list(key, footnote_ref, repaired).
clean_modality_label <- function(raw) {
  x <- norm_txt(raw)
  fn <- str_extract_all(x, "[0-9]+/|\\*")[[1]]
  x <- str_squish(str_replace_all(x, "[0-9]+/|\\*", ""))
  # SBS find-replace accident: "Comercio Exterior" -> "BANCOM Exterior" (2024-01, 2024-04)
  repaired <- FALSE
  if (str_detect(x, "^BANCOM EXTERIOR$")) { x <- "COMERCIO EXTERIOR"; repaired <- TRUE }
  # "TOTAL CREDITOS (EN MILES DE [NUEVOS ]SOLES)" -> "TOTAL CREDITOS"
  if (str_detect(x, "^TOTAL\\s+CREDITOS")) x <- "TOTAL CREDITOS"
  list(key = x, footnote_ref = paste(fn, collapse = " "), repaired = repaired)
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
    n_banks = 0L, n_modalities = 0L, has_total_row = NA,
    n_cells = 0L, n_cells_missing = 0L, n_cells_unparsed = 0L,
    label_repairs = 0L
  )
  fail <- function(msg, status = "error") { diag$status <- status; diag$message <- msg; list(diag = diag, data = NULL) }

  sheets <- try(readxl::excel_sheets(path), silent = TRUE)
  if (inherits(sheets, "try-error")) return(fail("excel_sheets() failed"))
  diag$n_sheets <- length(sheets)

  pick <- try(sbs_pick_sheet(path, TITLE_RX), silent = TRUE)
  if (inherits(pick, "try-error")) {
    return(fail(paste0("table not found: ", str_squish(as.character(pick))), "wrong_table"))
  }
  g <- pick$grid; diag$sheet_name <- pick$sheet

  tr <- sbs_find_cells(g, TITLE_RX)
  diag$title_row <- tr$row[1]
  table_title <- str_squish(tr$value[1])

  ur <- sbs_find_cells(g, UNITS_RX)
  if (!nrow(ur)) return(fail("units row '(En porcentaje)' not found"))
  diag$units_label <- str_squish(ur$value[1])

  # report date lives between the title and the units row
  date_raw <- NA_character_
  if (ur$row[1] > diag$title_row + 1L) {
    cand <- g[(diag$title_row + 1L):(ur$row[1] - 1L), 1]
    cand <- cand[!is.na(cand) & str_squish(cand) != ""]
    if (length(cand)) date_raw <- str_squish(as.character(cand[1]))
  }
  diag$date_cell_raw <- date_raw
  rdate <- sbs_parse_date(date_raw)
  expected <- month_end(report_year, report_month)
  # Fall back to the filename month when the cell is missing/unparseable, but
  # record that we did so rather than papering over it.
  diag$date_matches_filename <- !is.na(rdate) &&
    format(rdate, "%Y-%m") == format(expected, "%Y-%m")
  diag$report_date <- if (is.na(rdate)) expected else rdate

  hd <- sbs_find_cells(g, HDR_RX) %>% filter(col == 1L)
  if (!nrow(hd)) return(fail("header anchor 'Empresas' not found"))
  hrow <- hd$row[1]; diag$header_row <- hrow

  # modality columns = every non-blank cell to the right of the anchor
  hdr_vals <- unlist(g[hrow, ], use.names = FALSE)
  lab_cols <- which(!is.na(hdr_vals) & str_squish(hdr_vals) != "")
  lab_cols <- lab_cols[lab_cols > 1L]
  if (!length(lab_cols)) return(fail("no modality labels right of 'Empresas'"))

  cleaned <- map(hdr_vals[lab_cols], clean_modality_label)
  keys <- map_chr(cleaned, "key")
  diag$label_repairs <- sum(map_lgl(cleaned, "repaired"))

  cols <- tibble(
    col_index    = lab_cols,
    modality_raw = str_squish(as.character(hdr_vals[lab_cols])),
    key          = keys,
    footnote_ref = map_chr(cleaned, "footnote_ref")
  ) %>% left_join(MODALITY_MAP, by = "key")

  if (any(is.na(cols$modality_std))) {
    return(fail(paste0("unmapped modality label(s): ",
                       paste(unique(cols$modality_raw[is.na(cols$modality_std)]), collapse = " | "))))
  }
  diag$n_modalities <- sum(cols$modality_std != "__TOTAL__")

  # data block: rows below the header, stopping at the notes
  first <- norm_txt(g[[1]])
  body <- seq.int(hrow + 1L, nrow(g))
  note_at <- body[!is.na(first[body]) & str_detect(first[body], NOTE_RX)]
  last_row <- if (length(note_at)) min(note_at) - 1L else nrow(g)
  body <- body[body <= last_row]
  body <- body[!is.na(first[body]) & first[body] != ""]     # skip blank rows inside the block
  if (!length(body)) return(fail("empty data block"))

  is_total <- str_detect(first[body], TOTROW_RX)
  diag$has_total_row <- any(is_total)
  diag$n_banks <- sum(!is_total)

  long <- map_dfr(seq_len(nrow(cols)), function(k) {
    j <- cols$col_index[k]
    txt <- as.character(g[[j]])[body]
    tibble(
      row_index    = body,
      col_index    = j,
      bank_raw     = str_squish(as.character(g[[1]])[body]),
      is_total_row = is_total,
      modality_raw = cols$modality_raw[k],
      modality_std = cols$modality_std[k],
      footnote_ref = cols$footnote_ref[k],
      is_total_col = cols$modality_std[k] == "__TOTAL__",
      value_text   = txt,
      value_num    = sbs_num(txt)
    )
  })

  blankish <- is.na(long$value_text) | str_squish(long$value_text) %in% c("", "-", "–", "—")
  diag$n_cells          <- nrow(long)
  diag$n_cells_missing  <- sum(blankish)
  diag$n_cells_unparsed <- sum(!blankish & is.na(long$value_num))

  long <- long %>%
    mutate(
      cuadro = CODE, report_year = report_year, report_month = report_month,
      report_date = diag$report_date, sheet_name = diag$sheet_name,
      table_title = table_title, units_label = diag$units_label,
      source_file = fname, source_url = url, source_sha256 = sha256,
      file_format = file_format,
      value_flag = case_when(
        blankish                                   ~ "missing_marker",
        is.na(value_num)                           ~ "unparsed",
        !is_total_col & abs(value_num) < 1e-6 & value_num != 0 ~ "float_noise",
        !is_total_col & value_num < 0              ~ "negative_share",
        TRUE                                       ~ "ok"
      )
    )

  list(diag = diag, data = long)
}

# ---- run over every available file -------------------------------------------
mf <- read_csv(MANIFEST, show_col_types = FALSE) %>% filter(available)
cat(sprintf("Parsing %d workbooks...\n\n", nrow(mf)))

out <- map(seq_len(nrow(mf)), function(i) {
  parse_one(mf$dest[i], mf$url[i], mf$report_year[i], mf$report_month[i],
            mf$detected_format[i], mf$sha256[i])
})

diagnostics <- map_dfr(out, "diag")
panel_raw   <- map_dfr(out, "data")

cat("--- parse status ---\n"); print(as.data.frame(count(diagnostics, status)), row.names = FALSE)
if (any(diagnostics$status != "ok")) {
  cat("\nNon-ok files:\n")
  diagnostics %>% filter(status != "ok") %>% select(source_file, status, message) %>%
    as.data.frame() %>% print(row.names = FALSE)
}

# ==============================================================================
# Entity-name standardization
# ==============================================================================
# Scope qualifiers such as "(con sucursales en el exterior)" are EXTRACTED into
# their own column, never silently deleted: whether foreign branches are in
# scope is a live candidate explanation for level differences against the
# Series Estadisticas extract.
manual_path <- BANK_MAP
if (!file.exists(manual_path)) {
  # Documented successions and cross-source aliases. Every row is a deliberate,
  # sourced statement -- nothing here was inferred from name similarity alone.
  # review_required = TRUE means: do NOT rely on this mapping until checked.
  tribble(
    ~canon_excel,                          ~bank_standardized,   ~note, ~review_required,
    "BANCO CONTINENTAL",                   "bbva_peru",          "BBVA Continental -> BBVA Peru (rebrand)", FALSE,
    "BANCO BBVA PERU",                     "bbva_peru",          "current name", FALSE,
    "BBVA PERU",                           "bbva_peru",          "current name", FALSE,
    "BANCO DE COMERCIO",                   "bancom",             "Banco de Comercio -> BANCOM (rebrand)", FALSE,
    "BANCOM",                              "bancom",             "current name", FALSE,
    "BANCO DE CREDITO DEL PERU",           "bcp",                "BCP", FALSE,
    "BANCO DE CREDITO",                    "bcp",                "Series Estadisticas spelling", FALSE,
    "BANCO FINANCIERO",                    "banco_pichincha",    "Banco Financiero -> Banco Pichincha (2019 rebrand)", FALSE,
    "BANCO PICHINCHA",                     "banco_pichincha",    "current name", FALSE,
    "BANCO INTERAMERICANO DE FINANZAS",    "bif",                "BanBif", FALSE,
    "SCOTIABANK PERU",                     "scotiabank",         "Scotiabank Peru", FALSE,
    "SCOTIABANK",                          "scotiabank",         "Series Estadisticas spelling", FALSE,
    "CITIBANK",                            "citibank",           "Citibank Peru", FALSE,
    "INTERBANK",                           "interbank",          "Interbank", FALSE,
    "MIBANCO",                             "mibanco",            "Mibanco", FALSE,
    "BANCO GNB",                           "banco_gnb",          "HSBC Peru -> Banco GNB Peru (2013 acquisition)", FALSE,
    "BANCO HSBC",                          "hsbc_peru",          "kept SEPARATE from GNB; predecessor entity", FALSE,
    "HSBC BANK PERU",                      "hsbc_peru",          "kept SEPARATE from GNB; predecessor entity", FALSE,
    "BANCO FALABELLA PERU",                "banco_falabella",    "Banco Falabella", FALSE,
    "BANCO FALABELLA",                     "banco_falabella",    "Series Estadisticas spelling", FALSE,
    "FALABELLA PERU",                      "banco_falabella",    "2007 vintages print the name without the 'B.' prefix ('Falabella Peru S.A'); unambiguous", FALSE,
    "BANCO SANTANDER PERU",                "santander_peru",     "Santander Peru (2007-)", FALSE,
    "BANCO SANTANDER",                     "santander_peru",     "Series Estadisticas spelling", FALSE,
    "SANTANDER PERU",                      "santander_peru",     "2007 vintages print the name without the 'B.' prefix ('Santander Peru S.A.'); unambiguous", FALSE,
    "BANCO SANTANDER CENTRAL HISPANO",     "bsch_peru",          "predecessor entity, last observed 2003-02. Banco Santander Peru S.A. only appears from 2007-10 -- a 4.5-year gap. Relationship NOT established; kept separate. Do not merge without verifying the corporate history.", TRUE,
    "BANCO RIPLEY",                        "banco_ripley",       "Banco Ripley", FALSE,
    "BANCO AZTECA PERU",                   "alfin_banco",        "Banco Azteca del Peru renamed Alfin Banco S.A. -- Res. SBS 03054-2021 (published 2021-11-09); same legal entity, so the series is continuous. Ownership changed in 2020 (Grupo Elektra sold 100%); treat as a break for ownership-based analysis, not for entity identity.", FALSE,
    "AZTECA DEL PERU",                     "alfin_banco",        "Series Estadisticas spelling of Banco Azteca del Peru; see Res. SBS 03054-2021", FALSE,
    "ALFIN BANCO",                         "alfin_banco",        "post-2021 denomination of the same entity (Res. SBS 03054-2021)", FALSE,
    "BANCO ICBC",                          "banco_icbc",         "ICBC Peru", FALSE,
    "BANK OF CHINA",                       "bank_of_china",      "Bank of China Peru", FALSE,
    "BANCO BCI PERU",                      "banco_bci",          "BCI Peru", FALSE,
    "BANCO BCI",                           "banco_bci",          "Series Estadisticas spelling", FALSE,
    "BANCO EFECTIVA",                      "banco_efectiva",     "Financiera Efectiva -> Banco Efectiva (Res. SBS 01444-2026)", FALSE,
    "COMPARTAMOS BANCO",                   "compartamos_banco",  "Compartamos", FALSE,
    "SANTANDER CONSUMER BANK",             "santander_consumer", "distinct from Santander Peru", FALSE,
    "DEUTSCHE BANK PERU",                  "deutsche_bank",      "exited; no successor merge", FALSE,
    "DEUTSCHE BANK",                       "deutsche_bank",      "Series Estadisticas spelling", FALSE,
    "BANCO CENCOSUD",                      "banco_cencosud",     "exited; no successor merge", FALSE,
    "BANCO DEL TRABAJO",                   "banco_del_trabajo",  "exited 2009; no successor merge", FALSE,
    "BANCO DE TRABAJO",                    "banco_del_trabajo",  "Series Estadisticas spelling", FALSE,
    "BANKBOSTON",                          "bankboston",         "exited; no successor merge", FALSE,
    "BANCO STANDARD CHARTERED",            "standard_chartered", "exited; no successor merge", FALSE,
    "STANDARD CHARTERED",                  "standard_chartered", "Series Estadisticas spelling", FALSE,
    "BANCO SUDAMERICANO",                  "banco_sudamericano", "merged into Scotiabank 2006; NOT merged here", FALSE,
    "BANCO WIESE SUDAMERIS",               "wiese_sudameris",    "merged into Scotiabank 2006; NOT merged here", FALSE,
    "WIESE SUDAMERIS",                     "wiese_sudameris",    "Series Estadisticas spelling", FALSE,
    "BNP PARIBAS ANDES",                   "bnp_paribas",        "exited; no successor merge", FALSE,
    "BNP PARIBAS",                         "bnp_paribas",        "Series Estadisticas spelling", FALSE,
    "BANCO MIZUHO",                        "banco_mizuho",       "Mizuho Peru", FALSE,
    "TOTAL BANCA MULTIPLE",                "__SYSTEM__",         "system aggregate row", FALSE,
    "BANCA MULTIPLE",                      "__SYSTEM__",         "Series Estadisticas aggregate row", FALSE
  ) %>% write_csv(manual_path)
  cat(sprintf("\nSeeded manual bank map: %s\n", manual_path))
}
manual <- read_bank_map(manual_path)

bank_names <- panel_raw %>% distinct(bank_raw) %>% pull(bank_raw)
bank_std   <- standardize_banks(bank_names, "excel_B-2343", manual)

panel <- panel_raw %>%
  left_join(bank_std %>% select(bank_raw, bank_standardized, scope_note, bank_footnote,
                                mapping_method, mapping_confidence, review_required),
            by = "bank_raw") %>%
  mutate(bank_id = bank_standardized)   # project-scoped id; SBS publishes none

# ---- crosswalk output --------------------------------------------------------
# Minimal spoke: printed name -> bank_standardized (+ review flag). See 00_sbs_utils.R.
xwalk_bank <- write_xwalk(bank_std, "b2343")


# ---- panel outputs -----------------------------------------------------------
panel_out <- panel %>%
  select(cuadro, report_year, report_month, report_date, sheet_name, table_title,
         units_label, source_file, source_url, source_sha256, file_format,
         row_index, col_index, bank_raw, bank_standardized, bank_id, scope_note,
         bank_footnote, mapping_method, mapping_confidence, review_required,
         modality_raw, modality_std, footnote_ref,
         is_total_row, is_total_col, value_text, value_num, value_flag) %>%
  arrange(report_date, row_index, col_index)

write_csv(panel_out, file.path(PROC, "b2343_long.csv"))

# Bank-month totals: the reconciliation key (miles de S/)
totals <- panel_out %>%
  filter(is_total_col) %>%
  select(report_date, report_year, report_month, bank_raw, bank_standardized,
         bank_id, scope_note, is_total_row, review_required, file_format,
         source_file, total_credits_thousand_pen = value_num, value_text, value_flag)
write_csv(totals, file.path(PROC, "b2343_total_credits.csv"))

write_csv(diagnostics, file.path(VALID, "parse_diagnostics.csv"))

# ---- summary -----------------------------------------------------------------
cat("\n--- panel ---\n")
cat(sprintf("rows                : %d\n", nrow(panel_out)))
cat(sprintf("months parsed       : %d  (%s .. %s)\n", n_distinct(panel_out$report_date),
            min(panel_out$report_date), max(panel_out$report_date)))
cat(sprintf("distinct bank_raw   : %d\n", n_distinct(panel_out$bank_raw)))
cat(sprintf("unmatched bank names: %d\n", sum(is.na(xwalk_bank$bank_standardized))))
cat(sprintf("review_required     : %d of %d entity names\n",
            sum(xwalk_bank$review_required), nrow(xwalk_bank)))
cat(sprintf("modality labels     : %d raw -> %d standardized\n",
            n_distinct(panel_out$modality_raw), n_distinct(panel_out$modality_std)))
cat(sprintf("label repairs       : %d files\n", sum(diagnostics$label_repairs > 0, na.rm = TRUE)))
cat(sprintf("date != filename    : %d files\n", sum(!diagnostics$date_matches_filename, na.rm = TRUE)))
cat(sprintf("files w/o total row : %d\n", sum(!diagnostics$has_total_row, na.rm = TRUE)))
cat("\nvalue_flag:\n"); print(as.data.frame(count(panel_out, value_flag)), row.names = FALSE)
cat("\nOutputs written to data/processed/ and output/validation/.\nDone.\n")
