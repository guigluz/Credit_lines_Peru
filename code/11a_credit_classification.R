# ==============================================================================
# 11a_credit_classification.R -- classify RCC credit rows as credit lines or term loans
# ==============================================================================
# Run from the project root:   Rscript code/11a_credit_classification.R
# Run before:                  Rscript code/11_loan_reserve_shock_regression.R
#
# DESIGN (conservative classification)
#   1. Every product is classified by the hand-made table
#      data/documentation/credit_classification_table.xlsx:
#        is_credit_line == "T"  -> credit_line
#        is_credit_line == "F"  -> term_loan
#        is_credit_line == "?"  -> undefined   (excluded from both regression samples)
#        is_credit_line missing or "NA" -> ignored    (e.g. credito_otorgado_no_desembolsado)
#   2. `prestamos` is split by a row-level rule that overrides its table rows:
#        revolvencia == "revolvente"                              -> credit_line
#        revolvencia == "no_revolvente"                           -> term_loan
#        revolvencia == "no_identificable" & linea_no_utilizada > 0 -> credit_line
#        revolvencia == "no_identificable" & linea_no_utilizada == 0 -> term_loan
#   A row is one fecha_reporte x firm x entidad x currency x segment x product x revolvencia cell.
#
#   All classification logic is in classify_credit() and the xlsx table; change those to refine it.
#   A product in the data that is missing from the table stops the script (classify it in the table).
#
# INPUTS (never modified)
#   data/raw/muestra_anonima_base2015_v2.parquet          RCC firm sample (10,000 firms)
#   data/documentation/credit_classification_table.xlsx  manual product classification
#
# OUTPUTS
#   data/processed/credit_classified_v2.parquet           credit rows with credit_class and class_rule
#   output/validation/credit_classification_*.csv         diagnostics
#   output/logs/11a_credit_classification.log             console log
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(readxl)
})

CREDIT_PQ  <- "data/raw/muestra_anonima_base2015_v2.parquet"
CLASS_XLSX <- "data/documentation/credit_classification_table.xlsx"
OUT_PQ     <- "data/processed/credit_classified_v2.parquet"
VALID <- "output/validation"
LOGF  <- "output/logs/11a_credit_classification.log"

for (d in c(dirname(OUT_PQ), VALID, dirname(LOGF))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
log_con <- file(LOGF, open = "wt"); sink(log_con, split = TRUE)
fmt <- function(x) format(x, big.mark = ",", trim = TRUE)
hdr <- function(s) cat("\n", strrep("=", 78), "\n", s, "\n", strrep("=", 78), "\n", sep = "")

# ---- classification table ------------------------------------------------------
read_class_table <- function(path = CLASS_XLSX) {
  tab <- as.data.table(read_excel(path, col_types = "text"))
  stopifnot(all(c("producto", "is_credit_line") %in% names(tab)))
  tab <- tab[!is.na(producto)]                       # drops the prestamos sub-rows (revolving T/F/NA)
  tab[, producto := tolower(trimws(producto))]
  stopifnot(!anyDuplicated(tab$producto))
  tab[, table_class := fcase(is_credit_line == "T", "credit_line",
                             is_credit_line == "F", "term_loan",
                             is_credit_line == "?", "undefined",
                             is.na(is_credit_line) | is_credit_line == "NA", "ignored")]
  stopifnot(!anyNA(tab$table_class))
  tab[, .(producto, type, table_class, note)]
}

# ---- classification rule -------------------------------------------------------
# dt needs producto, revolvencia, linea_no_utilizada; adds credit_class and class_rule (by reference).
classify_credit <- function(dt, tab) {
  missing_prod <- setdiff(unique(dt$producto), tab$producto)
  if (length(missing_prod)) stop("products not in the classification table: ", paste(missing_prod, collapse = ", "))
  dt[tab, on = "producto", `:=`(credit_class = i.table_class, class_rule = "table")]
  dt[producto == "prestamos", c("credit_class", "class_rule") := {
    cls <- fcase(revolvencia == "revolvente",                                  "credit_line",
                 revolvencia == "no_revolvente",                               "term_loan",
                 revolvencia == "no_identificable" & linea_no_utilizada > 0,   "credit_line",
                 revolvencia == "no_identificable" & linea_no_utilizada == 0,  "term_loan")
    rule <- fcase(revolvencia == "revolvente",                                 "prestamos: revolving",
                  revolvencia == "no_revolvente",                              "prestamos: non-revolving",
                  revolvencia == "no_identificable" & linea_no_utilizada > 0,  "prestamos: revolving unknown, unused > 0",
                  revolvencia == "no_identificable" & linea_no_utilizada <= 0, "prestamos: revolving unknown, unused = 0")
    list(cls, rule)
  }]
  stopifnot(!anyNA(dt$credit_class))
  invisible(dt)
}

# ==============================================================================
hdr("1. Data and classification table")
cr <- as.data.table(read_parquet(CREDIT_PQ, col_select = c("fecha_reporte", "identificador", "entidad",
                                                           "moneda_codigo", "segmento_credito", "producto",
                                                           "revolvencia", "credito_vigente", "linea_no_utilizada")))
cr[, c("moneda_codigo", "segmento_credito", "producto", "revolvencia") :=
       lapply(.SD, as.character), .SDcols = c("moneda_codigo", "segmento_credito", "producto", "revolvencia")]
cr[, producto := tolower(trimws(gsub("\\s+", " ", producto)))]
stopifnot(!anyNA(cr), all(cr$linea_no_utilizada >= 0),
          all(cr$revolvencia %in% c("revolvente", "no_revolvente", "no_identificable")))
cat(sprintf("Credit file: %s rows, %s firms, %d entidad codes, %d dates (%s to %s).\n",
            fmt(nrow(cr)), fmt(uniqueN(cr$identificador)), uniqueN(cr$entidad), uniqueN(cr$fecha_reporte),
            format(min(cr$fecha_reporte)), format(max(cr$fecha_reporte))))

tab <- read_class_table()
cat("Classification table:\n"); print(tab[, .(producto, table_class)])

hdr("2. Classification")
classify_credit(cr, tab)

by_rule <- cr[, .(n_rows = .N, n_firms = uniqueN(identificador),
                  credito_vigente_mn = sum(credito_vigente) / 1e6, linea_no_utilizada_mn = sum(linea_no_utilizada) / 1e6),
              by = .(credit_class, class_rule, producto, revolvencia)][order(credit_class, producto, revolvencia)]
# Percentages of total credit (drawn + unused, all rows): pct_total_credit sums to 100 over the file
by_rule[, total_credit_mn := credito_vigente_mn + linea_no_utilizada_mn]
by_rule[, `:=`(pct_total_credit_vigente = 100 * credito_vigente_mn / sum(total_credit_mn),
               pct_total_credit_unused  = 100 * linea_no_utilizada_mn / sum(total_credit_mn),
               pct_total_credit         = 100 * total_credit_mn / sum(total_credit_mn))]
stopifnot(isTRUE(all.equal(sum(by_rule$pct_total_credit), 100)))
fwrite(by_rule, file.path(VALID, "credit_classification_by_product.csv"))
print(by_rule[, .(credit_class, class_rule, producto, revolvencia, n_rows,
                  vig_mn = round(credito_vigente_mn), unused_mn = round(linea_no_utilizada_mn),
                  pct_vig = round(pct_total_credit_vigente, 2), pct_unused = round(pct_total_credit_unused, 2),
                  pct_total = round(pct_total_credit, 2))], nrows = 100)

by_class <- cr[, .(n_rows = .N, n_firms = uniqueN(identificador),
                   credito_vigente = sum(credito_vigente), linea_no_utilizada = sum(linea_no_utilizada)), by = credit_class]
by_class[, `:=`(share_vigente = credito_vigente / sum(credito_vigente),
                share_unused = linea_no_utilizada / sum(linea_no_utilizada),
                share_commitment = (credito_vigente + linea_no_utilizada) / sum(credito_vigente + linea_no_utilizada))]
fwrite(by_class, file.path(VALID, "credit_classification_by_class.csv"))
cat("\nShares of drawn credit, unused lines and commitments by class (whole sample):\n")
print(by_class[, .(credit_class, n_rows, n_firms, share_vigente = round(share_vigente, 4),
                   share_unused = round(share_unused, 4), share_commitment = round(share_commitment, 4))])

# Stability of the prestamos unused rule within firm x bank x currency relationships
pu <- cr[producto == "prestamos" & revolvencia == "no_identificable",
         .(n = .N, n_pos = sum(linea_no_utilizada > 0)), by = .(identificador, entidad, moneda_codigo)]
cat(sprintf("\nprestamos with unknown revolving status: %s firm-bank-currency relationships; always unused > 0: %s, never: %s, switching: %s.\n",
            fmt(nrow(pu)), fmt(sum(pu$n_pos == pu$n)), fmt(sum(pu$n_pos == 0)), fmt(sum(pu$n_pos > 0 & pu$n_pos < pu$n))))

hdr("3. Output")
write_parquet(cr[, .(fecha_reporte, identificador, entidad, moneda_codigo, segmento_credito, producto, revolvencia,
                     credito_vigente, linea_no_utilizada, credit_class, class_rule)], OUT_PQ)
cat("Wrote ", OUT_PQ, " (", fmt(nrow(cr)), " rows)\n", sep = "")
sink(); close(log_con)
