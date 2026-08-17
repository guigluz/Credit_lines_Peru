# ==============================================================================
# 07_peru_dollarization.R
#
# Replicates Figure 2 of Keller, L. "Capital Controls and Risk Misallocation"
#   ("Dollarization of Deposits and Loans in the Peruvian Banking System")
# and extends it beyond the paper's 2014 endpoint.
#
# SOURCE ----------------------------------------------------------------------
# SBS (Superintendencia de Banca, Seguros y AFP del Perú), statistical series:
#   "Carpeta de Cuadros Estadísticos - Sistema Financiero", table code SF-2101.
#   https://www.sbs.gob.pe/app/stats_net/stats/EstadisticaSistemaFinancieroResultados.aspx?c=SF-2101
#   Files: https://intranet2.sbs.gob.pe/estadistica/financiera/<YYYY>/Diciembre/SF-2101-di<YYYY>.ZIP
#
# Each December vintage is a .zip holding one .xls workbook whose sheets
#   "Ctas BM"   -> Banca Múltiple  (the commercial banking system)
#   "Ctas SSFF" -> Sistema Financiero (banks + financieras + cajas + edpymes)
# report, for the last 5-7 year-ends, the rows
#   "Créditos Directos (Miles S/)" / "MN (Miles S/)" / "ME (Miles US$)"
#   "Depósitos totales (Miles S/)" / "MN (Miles S/)" / "ME (Miles US$)"
# Stacking the December vintages 2007-... yields an annual series from 2001 on.
# (SBS does not publish this workbook before Dec-2007, so 2000 — the first year
#  in Keller's figure — is not recoverable from this source; the series here
#  starts in 2001.)
#
# DOLLARIZATION MEASURES ------------------------------------------------------
# The total is reported in soles and already contains the FX-converted stock, so
#   share_ME = 1 - MN / TOTAL                       (current exchange rate)
# is exact and needs no external FX data. The implied end-of-year exchange rate
#   TC = (TOTAL - MN) / ME_USD
# is recovered and used to build a constant-exchange-rate variant
#   share_ME_const = (ME_USD * TC0) / (MN + ME_USD * TC0)
# which strips out the mechanical effect of sol depreciation on the ratio (the
# convention BCRP uses for its published dollarization coefficients).
#
# OUTPUTS ---------------------------------------------------------------------
#   data/raw/sbs/                                   cached SF-2101 zips + xls
#   data/processed/peru_dollarization_sbs.rds/.csv  tidy annual panel
#   data/documentation/peru_dollarization_dictionary.xlsx
#   output/figures/fig_peru_dollarization_bm.pdf/.png     (Banca Múltiple)
#   output/figures/fig_peru_dollarization_ssff.pdf/.png   (Sistema Financiero)
#   output/figures/fig_peru_dollarization_bm_constfx.pdf/.png
# ==============================================================================

library(tidyverse)
library(readxl)
library(httr)
library(writexl)

# This file is UTF-8, but Rscript inherits the shell locale (often "C"), in which
# case the parser leaves accented literals tagged as "unknown" and the graphics
# devices render them as garbage. Force a UTF-8 ctype where one exists, and tag
# the accented strings explicitly with u8() below so the fix does not depend on
# the locale being available.
if (!isTRUE(l10n_info()$`UTF-8`))
  for (loc in c("en_US.UTF-8", "C.UTF-8", "es_PE.UTF-8"))
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break

u8 <- function(x) { if (is.character(x)) Encoding(x) <- "UTF-8"; x }

# ── Parameters ────────────────────────────────────────────────────────────────
VINTAGES  <- 2007:2025   # December SF-2101 vintages to stack (extend as SBS publishes)
PLOT_FROM <- 2001        # first year on the figure (2000 unavailable from SF-2101)
PLOT_TO   <- 2023        # last year on the figure; raise to use the full panel
CC_YEAR   <- 2011        # Keller's event line: forward-position limits / capital controls
TC_BASE   <- 2010        # base year for the constant-exchange-rate variant

raw_dir  <- "data/raw/sbs"
xls_dir  <- file.path(raw_dir, "xls")
proc_dir <- "data/processed"
doc_dir  <- "data/documentation"
fig_dir  <- "output/figures"
for (d in c(raw_dir, xls_dir, proc_dir, doc_dir, fig_dir))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ── 1. Download the SBS zips ──────────────────────────────────────────────────
# NOTE: intranet2.sbs.gob.pe serves an incomplete TLS chain, so a verified
# handshake fails on most machines. We attempt a verified download first and
# only fall back to an unverified one (with a warning) for these public
# statistical files.
sbs_url <- function(year)
  sprintf("https://intranet2.sbs.gob.pe/estadistica/financiera/%d/Diciembre/SF-2101-di%d.ZIP",
          year, year)

download_sbs <- function(year, dest) {
  if (file.exists(dest) && file.size(dest) > 1e4) return(invisible(dest))
  url <- sbs_url(year)
  r <- try(GET(url, write_disk(dest, overwrite = TRUE), timeout(300)), silent = TRUE)
  if (inherits(r, "try-error") || http_error(r)) {
    warning("Verified TLS download failed for ", year,
            "; retrying without certificate verification (SBS chain is incomplete).",
            call. = FALSE)
    r <- GET(url, write_disk(dest, overwrite = TRUE), timeout(300),
             config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
    stop_for_status(r)
  }
  invisible(dest)
}

# The zip members carry Latin-1 accented names that R's internal unzip cannot
# create under a C locale, so we stream the workbook out with `unzip -p`.
extract_xls <- function(zip, dest) {
  if (file.exists(dest) && file.size(dest) > 1e4) return(invisible(dest))
  ok <- system2("unzip", c("-p", "-C", shQuote(zip), shQuote("*.xls")), stdout = dest)
  if (!file.exists(dest) || file.size(dest) < 1e4)
    stop("Could not extract the .xls from ", basename(zip))
  invisible(dest)
}

xls_paths <- map_chr(VINTAGES, function(y) {
  zip <- file.path(raw_dir, sprintf("SF-2101-di%d.ZIP", y))
  xls <- file.path(xls_dir, sprintf("carpeta_sif_%d.xls", y))
  download_sbs(y, zip)
  extract_xls(zip, xls)
  xls
})
names(xls_paths) <- VINTAGES

# ── 2. Parse one "Ctas ..." sheet ─────────────────────────────────────────────
as_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)))))

parse_ctas <- function(path, sheet, vintage) {

  x <- suppressWarnings(suppressMessages(
    read_excel(path, sheet = sheet, col_names = FALSE,
               col_types = "text", .name_repair = "minimal")))
  x   <- as.data.frame(x, stringsAsFactors = FALSE)
  lab <- trimws(ifelse(is.na(x[[1]]), "", x[[1]]))

  # Header row = the row with the most Excel date serials.
  n_serial <- function(r) {
    v <- as_num(unlist(x[r, -1])); sum(!is.na(v) & v > 30000 & v < 70000)
  }
  hdr <- which.max(vapply(seq_len(min(15, nrow(x))), n_serial, numeric(1)))
  dts <- as.Date(as_num(unlist(x[hdr, ])), origin = "1899-12-30")

  # The same dates repeat under the "Variación/Crecimiento Anual (%)" block, so
  # keep only the columns spanned by the "Saldo" banner one row above.
  banner <- trimws(as.character(unlist(x[hdr - 1L, ])))
  banner[banner %in% c("", "NA")] <- NA_character_
  j0  <- which(grepl("^Saldo", banner))[1]
  nxt <- which(!is.na(banner) & seq_along(banner) > j0)
  j1  <- if (length(nxt)) nxt[1] - 1L else ncol(x)
  keep <- seq_len(ncol(x)) >= j0 & seq_len(ncol(x)) <= j1 &
          !is.na(dts) & format(dts, "%m") == "12"

  # Each concept is a block of three consecutive rows: TOTAL / MN / ME.
  grab <- function(rx) {
    i <- grep(rx, lab)
    if (!length(i)) stop("row '", rx, "' not found in ", sheet, " (", vintage, ")")
    i <- i[1]
    if (!grepl("^MN", lab[i + 1L]) || !grepl("^ME", lab[i + 2L]))
      stop("unexpected MN/ME layout under '", lab[i], "' in ", sheet, " (", vintage, ")")
    list(total = as_num(unlist(x[i,        ]))[keep],
         mn    = as_num(unlist(x[i + 1L, ]))[keep],
         me    = as_num(unlist(x[i + 2L, ]))[keep],
         label = lab[i])
  }
  cr <- grab("^Cr.ditos Directos")
  dp <- grab("^Dep.sitos totales")

  tibble(
    vintage      = vintage,
    date         = dts[keep],
    cred_total   = cr$total,  cred_mn = cr$mn,  cred_me_usd = cr$me,
    dep_total    = dp$total,  dep_mn  = dp$mn,  dep_me_usd  = dp$me,
    label_credit = cr$label,  label_deposit = dp$label
  )
}

sheets <- c(BM = "Ctas BM", SSFF = "Ctas SSFF")

raw_panel <- map_dfr(names(sheets), function(u)
  map_dfr(VINTAGES, function(y)
    parse_ctas(xls_paths[[as.character(y)]], sheets[[u]], y) |> mutate(unit = u)))

# ── 3. Collapse vintages and build the dollarization measures ────────────────
# Overlapping vintages agree to <0.6pp; keep the most recent publication of each
# year, and record the across-vintage spread as a revision diagnostic.
doll <- raw_panel |>
  mutate(year = as.integer(format(date, "%Y"))) |>
  filter(!is.na(cred_total), cred_total > 0, !is.na(dep_total), dep_total > 0) |>
  group_by(unit, year) |>
  mutate(n_vintages   = n(),
         rev_spread_c = 100 * (max(1 - cred_mn / cred_total) - min(1 - cred_mn / cred_total)),
         rev_spread_d = 100 * (max(1 - dep_mn  / dep_total)  - min(1 - dep_mn  / dep_total))) |>
  slice_max(vintage, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    # current exchange rate (as reported by SBS)
    doll_credit   = 100 * (1 - cred_mn / cred_total),
    doll_deposit  = 100 * (1 - dep_mn  / dep_total),
    # implied end-of-year exchange rate, S/ per US$
    fx_credit     = (cred_total - cred_mn) / cred_me_usd,
    fx_deposit    = (dep_total  - dep_mn)  / dep_me_usd,
    fx_implied    = (cred_total - cred_mn + dep_total - dep_mn) / (cred_me_usd + dep_me_usd)
  ) |>
  arrange(unit, year)

# constant-exchange-rate variant, base = TC_BASE
fx0 <- doll |> filter(year == TC_BASE) |> group_by(unit) |> summarise(fx0 = mean(fx_implied))
doll <- doll |>
  left_join(fx0, by = "unit") |>
  mutate(
    doll_credit_constfx  = 100 * (cred_me_usd * fx0) / (cred_mn + cred_me_usd * fx0),
    doll_deposit_constfx = 100 * (dep_me_usd  * fx0) / (dep_mn  + dep_me_usd  * fx0)
  ) |>
  select(unit, year, everything(), -date)

stopifnot(all(doll$doll_credit  > 0 & doll$doll_credit  < 100),
          all(doll$doll_deposit > 0 & doll$doll_deposit < 100))

saveRDS(doll, file.path(proc_dir, "peru_dollarization_sbs.rds"))
write_csv(doll, file.path(proc_dir, "peru_dollarization_sbs.csv"))

message(sprintf("Panel: %d-%d, units %s; max across-vintage revision %.2f pp",
                min(doll$year), max(doll$year), paste(unique(doll$unit), collapse = "/"),
                max(c(doll$rev_spread_c, doll$rev_spread_d))))

# ── 4. Variable dictionary ───────────────────────────────────────────────────
dict <- tribble(
  ~variable,               ~definition,                                                        ~sbs_source,
  "unit",                  "Aggregate: BM = Banca Múltiple, SSFF = Sistema Financiero",         "SF-2101 sheets 'Ctas BM' / 'Ctas SSFF'",
  "year",                  "Calendar year; all stocks are end-December",                        "SF-2101 'Saldo' column headers",
  "vintage",               "December vintage of the SF-2101 workbook the figure was taken from", "SF-2101 file year",
  "n_vintages",            "Number of vintages reporting this year (revision diagnostic)",      "derived",
  "rev_spread_c",          "Max-min of credit dollarization across vintages, pp",               "derived",
  "rev_spread_d",          "Max-min of deposit dollarization across vintages, pp",              "derived",
  "cred_total",            "Direct credit, all currencies, thousands of S/",                    "row 'Créditos Directos (Miles S/)'",
  "cred_mn",               "Direct credit in domestic currency, thousands of S/",               "row 'MN (Miles S/)' under Créditos Directos",
  "cred_me_usd",           "Direct credit in foreign currency, thousands of US$",               "row 'ME (Miles US$)' under Créditos Directos",
  "dep_total",             "Total deposits, all currencies, thousands of S/",                   "row 'Depósitos totales (Miles S/)'",
  "dep_mn",                "Total deposits in domestic currency, thousands of S/",              "row 'MN (Miles S/)' under Depósitos totales",
  "dep_me_usd",            "Total deposits in foreign currency, thousands of US$",              "row 'ME (Miles US$)' under Depósitos totales",
  "doll_credit",           "Share of direct credit in foreign currency, % (current FX)",        "100 * (1 - cred_mn / cred_total)",
  "doll_deposit",          "Share of deposits in foreign currency, % (current FX)",             "100 * (1 - dep_mn / dep_total)",
  "fx_credit",             "Implied end-year S//US$ from the credit block",                     "(cred_total - cred_mn) / cred_me_usd",
  "fx_deposit",            "Implied end-year S//US$ from the deposit block",                    "(dep_total - dep_mn) / dep_me_usd",
  "fx_implied",            "Implied end-year S//US$, credit and deposits pooled",               "derived",
  "fx0",                   sprintf("Implied S//US$ in the base year %d", TC_BASE),              "derived",
  "doll_credit_constfx",   "Credit dollarization at a constant exchange rate, %",               "100 * cred_me_usd*fx0 / (cred_mn + cred_me_usd*fx0)",
  "doll_deposit_constfx",  "Deposit dollarization at a constant exchange rate, %",              "100 * dep_me_usd*fx0 / (dep_mn + dep_me_usd*fx0)",
  "label_credit",          "Verbatim SBS row label for the credit block (varies by vintage)",   "SF-2101",
  "label_deposit",         "Verbatim SBS row label for the deposit block (varies by vintage)",  "SF-2101"
)
dict <- mutate(dict, across(everything(), u8))
write_xlsx(dict, file.path(doc_dir, "peru_dollarization_dictionary.xlsx"))

# ── 5. Figure ────────────────────────────────────────────────────────────────
keller_theme <- theme_bw(base_size = 10, base_family = "serif") +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
    panel.border       = element_rect(colour = "grey30", linewidth = 0.4),
    axis.title.x       = element_blank(),
    axis.text.x        = element_text(size = 7),
    axis.title.y       = element_text(size = 8),
    legend.title       = element_blank(),
    legend.position    = "bottom",
    legend.key.width   = unit(1.6, "lines"),
    legend.margin      = margin(t = -4),
    plot.title         = element_text(size = 10, face = "bold"),
    plot.caption       = element_text(size = 7, hjust = 0, colour = "grey30")
  )

plot_dollarization <- function(dat, unit_code, credit_var, deposit_var,
                               title, caption) {

  d <- dat |>
    filter(unit == unit_code, year >= PLOT_FROM, year <= PLOT_TO) |>
    select(year, loans = all_of(credit_var), deposits = all_of(deposit_var)) |>
    pivot_longer(-year, names_to = "series", values_to = "share") |>
    mutate(series = factor(series, levels = c("loans", "deposits"),
                           labels = c("Share dollar loans (%, lhs)",
                                      "Share of dollar deposits (%, lhs)")))

  y_rng  <- range(d$share)
  y_top  <- ceiling(y_rng[2] / 10) * 10
  y_bot  <- floor(y_rng[1] / 10) * 10

  ggplot(d, aes(year, share, colour = series, linetype = series)) +
    geom_vline(xintercept = CC_YEAR, linetype = "dashed",
               colour = "grey20", linewidth = 0.35) +
    annotate("text", x = CC_YEAR - 0.28, y = y_top - 0.14 * (y_top - y_bot),
             label = "Capital Controls", angle = 90, hjust = 1, size = 2.5,
             family = "serif", colour = "grey20") +
    geom_line(linewidth = 0.6) +
    scale_colour_manual(values = c("firebrick2", "grey45")) +
    scale_linetype_manual(values = c("solid", "22")) +
    scale_x_continuous(breaks = seq(PLOT_FROM, PLOT_TO, by = 1), expand = expansion(0.015)) +
    scale_y_continuous(breaks = seq(y_bot, y_top, by = 10),
                       limits = c(y_bot, y_top)) +
    labs(y = "Share of Dollar Deposits and Loans (%)",
         title = title, caption = caption) +
    keller_theme
}

cap_src <- u8(paste0(
  "Source: SBS, Carpeta de Cuadros Estadísticos del Sistema Financiero (SF-2101), December vintages ",
  min(VINTAGES), "-", max(VINTAGES), ".\n",
  "Foreign-currency shares computed as 1 - MN / total. SF-2101 is not published before Dec-2007, so the series starts in 2001."
))

figs <- list(
  bm = list(
    unit = "BM", cv = "doll_credit", dv = "doll_deposit",
    file = "fig_peru_dollarization_bm",
    title = "Dollarization of Deposits and Loans in the Peruvian Banking System",
    cap = paste0(cap_src, u8("\nUnit: Banca Múltiple (commercial banks). Current exchange rate."))),
  ssff = list(
    unit = "SSFF", cv = "doll_credit", dv = "doll_deposit",
    file = "fig_peru_dollarization_ssff",
    title = "Dollarization of Deposits and Loans in the Peruvian Financial System",
    cap = paste0(cap_src, "\nUnit: Sistema Financiero (banks, financieras, cajas, edpymes). Current exchange rate.")),
  bm_const = list(
    unit = "BM", cv = "doll_credit_constfx", dv = "doll_deposit_constfx",
    file = "fig_peru_dollarization_bm_constfx",
    title = "Dollarization of Deposits and Loans in the Peruvian Banking System",
    cap = paste0(cap_src, u8(sprintf("\nUnit: Banca Múltiple. Constant exchange rate (S/ per US$ fixed at its Dec-%d value).", TC_BASE))))
)

# cairo_pdf is not usable everywhere (it silently falls back to pdf() when the
# cairo/X11 libs are missing), so write the PDF with the base device -- correct
# once the strings are tagged UTF-8 -- and the PNG with ragg.
for (f in figs) {
  p <- plot_dollarization(doll, f$unit, f$cv, f$dv, f$title, f$cap)
  ggsave(file.path(fig_dir, paste0(f$file, ".pdf")), p,
         width = 7.2, height = 4.2, device = grDevices::pdf, encoding = "ISOLatin1")
  ggsave(file.path(fig_dir, paste0(f$file, ".png")), p,
         width = 7.2, height = 4.2, dpi = 300, device = ragg::agg_png)
}

message("Figures written to ", fig_dir)

# ── 6. Console check against Keller's Figure 2 ───────────────────────────────
doll |>
  filter(year <= 2014) |>
  select(unit, year, doll_credit, doll_deposit) |>
  pivot_wider(names_from = unit, values_from = c(doll_credit, doll_deposit)) |>
  print(n = Inf)
