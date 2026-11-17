# ==============================================================================
# Dollarization of loans and deposits in the Peruvian financial system.
#
# Produces two figures:
#   1) Cross-section -- dollar share of LOANS vs. dollar share of DEPOSITS for
#      individual institutions, at two end-December snapshots. If institutions
#      match currencies on the balance sheet, the dots sit on the 45-degree line.
#   2) Time series  -- dollar share of loans and deposits for the Banca Multiple
#      aggregate, monthly.
#
# DATA ------------------------------------------------------------------------
#   data/raw/Reporte_SistemaFinanciero.csv, read with read_reporte()
#   (code/00_sbs_utils.R). Hand-downloaded from the SBS statistical series
#   ("Series Estadisticas"). Monthly, Jan-2001 -- Sep-2022; every institution of
#   banca multiple, empresas financieras, cajas municipales, cajas rurales and
#   edpymes / empresas de creditos. Both figures come from it.
#
#   Entity types come from the bank map (data/documentation/bank_map_manual.csv,
#   column entity_type). The extract has no subsystem total rows: the Banca
#   Multiple series is summed over its institutions.
#
#   UNITS: unlike SF-2101, BOTH the MN and the ME columns are already in
#   thousands of SOLES. No exchange-rate conversion is needed anywhere, and the
#   shares are not mechanically inflated by sol depreciation.
#
# OUTPUTS ---------------------------------------------------------------------
#   output/figures/fig_dollarization_scatter_entity.pdf
#   output/figures/fig_dollarization_ts_reporte.pdf
# ==============================================================================

library(tidyverse)
source("code/00_sbs_utils.R")   # read_reporte(), reporte_attach_types(), reporte_subsystem()

# Rscript inherits the shell locale (often "C"), which mangles accented names.
if (!isTRUE(l10n_info()$`UTF-8`))
  for (loc in c("en_US.UTF-8", "C.UTF-8", "es_PE.UTF-8"))
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break

u8 <- function(x) { if (is.character(x)) Encoding(x) <- "UTF-8"; x }

# ── Parameters ────────────────────────────────────────────────────────────────
PANEL_YEARS <- c(2010, 2021)          # the two December cross-sections plotted
TS_TYPE     <- "banca_multiple"       # subsystem shown in the time series
TS_UNIT     <- "Banca Múltiple"       # its label
CC_DATE     <- as.Date("2011-01-01")  # Keller's event line: capital controls

fig_dir <- "output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Plot labels for the bank map's entity_type.
TYPE_LABELS <- c(banca_multiple     = "Bank",
                 empresa_financiera = "Financiera",
                 caja_municipal     = "Caja municipal",
                 caja_rural         = "Caja rural",
                 empresa_credito    = "Edpyme / Empresa de créditos")

dollar_shares <- function(df) df |>
  mutate(doll_credit  = 100 * cred_me / cred_total,
         doll_deposit = 100 * dep_me  / dep_total)

# ── 1. Read ───────────────────────────────────────────────────────────────────
reporte <- read_reporte() |>
  reporte_attach_types() |>
  mutate(entity = u8(entity),
         type   = factor(u8(TYPE_LABELS[entity_type]), levels = u8(TYPE_LABELS)),
         # A blank ME against a present total means "no foreign-currency balance".
         cred_me = if_else(is.na(cred_me) & !is.na(cred_total), 0, cred_me),
         dep_me  = if_else(is.na(dep_me)  & !is.na(dep_total),  0, dep_me)) |>
  dollar_shares()
stopifnot(nrow(reporte) > 0, !anyNA(reporte$type))

message(sprintf("Read %d rows: %d institutions (%s), %s to %s.",
                nrow(reporte), n_distinct(reporte$entity),
                paste(names(table(reporte$entity_type)),
                      tapply(reporte$entity, reporte$entity_type, n_distinct),
                      sep = " = ", collapse = ", "),
                format(min(reporte$date), "%b-%Y"), format(max(reporte$date), "%b-%Y")))

# ── 3. Figure 1: cross-section ───────────────────────────────────────────────
cross <- reporte |>
  filter(month(date) == 12, year(date) %in% PANEL_YEARS) |>
  mutate(year = year(date))

# An institution needs BOTH a loan book and a deposit base to have a coordinate.
usable <- cross |> filter(!is.na(doll_credit), !is.na(doll_deposit),
                          cred_total > 0, dep_total > 0)

dropped <- anti_join(cross, usable, by = c("entity", "year"))
if (nrow(dropped)) {
  message("\nDropped (no loan book and/or no deposits -- undefined coordinate):")
  dropped |>
    transmute(year, entity, type, cred_total, dep_total) |>
    arrange(year, entity) |>
    as.data.frame() |> print(row.names = FALSE)
}

message(sprintf("\nPlotted institutions: %s",
                paste(sprintf("%d = %d", PANEL_YEARS,
                              tabulate(match(usable$year, PANEL_YEARS))),
                      collapse = ", ")))

# Correlation is reported to the console only -- deliberately NOT on the figure.
usable |>
  group_by(year) |>
  summarise(n = n(),
            rho_pearson  = cor(doll_deposit, doll_credit),
            rho_spearman = cor(doll_deposit, doll_credit, method = "spearman"),
            .groups = "drop") |>
  as.data.frame() |> print(row.names = FALSE)

scatter_theme <- theme_bw(base_size = 10, base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
    panel.border     = element_rect(colour = "grey30", linewidth = 0.4),
    axis.title       = element_text(size = 8),
    strip.background = element_rect(fill = "grey92", colour = "grey30", linewidth = 0.4),
    strip.text       = element_text(size = 9, face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.margin    = margin(t = -4),
    plot.title       = element_text(size = 10, face = "bold"),
    plot.caption     = element_text(size = 7, hjust = 0, colour = "grey30")
  )

cap_scatter <- u8(paste0(
  "Source: SBS statistical series, institution-level report Jan-2001 - Sep-2022. Each dot is one financial institution at end-December.\n",
  "Shares are foreign-currency balances over total balances; the SBS reports both MN and ME in thousands of soles, so no exchange-rate conversion is applied.\n",
  "Institutions without deposits (e.g. Mitsui Auto Finance, most Edpymes) have no horizontal coordinate and are dropped.\n",
  "The dashed line is the 45-degree currency-matching benchmark. 2021 is the last full December in the source."
))

p_scatter <- ggplot(usable, aes(doll_deposit, doll_credit)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey40", linewidth = 0.35) +
  geom_point(aes(colour = type, shape = type), size = 1.9, stroke = 0.6, alpha = 0.9) +
  scale_colour_manual(values = setNames(c("firebrick2", "grey35", "darkorange2", "steelblue3", "darkgreen"),
                                        u8(TYPE_LABELS)), drop = TRUE) +
  scale_shape_manual(values = setNames(c(16, 17, 18, 15, 4), u8(TYPE_LABELS)), drop = TRUE) +
  scale_x_continuous(breaks = seq(0, 100, 20), expand = expansion(0.02)) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expansion(0.02)) +
  coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(0, 100)) +
  facet_wrap(~ year, nrow = 1) +
  labs(x = "Share of dollar deposits (%)",
       y = "Share of dollar loans (%)",
       title = "Dollar Funding and Dollar Lending Across Peruvian Financial Institutions",
       caption = cap_scatter) +
  scatter_theme

ggsave(file.path(fig_dir, "fig_dollarization_scatter_entity.pdf"), p_scatter,
       width = 7.2, height = 4.6, device = grDevices::pdf, encoding = "ISOLatin1")

# ── 4. Figure 2: time series ─────────────────────────────────────────────────
# Subsystem total summed over its institutions; the extract has no total rows.
ts <- reporte |>
  reporte_subsystem(TS_TYPE, c("cred_total", "cred_me", "dep_total", "dep_me")) |>
  dollar_shares()
stopifnot(nrow(ts) > 0)

# Monthly grid with no gaps or duplicates. Count in year-month arithmetic:
# seq(by = "month") from a month-end date overshoots (Jan-31 -> Mar-03).
ym   <- 12 * year(ts$date) + month(ts$date)
span <- diff(range(ym)) + 1
if (nrow(ts) != span || anyDuplicated(ym))
  warning(sprintf("%s: %d rows, %d distinct months, %d-month span -- gaps or duplicates.",
                  TS_UNIT, nrow(ts), length(unique(ym)), span), call. = FALSE)

message(sprintf("\n%s: %d months, %s to %s.", TS_UNIT, nrow(ts),
                format(min(ts$date), "%b-%Y"), format(max(ts$date), "%b-%Y")))

d <- ts |>
  select(date, loans = doll_credit, deposits = doll_deposit) |>
  pivot_longer(-date, names_to = "series", values_to = "share") |>
  mutate(series = factor(series, levels = c("loans", "deposits"),
                         labels = c("Share of dollar loans (%)",
                                    "Share of dollar deposits (%)")))

y_top <- ceiling(max(d$share) / 10) * 10
y_bot <- floor(min(d$share) / 10) * 10

# Same visual language as the Keller replication: serif, firebrick solid for
# loans, grey dashed for deposits, y-axis snapped to multiples of 10.
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

cap_ts <- u8(paste0(
  "Source: SBS statistical series; ", TS_UNIT, " subsystem total (sum of its institutions), monthly ",
  format(min(ts$date), "%b-%Y"), " - ", format(max(ts$date), "%b-%Y"), ".\n",
  "Shares are foreign-currency balances over total balances. The SBS reports both MN and ME in thousands of soles, so no exchange-rate\n",
  "conversion is applied and the series is not affected by sol depreciation mechanically revaluing the dollar stock."
))

p_ts <- ggplot(d, aes(date, share, colour = series, linetype = series)) +
  geom_vline(xintercept = CC_DATE, linetype = "dashed",
             colour = "grey20", linewidth = 0.35) +
  annotate("text", x = CC_DATE - 90, y = y_top - 0.14 * (y_top - y_bot),
           label = "Capital Controls", angle = 90, hjust = 1, size = 2.5,
           family = "serif", colour = "grey20") +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = c("firebrick2", "grey45")) +
  scale_linetype_manual(values = c("solid", "22")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(0.015)) +
  scale_y_continuous(breaks = seq(y_bot, y_top, by = 10),
                     limits = c(y_bot, y_top)) +
  labs(y = "Share of Dollar Deposits and Loans (%)",
       title = "Dollarization of Deposits and Loans in the Peruvian Banking System",
       caption = cap_ts) +
  keller_theme

ggsave(file.path(fig_dir, "fig_dollarization_ts_reporte.pdf"), p_ts,
       width = 7.2, height = 4.2, device = grDevices::pdf, encoding = "ISOLatin1")

message("\nFigures written to ", fig_dir)
