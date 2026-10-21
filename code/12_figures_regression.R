# ==============================================================================
# 12_figures_regression.R -- figures that visualize the inputs to the reserve requirement shock
# loan-growth regressions (code/11_loan_reserve_shock_regression.R).
#
# Figure 1) fig_encaje_rates   The monthly statutory reserve-requirement rates that
#      drive the bank shocks, full available history: minimum legal rate and
#      marginal rate, domestic currency (MN) against foreign currency (ME), one
#      panel each (the two rates are on different scales, so they are separate
#      panels, not a second axis).
#
# Figure 2) fig_deposit_shares  Foreign-currency (ME) share of deposits over time for each
#      institution with positive deposits in at least MIN_MONTHS months (five years); the
#      big four banks are highlighted, all others are grey context lines. A dashed line marks
#      the baseline date of the shares that enter the bank shocks.
#
# DATA ------------------------------------------------------------------------
#   data/processed/encaje_rates_monthly.csv   BCRP reserve-requirement panel
#       (from 09_encaje_series.R). Percent of reservable obligations; each month's
#       value is the rate in force on the LAST day of the month.
#   The MN marginal rate is NA (not applicable, NOT zero) while the MN regime is a
#   single rate; the NA months are left blank, so the line simply breaks there.
#   data/processed/bank_deposit_shares.csv    MN / ME deposit shares, every Reporte institution
#       x month (from 10_reserve_shocks.R). `valid` = positive MN + ME deposits; months that
#       are not valid are drawn as gaps, never as zero. ME deposits are valued in soles at the
#       month-end exchange rate, so sol depreciation raises the ME share mechanically.
#   data/processed/bank_encaje_shocks.csv     read only for `baseline_date`.
#
# Usage:  Rscript code/12_figures_regression.R      (output: output/figures/*.pdf)
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ggrepel)
})

ENCAJE_CSV <- "data/processed/encaje_rates_monthly.csv"
fig_dir <- "output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Two-series categorical colours: slots 1 (blue) and 2 (orange) of the validated default
# palette (validate_palette.py --mode light --pairs all: all checks pass, CVD delta-E 24.7).
# MN is always blue and ME always orange. Swap here to re-colour every panel.
COL <- c(MN = "#2a78d6", ME = "#eb6834")
LAB <- c(MN = "MN (soles)", ME = "ME (foreign currency)")

# ── Shared look (same as code/05_figures_deposits.R) ─────────────────────────
base_theme <- theme_bw(base_size = 10, base_family = "serif") +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
    panel.border       = element_rect(colour = "grey30", linewidth = 0.4),
    axis.title.x       = element_blank(),
    axis.text.x        = element_text(size = 7),
    axis.title.y       = element_text(size = 8),
    strip.background   = element_rect(fill = "grey92", colour = "grey30", linewidth = 0.4),
    strip.text         = element_text(size = 8, face = "bold"),
    legend.title       = element_blank(),
    legend.position    = "bottom",
    legend.key.width   = unit(1.6, "lines"),
    legend.margin      = margin(t = -4),
    plot.title         = element_text(size = 10, face = "bold"),
    plot.subtitle      = element_text(size = 8, colour = "grey30"),
    plot.caption       = element_text(size = 7, hjust = 0, colour = "grey30")
  )

wrap_cap <- function(...) paste(strwrap(paste0(...), width = 140), collapse = "\n")

save_fig <- function(name, plot, height)
  ggsave(file.path(fig_dir, name), plot, width = 7.2, height = height,
         device = grDevices::pdf, encoding = "ISOLatin1")

# ══ Figure 1: monthly reserve-requirement rates ═══════════════════════════════
stopifnot(file.exists(ENCAJE_CSV))
enc <- fread(ENCAJE_CSV)
enc[, date := as.Date(date)]
stopifnot(!anyDuplicated(enc$date))

lv <- melt(enc[, .(date, MN_min = as.numeric(mn_gen_min_legal), ME_min = as.numeric(me_gen_min_legal),
                   MN_mar = as.numeric(mn_gen_marginal),  ME_mar = as.numeric(me_gen_marginal))],
           id.vars = "date", value.name = "rate")
lv[, `:=`(currency = factor(substr(variable, 1, 2), c("MN", "ME")),
          panel = factor(fifelse(grepl("_min$", variable), "Minimum legal reserve rate",
                                 "Marginal reserve rate"),
                         c("Minimum legal reserve rate", "Marginal reserve rate")))]
setorder(lv, panel, currency, date)

# ME is dashed (colour AND line type) because the two rates coincide for long stretches (both 9% in
# the minimum-legal panel, 2010-2015): a solid ME line would hide the MN line beneath it.
p_rates <- ggplot(lv, aes(date, rate, colour = currency, linetype = currency)) +
  geom_step(linewidth = 0.6, na.rm = TRUE) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = COL, labels = LAB) +
  scale_linetype_manual(values = c(MN = "solid", ME = "22"), labels = LAB) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
  labs(y = "Percent of reservable obligations",
       title = "Statutory reserve requirements, domestic versus foreign currency",
       caption = wrap_cap(
         "Source: BCRP circulars on reserve requirements. ",
         "Each month shows the rate in force on its last day; a rate change effective at any point in a month ",
         "appears in that month. The marginal rate applies to obligations above an entity's base level and exists only ",
         "under the base-plus-marginal regime; it is not applicable where its line is blank.")) +
  base_theme +
  theme(legend.key.width = unit(2.4, "lines"))

save_fig("fig_encaje_rates.pdf", p_rates, height = 5.6)
message("Wrote ", file.path(fig_dir, "fig_encaje_rates.pdf"))


# ══ Figure 2: deposit currency shares by institution ═════════════════════════
SHARES_CSV <- "data/processed/bank_deposit_shares.csv"
SHOCKS_CSV <- "data/processed/bank_encaje_shocks.csv"
MIN_MONTHS <- 60L          # months with positive deposits required: five years (total, not necessarily consecutive)
# Big four by deposits (Sep 2022, S/ bn: BCP 122, BBVA 71, Interbank 45, Scotiabank 44; next is BanBif at 13)
BIG4 <- c(bcp = "BCP", bbva_peru = "BBVA", interbank = "Interbank", scotiabank = "Scotiabank")
# Highlight colours: slots 1, 2, 3 and 7 of the validated default palette (validate_palette.py
# --pairs all: all checks pass; aqua is below 3:1 contrast, relieved by the visible direct labels).
COL_BIG4 <- c(BCP = "#2a78d6", BBVA = "#eb6834", Interbank = "#1baf7a", Scotiabank = "#4a3aa7")

stopifnot(file.exists(SHARES_CSV), file.exists(SHOCKS_CSV))
sh <- fread(SHARES_CSV, encoding = "UTF-8")
sh[, date := as.Date(date)]
base_date <- as.Date(fread(SHOCKS_CSV, select = "baseline_date", nrows = 1L)$baseline_date)

# Keep institutions with positive deposits (valid shares) in at least MIN_MONTHS months
n_pos <- sh[valid == TRUE, .N, by = bank_standardized]
keep  <- n_pos[N >= MIN_MONTHS, bank_standardized]
stopifnot(all(names(BIG4) %in% keep))
message(sprintf("Figure 2: %d of %d institutions have positive deposits in >= %d months; baseline date %s.",
                length(keep), uniqueN(sh$bank_standardized), MIN_MONTHS, format(base_date)))

# Full monthly grid: a month without valid shares is NA (a gap in the line), never a zero
dates <- sort(unique(sh$date))
grid <- merge(CJ(bank_standardized = keep, date = dates),
              sh[valid == TRUE, .(bank_standardized, date, s_me)],
              by = c("bank_standardized", "date"), all.x = TRUE)
grid[, me_pct := 100 * s_me]
OTHER <- sprintf("Other institutions (n = %d)", length(keep) - length(BIG4))
grid[, big4 := bank_standardized %in% names(BIG4)]
grid[, series := factor(fifelse(big4, unname(BIG4[bank_standardized]), OTHER), c(unname(BIG4), OTHER))]
# lines are drawn in group order: grey context first, big four on top
grid[, grp := factor(bank_standardized, c(setdiff(keep, names(BIG4)), names(BIG4)))]
setorder(grid, grp, date)

# Direct labels at each big-four line's last observation (neutral ink, not the series colour)
lab <- grid[big4 == TRUE & !is.na(me_pct)][order(date), .SD[.N], by = bank_standardized]
lab[, label := as.character(series)]

p_shares <- ggplot(grid, aes(date, me_pct, group = grp, colour = series, linewidth = series)) +
  #geom_vline(xintercept = base_date, linetype = "22", colour = "grey40", linewidth = 0.4) +
  geom_line(na.rm = TRUE) +
  geom_vline(xintercept = base_date, linetype = "22", colour = "black", linewidth = 1) +
  annotate("label", x = base_date, y = 100, label = sprintf("Baseline shares (%s)", format(base_date, "%b %Y")),
           hjust = -0.04, vjust = 1, size = 2.4, family = "serif", colour = "grey20",
           fill = "white", label.size = 0, label.padding = unit(0.15, "lines")) +
  geom_text_repel(data = lab, aes(x = date, y = me_pct, label = label), inherit.aes = FALSE,
                  direction = "y", hjust = 0, xlim = c(max(dates) + 90, NA), size = 2.6, family = "serif",
                  colour = "grey20", segment.size = 0.2, segment.colour = "grey60",
                  min.segment.length = 0, box.padding = 0.15, seed = 1) +
  scale_colour_manual(values = c(COL_BIG4, setNames("grey75", OTHER))) +
  scale_linewidth_manual(values = c(setNames(rep(0.8, length(BIG4)), BIG4), setNames(0.25, OTHER))) +
  scale_x_date(breaks = seq(as.Date("2002-01-01"), max(dates), by = "2 years"), date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.12))) +     # right padding holds the direct labels
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.03))) +
  coord_cartesian(clip = "off") +
  labs(y = "Foreign-currency share of deposits",
       title = "Foreign-currency share of deposits by institution",
       subtitle = sprintf("Monthly, %s to %s. %d of %d institutions shown (positive deposits in at least %d months); big four highlighted",
                          format(min(dates), "%b %Y"), format(max(dates), "%b %Y"),
                          length(keep), uniqueN(sh$bank_standardized), MIN_MONTHS),
       caption = wrap_cap(
         "Source: SBS Series Estadisticas (Banca Multiple and Empresas Financieras). Share = ME deposits / (MN + ME deposits), ",
         "both in thousands of soles. ME deposits are valued in soles at the month-end exchange rate, so a sol depreciation ",
         "raises the ME share mechanically. A month without positive deposits is left blank. Institutions with positive deposits ",
         "in fewer than ", MIN_MONTHS, " months (edpymes, which take no deposits, and short-lived entrants and exits) are omitted. ",
         "The dashed line marks the baseline date of the shares used in the bank shocks.")) +
  base_theme

save_fig("fig_deposit_shares.pdf", p_shares, height = 5.2)
message("Wrote ", file.path(fig_dir, "fig_deposit_shares.pdf"))
