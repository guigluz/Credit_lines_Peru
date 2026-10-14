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
# DATA ------------------------------------------------------------------------
#   data/processed/encaje_rates_monthly.csv   BCRP reserve-requirement panel
#       (from 09_encaje_series.R). Percent of reservable obligations; each month's
#       value is the rate in force on the LAST day of the month.
#   The MN marginal rate is NA (not applicable, NOT zero) while the MN regime is a
#   single rate; the NA months are left blank, so the line simply breaks there.
#
# Usage:  Rscript code/12_figures_regression.R      (output: output/figures/*.pdf)
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
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
