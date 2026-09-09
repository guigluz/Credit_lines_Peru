# ==============================================================================
# 05_figures_deposits.R -- deposit figures for the credit-supply project.
#
# Three figures:
#   1) fig_deposits_fd_currency  Monthly growth of aggregate deposits split into
#      the domestic-currency (MN) and foreign-currency (ME) books. Separates
#      real deposit flows from exchange-rate revaluation of the dollar stock.
#   2) fig_deposits_growth_yoy   Monthly growth (bars) against 12-month growth
#      (line). No seasonal adjustment.
#   3) fig_regional_growth_panels  12-month deposit growth by departamento
#      against the national series, one panel per departamento.
#
# DATA ------------------------------------------------------------------------
# Figures 1-2: data/raw/Reporte(01-2001 - 09-2022).csv
#   SBS "Series Estadisticas", monthly Jan-2001 -- Sep-2022. Uses the published
#   "Banca Multiple" subsystem total row.
#   UNITS: both the MN and the ME columns are already in thousands of SOLES;
#   converted to billions here. Nominal -- the source carries no deflator.
#   VALUATION CAVEAT: the ME column is the sol value of foreign-currency
#   deposits, so it moves with the exchange rate even when no depositor moves
#   money. MN is valuation-free. Figure 1 exists to separate the two.
#   ENCODING: UTF-32LE with no BOM. Accented names cannot be translated under a
#   C locale, so columns are renamed by POSITION on read and the aggregate row
#   is matched by pattern rather than by the literal accented string.
#
# Figure 3: data/processed/b2358_branch_panel.csv.gz  (from 02_clean_deposits.R)
#   Branch x month, Jan-2010 -- Jun-2026, no gaps, levels in miles de S/.
#   Foreign branches (departamento == "Extranjero") are dropped.
#   COVERAGE: 23 PRIVATE banks. Banco de la Nacion, cajas municipales and cajas
#   rurales are not in this table, so regional government deposits (canon
#   minero) and much of the small-departamento deposit base are absent. The SBS
#   note also excludes judicial and administrative deposits, so branch deposits
#   summed to bank level need not match published bank totals. Deposits are
#   booked where the ACCOUNT sits, which for corporates is often a head-office
#   branch in Lima rather than where the activity happens.
#
# Usage:  Rscript code/05_figures_deposits.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate)
  library(ggplot2); library(scales); library(data.table)
})

# Rscript inherits the shell locale (often "C"), which mangles accented names.
if (!isTRUE(l10n_info()$`UTF-8`))
  for (loc in c("en_US.UTF-8", "C.UTF-8", "es_PE.UTF-8"))
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break

REPORTE_CSV <- "data/raw/Reporte(01-2001 - 09-2022).csv"
BRANCH_GZ   <- "data/processed/b2358_branch_panel.csv.gz"
AGG_PATTERN <- "^Banca M"      # matches "Banca Múltiple" without the accent
AGG_LABEL   <- "Banca Multiple"
K           <- 1e6             # thousands of soles -> billions of soles

fig_dir <- "output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ── Shared look ──────────────────────────────────────────────────────────────
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
    strip.text         = element_text(size = 6.5, face = "bold"),
    legend.title       = element_blank(),
    legend.position    = "bottom",
    legend.key.width   = unit(1.6, "lines"),
    legend.margin      = margin(t = -4),
    plot.title         = element_text(size = 10, face = "bold"),
    plot.subtitle      = element_text(size = 8, colour = "grey30"),
    plot.caption       = element_text(size = 7, hjust = 0, colour = "grey30")
  )

# Captions are hard-wrapped, not hand-broken: at 7.2in wide and 7pt serif,
# lines longer than ~140 characters run past the right margin.
wrap_cap <- function(...) {
  x <- paste(strwrap(paste0(...), width = 140), collapse = "\n")
  Encoding(x) <- "UTF-8"; x
}

save_fig <- function(name, plot, height)
  ggsave(file.path(fig_dir, name), plot, width = 7.2, height = height,
         device = grDevices::pdf, encoding = "ISOLatin1")

# ══ Part A: aggregate series (figures 1 and 2) ═══════════════════════════════
raw <- read_csv(REPORTE_CSV,
                locale = locale(encoding = "UTF-32LE"),
                col_types = cols(.default = col_character()),
                name_repair = "minimal")

stopifnot(ncol(raw) >= 8)
names(raw)[1:8] <- c("entity", "fecha",
                     "cred_total", "cred_mn", "cred_me",
                     "dep_total",  "dep_mn",  "dep_me")

as_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(x))))

ag <- raw |>
  transmute(entity = trimws(entity),
            date   = as.Date(trimws(fecha), format = "%d/%m/%Y"),
            across(c(dep_total, dep_mn, dep_me), as_num)) |>
  filter(!is.na(date), grepl(AGG_PATTERN, entity)) |>
  arrange(date)

stopifnot(nrow(ag) > 0, n_distinct(ag$entity) == 1, !anyDuplicated(ag$date))

# Monthly grid with no gaps. Count in year-month arithmetic: seq(by = "month")
# from a month-end date overshoots (Jan-31 -> Mar-03).
ym <- 12 * year(ag$date) + month(ag$date)
if (nrow(ag) != diff(range(ym)) + 1)
  warning(sprintf("%s: %d rows over a %d-month span -- gaps in the monthly grid.",
                  AGG_LABEL, nrow(ag), diff(range(ym)) + 1), call. = FALSE)

dep <- ag |>
  transmute(
    date,
    lvl    = dep_total / K,
    lvl_mn = dep_mn    / K,
    lvl_me = dep_me    / K,
    g      = 100 * (log(lvl) - log(lag(lvl))),       # monthly, log points
    g12    = 100 * (log(lvl) - log(lag(lvl, 12))),   # 12-month, log points
    # Contribution of each currency book to total monthly growth: the change in
    # that book over the previous month's total, so the two add up to g.
    contrib_mn = 100 * (lvl_mn - lag(lvl_mn)) / lag(lvl),
    contrib_me = 100 * (lvl_me - lag(lvl_me)) / lag(lvl)
  )

message(sprintf("%s deposits: %d months, %s to %s. Level S/ %.1f bn -> S/ %.1f bn. Monthly growth mean %.2f%%, sd %.2f%%.",
                AGG_LABEL, nrow(dep),
                format(min(dep$date), "%b-%Y"), format(max(dep$date), "%b-%Y"),
                first(dep$lvl), last(dep$lvl),
                mean(dep$g, na.rm = TRUE), sd(dep$g, na.rm = TRUE)))

x_scale_agg <- scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                            expand = expansion(0.015))

SRC_AGG <- paste0("Source: SBS statistical series, ", AGG_LABEL,
                  " subsystem total as published, monthly ",
                  format(min(dep$date), "%b-%Y"), " - ",
                  format(max(dep$date), "%b-%Y"), ". ")

# ── Figure 1: MN / ME decomposition of monthly growth ───────────────────────
dcomp <- dep |>
  select(date, contrib_mn, contrib_me) |>
  pivot_longer(-date, names_to = "book", values_to = "contrib") |>
  mutate(book = factor(book, levels = c("contrib_mn", "contrib_me"),
                       labels = c("Domestic currency (MN)",
                                  "Foreign currency (ME), sol value")))

cap_cur <- wrap_cap(
  SRC_AGG,
  "Contribution of each currency book to the monthly growth rate of total deposits: the change in that book divided by the previous ",
  "month's total. The two bars sum to total growth (black line) by construction. ",
  "This is the plot that decides whether a candidate shock is usable. The ME series is the sol value of foreign-currency deposits, so an ",
  "ME-driven spike can be pure exchange-rate revaluation of an unchanged dollar stock, or a currency switch inside the same banks, neither ",
  "of which is new funding. An MN-driven spike is a real sol inflow. The source reports no exchange rate, so the valuation part cannot be ",
  "netted out here."
)

p_cur <- ggplot(dcomp, aes(date, contrib, fill = book)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.35) +
  geom_col(width = 22, colour = NA) +
  geom_line(data = dep, aes(date, g), inherit.aes = FALSE,
            colour = "grey15", linewidth = 0.3, alpha = 0.75) +
  scale_fill_manual(values = c("Domestic currency (MN)" = "firebrick2",
                               "Foreign currency (ME), sol value" = "steelblue3")) +
  x_scale_agg +
  scale_y_continuous(expand = expansion(mult = 0.08)) +
  labs(y = "Contribution to monthly deposit growth (pp)",
       title = "Which Currency Book Moves? Decomposition of Monthly Deposit Growth",
       subtitle = "Bars sum to total deposit growth (black line)",
       caption = cap_cur) +
  base_theme

save_fig("fig_deposits_fd_currency.pdf", p_cur, 4.6)

# ── Figure 2: monthly growth (bars) against 12-month growth (line) ──────────
cap_12 <- wrap_cap(
  SRC_AGG,
  "Bars are 100 x the monthly change in log deposits; the line is 100 x the change in log deposits over twelve months. Both are raw -- ",
  "no seasonal adjustment is applied. The 12-month difference nets out any fixed calendar-month pattern on its own, and the calendar-month ",
  "pattern in these data is weak in any case (the spread of calendar-month means is well inside the month-to-month noise). ",
  "Read the two together: a bar that leaves no mark on the line was reversed within the year and is a level blip rather than a persistent ",
  "funding shift. Average monthly growth is ", sprintf("%.2f", mean(dep$g, na.rm = TRUE)),
  "%, so the series drifts up and a shock means a large deviation from that drift, not merely a positive month."
)

p_12 <- ggplot(dep, aes(date)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.35) +
  geom_col(aes(y = g, fill = "Monthly growth"), width = 22, colour = NA) +
  geom_line(aes(y = g12, colour = "12-month growth"), linewidth = 0.55) +
  scale_fill_manual(values = c("Monthly growth" = "grey60")) +
  scale_colour_manual(values = c("12-month growth" = "firebrick2")) +
  x_scale_agg +
  scale_y_continuous(expand = expansion(mult = 0.08)) +
  labs(y = "Growth (log points, %)",
       title = "Deposit Growth, Monthly and Over Twelve Months",
       subtitle = "Bars: monthly log growth. Line: 12-month log growth. No seasonal adjustment",
       caption = cap_12) +
  base_theme

save_fig("fig_deposits_growth_yoy.pdf", p_12, 4.6)

# ══ Part B: departamento panel (figure 3) ════════════════════════════════════
br <- fread(BRANCH_GZ)
br[, report_date := as.IDate(report_date)]
br <- br[is_foreign == FALSE]

tot <- br[, .(tot = sum(dep_total)), by = .(departamento, report_date)][
  order(departamento, report_date)]
tot[, g12 := 100 * (log(tot) - log(shift(tot, 12))), by = departamento]

nat <- br[, .(tot = sum(dep_total)), by = report_date][order(report_date)]
nat[, g12_nat := 100 * (log(tot) - log(shift(tot, 12)))]

message(sprintf("Branch panel: %d departamentos, %d months, %s to %s.",
                uniqueN(tot$departamento), uniqueN(tot$report_date),
                format(min(tot$report_date), "%b-%Y"), format(max(tot$report_date), "%b-%Y")))

# Panels ordered by deposit size, Lima first.
ord <- tot[, .(s = mean(tot)), by = departamento][order(-s), departamento]

pan <- merge(tot[!is.na(g12)], nat[, .(report_date, g12_nat)], by = "report_date")
pan[, dept_f := factor(departamento, levels = ord)]

cap_pan <- wrap_cap(
  "Source: SBS Boletin cuadro B-2358 (deposits by office and bank), branch panel ",
  format(min(pan$report_date), "%b-%Y"), " - ", format(max(pan$report_date), "%b-%Y"),
  ". 23 private banks; Banco de la Nacion, cajas municipales and cajas rurales are not in this table. ",
  "Twelve-month log growth of total deposits in each departamento (red), against the same for the 23-bank national total (grey), ",
  "repeated in every panel. Panels are ordered by deposit size, Lima first; note the free vertical scale on each. ",
  "The national line is the dominant feature of almost every panel -- the 2020-21 COVID surge and the 2022-23 contraction appear ",
  "everywhere at once. Departamento-specific departures are largest in the smallest regions and are short-lived. Deposits are booked ",
  "where the account sits, so corporate balances held at Lima head offices are attributed to Lima, not to the region of activity."
)

p_pan <- ggplot(pan, aes(report_date)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.25) +
  geom_line(aes(y = g12_nat), colour = "grey65", linewidth = 0.35) +
  geom_line(aes(y = g12), colour = "firebrick2", linewidth = 0.4) +
  facet_wrap(~ dept_f, ncol = 5, scales = "free_y") +
  scale_x_date(date_breaks = "6 years", date_labels = "%Y", expand = c(0.02, 0)) +
  labs(y = "12-month deposit growth (log points, %)",
       title = "Deposit Growth by Departamento Against the National Series",
       subtitle = "Red: departamento. Grey: 23-bank national total, same line in every panel",
       caption = cap_pan) +
  base_theme +
  theme(axis.text = element_text(size = 5.5))

save_fig("fig_regional_growth_panels.pdf", p_pan, 6.6)

message("\nFigures written to ", fig_dir)
