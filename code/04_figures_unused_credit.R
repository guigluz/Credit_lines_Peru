# ==============================================================================
# 04_figures_unused_credit.R -- Figures on used vs unused credit, Banca Multiple
# ==============================================================================
# Figure 1  Used vs unused credit, stacked area
#           used   = creditos directos, from the Series Estadisticas extract
#                    data/raw/Reporte(01-2001 - 09-2022).csv, entity "Banca Multiple"
#           unused = creditos indirectos, from data/processed/b2322_unused_credit_panel.csv
# Figure 2  Composition of unused (indirect) credit by category, stacked area
#
# Units: both sources are in miles de S/; plotted in billions of soles (/1e6).
#
# THE 2010-07 BREAK. The "Lineas de credito no utilizadas y creditos concedidos
# no desembolsados" column does not exist before 2010-07: it was introduced with
# the broader definition of creditos indirectos in Res. SBS 11356-2008. Total
# indirect credit therefore jumps S/23.75bn -> S/63.80bn between 2010-06 and
# 2010-07 for definitional reasons alone. Both figures mark this with a dashed
# rule labelled "Basel III reporting change"; nothing is interpolated across it
# and the missing category stays NA rather than being read as zero.
#
# SYSTEM AGGREGATE. The published "TOTAL BANCA MULTIPLE" row is used where it
# exists. It is absent from the ten 2001 workbooks (Jan-Oct 2001), where the
# aggregate is summed from the bank rows instead and flagged as such in the
# accompanying data tables.
#
# Palette: categorical slots assigned in a fixed order that was validated for
# colour-vision separation on the adjacent pairlist (stacked fills) in light
# mode. Three slots (aqua, yellow, magenta) fall below 3:1 contrast on the chart
# surface; they carry the three smallest categories in figure 2, so they are
# legible only via the legend. Add direct labels if those categories ever need
# to be read off the figure itself.
#
# Usage:  Rscript code/04_figures_unused_credit.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(stringr)
})

PANEL  <- "data/processed/b2322_unused_credit_panel.csv"
REPORTE<- "data/raw/Reporte(01-2001 - 09-2022).csv"
FIGDIR <- "output/figures"
LOG    <- "output/logs/04_figures_unused_credit.log"
dir.create(FIGDIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(LOG), recursive = TRUE, showWarnings = FALSE)

con <- file(LOG, open = "wt"); sink(con, split = TRUE); on.exit({sink(); close(con)}, add = TRUE)
cat("=== 04_figures_unused_credit.R ===\n", format(Sys.time()), "\n\n")

BREAK_DATE <- as.Date("2010-07-31")
BREAK_LAB  <- "Basel III reporting change"

# Validated categorical slots, assigned by entity in a fixed order (never cycled).
PAL <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a",
         yellow = "#eda100", magenta = "#e87ba4")
INK  <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#8a8880")
SURF <- "#fcfcfb"

theme_fig <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = SURF, colour = NA),
      panel.background  = element_rect(fill = SURF, colour = NA),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x= element_blank(),
      panel.grid.major.y= element_line(colour = "#e6e5e0", linewidth = 0.3),
      axis.line.x       = element_line(colour = "#c9c8c2", linewidth = 0.3),
      axis.ticks        = element_blank(),
      axis.title.x      = element_blank(),
      axis.title.y      = element_text(colour = INK[["secondary"]], size = base_size - 1),
      axis.text         = element_text(colour = INK[["secondary"]], size = base_size - 1),
      plot.title        = element_text(colour = INK[["primary"]], size = base_size + 2,
                                       face = "bold", margin = margin(b = 2)),
      plot.subtitle     = element_text(colour = INK[["secondary"]], size = base_size - 1,
                                       margin = margin(b = 8)),
      plot.caption      = element_text(colour = INK[["muted"]], size = base_size - 2.5,
                                       hjust = 0, margin = margin(t = 8)),
      legend.position   = "top",
      legend.title      = element_blank(),
      legend.text       = element_text(colour = INK[["secondary"]], size = base_size - 1),
      legend.key.size   = unit(9, "pt"),
      plot.margin       = margin(10, 14, 8, 10)
    )
}

# Figure devices. capabilities("cairo") reports TRUE on this machine but the
# cairo DLL fails to load (missing X11 libXrender) and cairo_pdf then WARNS
# rather than erroring, silently producing no file. All figure text is ASCII, so
# the base pdf device with ISOLatin1 is used unconditionally -- the same approach
# as code/peru_dollarization.R -- and every write is verified to have landed.
save_fig <- function(p, stem, w = 7.2, h = 4.3) {
  f <- file.path(FIGDIR, paste0(stem, ".pdf"))
  ggsave(f, p, width = w, height = h,
         device = function(filename, ...)
           grDevices::pdf(file = filename, encoding = "ISOLatin1", ...))
  if (!file.exists(f) || file.size(f) < 1000)
    stop("figure was not written: ", f)
  cat(sprintf("  wrote %s.pdf (%d bytes)\n", stem, file.size(f)))
}

# ---- system aggregate from the indirect panel --------------------------------
panel <- read_csv(PANEL, show_col_types = FALSE)

lvl_cols <- c("level_lineas_no_utilizadas_thousand_pen",
              "level_cartas_fianza_thousand_pen",
              "level_cartas_credito_thousand_pen",
              "level_aceptaciones_bancarias_thousand_pen",
              "level_avales_thousand_pen")

published <- panel %>% filter(row_kind == "system_total") %>%
  select(report_date, total_indirect_thousand_pen, all_of(lvl_cols)) %>%
  mutate(system_source = "published_total_row")

summed <- panel %>% filter(row_kind == "bank") %>%
  group_by(report_date) %>%
  # n_lineas MUST be computed before the across() below: summarise() evaluates
  # its arguments in order, so after across() the column name refers to the new
  # scalar sum (never NA) and the "does this category exist yet" test would
  # silently turn absent categories into zeros.
  summarise(n_lineas = sum(!is.na(level_lineas_no_utilizadas_thousand_pen)),
            total_indirect_thousand_pen = sum(total_indirect_thousand_pen, na.rm = TRUE),
            across(all_of(lvl_cols), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(level_lineas_no_utilizadas_thousand_pen =
           ifelse(n_lineas == 0, NA_real_, level_lineas_no_utilizadas_thousand_pen),
         system_source = "summed_bank_rows") %>%
  select(-n_lineas)

sys <- bind_rows(published, summed %>% anti_join(published, by = "report_date")) %>%
  arrange(report_date)

cat(sprintf("system months: %d (%s .. %s) | published rows: %d | summed: %d\n",
            nrow(sys), min(sys$report_date), max(sys$report_date),
            sum(sys$system_source == "published_total_row"),
            sum(sys$system_source == "summed_bank_rows")))

# ---- used credit: Series Estadisticas ----------------------------------------
# UTF-32LE, no BOM; columns renamed by position because accented names are
# unreliable under a C locale.
rep_raw <- read_csv(REPORTE, locale = readr::locale(encoding = "UTF-32LE"),
                    col_types = cols(.default = col_character()), name_repair = "minimal")
names(rep_raw)[1:8] <- c("entity", "fecha", "cred_total", "cred_mn", "cred_me",
                         "dep_total", "dep_mn", "dep_me")
used <- rep_raw %>%
  filter(str_detect(str_squish(entity), "^Banca M")) %>%    # accent-safe match
  transmute(report_date = as.Date(str_squish(fecha), "%d/%m/%Y"),
            used_thousand_pen = as.numeric(str_squish(cred_total))) %>%
  filter(!is.na(report_date)) %>% arrange(report_date)
stopifnot(nrow(used) > 0, !any(is.na(used$used_thousand_pen)))
cat(sprintf("used-credit months: %d (%s .. %s)\n\n",
            nrow(used), min(used$report_date), max(used$report_date)))

# ==============================================================================
# FIGURE 1 -- used vs unused credit
# ==============================================================================
f1 <- used %>%
  inner_join(sys %>% select(report_date, unused_thousand_pen = total_indirect_thousand_pen,
                            system_source),
             by = "report_date") %>%
  transmute(report_date, system_source,
            `Used credit (direct)`     = used_thousand_pen  / 1e6,
            `Unused credit (indirect)` = unused_thousand_pen / 1e6)

f1_long <- f1 %>%
  pivot_longer(c(`Used credit (direct)`, `Unused credit (indirect)`),
               names_to = "series", values_to = "bn") %>%
  mutate(series = factor(series, levels = c("Used credit (direct)",
                                            "Unused credit (indirect)")))

ymax1 <- f1 %>% mutate(t = `Used credit (direct)` + `Unused credit (indirect)`) %>% pull(t) %>% max()

p1 <- ggplot(f1_long, aes(report_date, bn, fill = series)) +
  # reverse = TRUE puts the first factor level at the BASE of the stack, so the
  # lead series anchors the baseline instead of floating on top
  geom_area(colour = SURF, linewidth = 0.35,
            position = position_stack(reverse = TRUE)) +   # surface gap between fills
  geom_vline(xintercept = BREAK_DATE, linetype = "dashed",
             colour = INK[["secondary"]], linewidth = 0.4) +
  annotate("text", x = BREAK_DATE - 60, y = ymax1 * 0.97, label = BREAK_LAB,
           hjust = 1, vjust = 1, size = 2.9, colour = INK[["secondary"]]) +
  scale_fill_manual(values = unname(PAL[c("blue", "orange")])) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", expand = expansion(c(0.01, 0.01))) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                     expand = expansion(c(0, 0.05))) +
  labs(title = "Used and Unused Credit in the Peruvian Banking System",
       subtitle = "Banca Multiple, monthly, billions of soles",
       y = "S/ billions",
       caption = paste0(
         "Used credit = creditos directos (SBS Series Estadisticas). Unused credit = creditos indirectos (SBS B-2322),\n",
         "which comprises unused credit lines and undisbursed commitments plus guarantees, letters of credit and acceptances.\n",
         "Unused credit lines enter the definition only from July 2010 (Res. SBS 11356-2008), hence the dashed rule; the\n",
         "series is not comparable across it. Used credit ends 2022-09, the last month of the Series Estadisticas extract.")) +
  theme_fig()

save_fig(p1, "fig_used_vs_unused_credit")

# ==============================================================================
# FIGURE 2 -- composition of unused (indirect) credit
# ==============================================================================
# Stack order, BASE -> TOP. Unused credit lines sit at the TOP: they dominate
# the total, so putting them on top lets the four small categories rest on a
# flat baseline where they stay readable.
CATS <- c(
  level_cartas_fianza_thousand_pen          = "Letters of guarantee (cartas fianza)",
  level_cartas_credito_thousand_pen         = "Letters of credit",
  level_aceptaciones_bancarias_thousand_pen = "Bankers' acceptances",
  level_avales_thousand_pen                 = "Guarantees (avales)",
  level_lineas_no_utilizadas_thousand_pen   = "Unused credit lines & undisbursed commitments")

# Colour follows the CATEGORY, never its position in the stack -- so reordering
# the stack does not repaint the series. The resulting adjacent pairs were
# re-validated for colour-vision separation after the reorder.
CAT_COL <- c(
  "Unused credit lines & undisbursed commitments" = unname(PAL["blue"]),
  "Letters of guarantee (cartas fianza)"          = unname(PAL["orange"]),
  "Letters of credit"                             = unname(PAL["aqua"]),
  "Bankers' acceptances"                          = unname(PAL["yellow"]),
  "Guarantees (avales)"                           = unname(PAL["magenta"]))

f2 <- sys %>% select(report_date, system_source, all_of(names(CATS))) %>%
  pivot_longer(all_of(names(CATS)), names_to = "cat", values_to = "thousand_pen") %>%
  mutate(category = factor(unname(CATS[cat]), levels = unname(CATS)),
         bn = thousand_pen / 1e6)

ymax2 <- f2 %>% group_by(report_date) %>% summarise(t = sum(bn, na.rm = TRUE)) %>% pull(t) %>% max()

p2 <- ggplot(f2, aes(report_date, bn, fill = category)) +
  geom_area(colour = SURF, linewidth = 0.35, na.rm = TRUE,
            position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = BREAK_DATE, linetype = "dashed",
             colour = INK[["secondary"]], linewidth = 0.4) +
  annotate("text", x = BREAK_DATE - 60, y = ymax2 * 0.97, label = BREAK_LAB,
           hjust = 1, vjust = 1, size = 2.9, colour = INK[["secondary"]]) +
  scale_fill_manual(values = CAT_COL, drop = FALSE) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", expand = expansion(c(0.01, 0.01))) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                     expand = expansion(c(0, 0.05))) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE)) +
  labs(title = "Composition of Unused Credit in the Peruvian Banking System",
       subtitle = "Creditos indirectos by category, Banca Multiple, monthly, billions of soles",
       y = "S/ billions",
       caption = paste0(
         "Source: SBS cuadro B-2322. Category levels are derived as the published share times the published total.\n",
         "Unused credit lines and undisbursed commitments are not reported before July 2010 (Res. SBS 11356-2008):\n",
         "the category is left missing there rather than set to zero, so the step at the dashed rule is definitional,\n",
         "not economic."),
       fill = NULL) +
  theme_fig()

save_fig(p2, "fig_unused_credit_composition")

# ---- sanity checks on what was plotted ---------------------------------------
cat("\n--- figure 1 ---\n")
cat(sprintf("months: %d (%s .. %s)\n", nrow(f1), min(f1$report_date), max(f1$report_date)))
cat(sprintf("used  : %.1f -> %.1f S/bn\n", first(f1$`Used credit (direct)`), last(f1$`Used credit (direct)`)))
cat(sprintf("unused: %.1f -> %.1f S/bn\n", first(f1$`Unused credit (indirect)`), last(f1$`Unused credit (indirect)`)))

cat("\n--- figure 2 ---\n")
cat(sprintf("months: %d (%s .. %s)\n", n_distinct(f2$report_date),
            min(f2$report_date), max(f2$report_date)))
cat("category totals at the last month (S/bn):\n")
print(as.data.frame(f2 %>% filter(report_date == max(report_date)) %>%
        select(category, bn)), row.names = FALSE, digits = 4)
cat(sprintf("\nunused-lines category is NA in %d of %d months (all before %s)\n",
            sum(is.na(f2$bn[f2$cat == "level_lineas_no_utilizadas_thousand_pen"])),
            n_distinct(f2$report_date), BREAK_DATE))
cat("\nDone.\n")
