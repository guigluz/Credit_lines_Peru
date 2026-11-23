# =========================================================
# Initial analysis of the loan data
#
# Sample: muestra_anonima_base2015_v2.parquet
#   10,000 firms, 2010-09 to 2023-12 (160 months), firm x bank x product x
#   currency x month rows.
#
# Every plot is built and printed. Only the plots listed in SAVE_PLOTS are
# written to output/figures/ (as .pdf).
# =========================================================

# ---- Options ----------------------------------------------------------------

# Plots to save. Leave empty to save nothing; add any of the names below.
#   "n_banks_2023"        Number of banks per firm, Dec 2023
#   "n_products_2023"     Number of products per firm, Dec 2023
#   "n_products_all"      Number of products per firm, entire sample
#   "used_vs_unused"      Aggregate used vs unused credit, monthly
SAVE_PLOTS <- c("n_products_all", "n_banks_2023")

SNAPSHOT_DATE <- as.Date("2023-12-31")
FIGDIR        <- "output/figures"

# ---- Packages ---------------------------------------------------------------

library(arrow)
library(tidyverse)
library(data.table)
library(scales)
source("code/00_palette.R")     # LANCET, lancet_n()

# ---- Figure style -----------------------------------------------------------

PAL  <- LANCET   # project Lancet palette (code/00_palette.R)
INK  <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#8a8880")
SURF <- "white"

theme_fig <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background    = element_rect(fill = SURF, colour = NA),
      panel.background   = element_rect(fill = SURF, colour = NA),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#e6e5e0", linewidth = 0.3),
      axis.line.x        = element_line(colour = "#c9c8c2", linewidth = 0.3),
      axis.ticks         = element_blank(),
      axis.title         = element_text(colour = INK[["secondary"]], size = base_size - 1),
      axis.text          = element_text(colour = INK[["secondary"]], size = base_size - 1),
      plot.title         = element_text(colour = INK[["primary"]], size = base_size + 2,
                                        face = "bold", margin = margin(b = 2)),
      plot.subtitle      = element_text(colour = INK[["secondary"]], size = base_size - 1,
                                        margin = margin(b = 8)),
      plot.caption       = element_text(colour = INK[["muted"]], size = base_size - 2.5,
                                        hjust = 0, margin = margin(t = 8)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_blank(),
      legend.text        = element_text(colour = INK[["secondary"]], size = base_size - 1),
      legend.key.size    = unit(9, "pt"),
      plot.margin        = margin(10, 14, 8, 10)
    )
}

# Base pdf device: cairo_pdf fails silently on this machine (see
# code/04_figures_unused_credit.R). All figure text is ASCII.
save_fig <- function(p, stem, w = 7.2, h = 4.3) {
  f <- file.path(FIGDIR, paste0("fig_desc_", stem, ".pdf"))
  ggsave(f, p, width = w, height = h, bg = "white",
         device = function(filename, ...)
           grDevices::pdf(file = filename, encoding = "ISOLatin1", ...))
  if (!file.exists(f) || file.size(f) < 1000) stop("figure was not written: ", f)
  cat(sprintf("  wrote %s (%d bytes)\n", f, file.size(f)))
}

plots <- list()

# Bar chart of a discrete count per firm (number of banks / products).
count_bar <- function(dt, var, title, x_lab, subtitle, x_breaks = pretty_breaks()) {
  ggplot(dt, aes(x = .data[[var]])) +
    geom_bar(fill = PAL[["navy"]], width = 0.8) +
    scale_x_continuous(breaks = x_breaks, expand = expansion(add = 0.6)) +
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, subtitle = subtitle, x = x_lab, y = "Number of firms") +
    theme_fig()
}

# ---- Data -------------------------------------------------------------------

sample <- read_parquet("data/raw/muestra_anonima_base2015_v2.parquet")
setDT(sample)
glimpse(sample)

sample[, cantidad_empresas := as.integer(cantidad_empresas)]

n_firms_snap <- uniqueN(sample[fecha_reporte == SNAPSHOT_DATE, identificador])
n_firms_all  <- uniqueN(sample$identificador)
snap_lab     <- format(SNAPSHOT_DATE, "%B %Y")

# ---- Number of banks per firm -----------------------------------------------

N_BANKS_MAX <- 10   # x-axis truncation for the lenders plot

n_banks_snap <- sample[fecha_reporte == SNAPSHOT_DATE,
                       .(cantidad_empresas = as.integer(round(mean(cantidad_empresas)))),
                       by = identificador]
n_above_max  <- n_banks_snap[cantidad_empresas > N_BANKS_MAX, .N]

plots$n_banks_2023 <- count_bar(
  n_banks_snap,
  "cantidad_empresas",
  title    = "Number of lenders per firm",
  x_lab    = "Number of financial institutions",
  subtitle = sprintf("%s, %s firms", snap_lab, comma(n_firms_snap)),
  x_breaks = 1:N_BANKS_MAX
) +
  coord_cartesian(xlim = c(1, N_BANKS_MAX)) +
  labs(caption = sprintf("Axis truncated at %d lenders; %s firm(s) with more are not shown.",
                         N_BANKS_MAX, comma(n_above_max)))

# ---- Number of products per firm --------------------------------------------

plots$n_products_2023 <- count_bar(
  sample[fecha_reporte == SNAPSHOT_DATE, .(n_products = uniqueN(producto)), by = identificador],
  "n_products",
  title    = "Number of credit products per firm",
  x_lab    = "Number of distinct products",
  subtitle = sprintf("%s, %s firms", snap_lab, comma(n_firms_snap))
)

plots$n_products_all <- count_bar(
  sample[, .(n_products = uniqueN(producto)), by = identificador],
  "n_products",
  title    = "Number of credit products per firm",
  x_lab    = "Number of distinct products ever held",
  subtitle = sprintf("Pooled 2010-2023, %s firms", comma(n_firms_all))
)

# ---- Used vs unused credit --------------------------------------------------

# Sep 2010 is dropped: one firm reports ~S/295bn of used credit that month
# (vs ~S/14bn for the whole sample in a typical month), an apparent data error.
TS_START <- as.Date("2010-10-31")

used_unused <- sample[fecha_reporte >= TS_START,
                      .(Used   = sum(credito_utilizado,  na.rm = TRUE),
                          Unused = sum(no_utilizado_total, na.rm = TRUE)),
                      by = fecha_reporte] %>%
  pivot_longer(c(Used, Unused), names_to = "type", values_to = "amount") %>%
  mutate(type = factor(type, levels = c("Unused", "Used")))

plots$used_vs_unused <- ggplot(used_unused,
                               aes(x = fecha_reporte, y = amount / 1e9, fill = type)) +
  geom_area(colour = SURF, linewidth = 0.2) +
  scale_fill_manual(values = c(Used = PAL[["navy"]], Unused = PAL[["red"]]),
                    breaks = c("Used", "Unused")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = label_comma(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Used and unused credit",
       subtitle = sprintf("Monthly totals across %s sample firms", comma(n_firms_all)),
       x = NULL, y = "Billions",
       caption  = paste("Sums across all currencies as reported. Unused = no_utilizado_total.",
                        "Sample starts Oct 2010 (Sep 2010 excluded: single-firm reporting outlier).",
                        sep = "\n")) +
  theme_fig()

# ---- Print and save ---------------------------------------------------------

walk(plots, print)

unknown <- setdiff(SAVE_PLOTS, names(plots))
if (length(unknown)) stop("Unknown plot name(s) in SAVE_PLOTS: ", paste(unknown, collapse = ", "))

if (length(SAVE_PLOTS)) {
  dir.create(FIGDIR, recursive = TRUE, showWarnings = FALSE)
  walk(SAVE_PLOTS, ~ save_fig(plots[[.x]], .x))
} else {
  cat("SAVE_PLOTS is empty: no figures written.\n")
}
