# ==============================================================================
# peru_dollarization2.R -- Used vs unused credit and dollarization, RCC sample
# ==============================================================================
# DATA   data/raw/muestra_anonima_base2015_v2.parquet
#        Firm x institution x currency x segment x product balances from the SBS
#        Reporte Crediticio Consolidado (RCC), month-ends 2010-09 .. 2023-12.
#
# OUTPUTS
#   Figure  output/figures/fig_used_unused_by_currency.pdf   (1) area plot
#   Figure  output/figures/fig_unused_by_producto.pdf        (5) bar plot
#   Log     output/logs/peru_dollarization2.log              (2)-(4) tables / stats
#
# DEFINITIONS
#   used   = credito_utilizado   (drawn credit, all five credito_* balances)
#   unused = linea_no_utilizada  (undrawn credit lines only; credito_otorgado_no_desembolsado excluded)
#   currency: moneda_codigo "1" = soles, "2" = foreign currency (dollars). Code
#     "3" is undefined in the SBS manual (439 rows, all sovereign) and is dropped.
#   tipo_empresa: blank = other / private, X = foreign firm, E = State
#     enterprise, M = Ministry.
#
# Usage:  Rscript code/peru_dollarization2.R
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(forcats)
})

PARQUET  <- "data/raw/muestra_anonima_base2015_v2.parquet"
FIGDIR   <- "output/figures"
LOG      <- "output/logs/peru_dollarization2.log"
SNAPSHOT <- as.Date("2015-12-31")
dir.create(FIGDIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(LOG), recursive = TRUE, showWarnings = FALSE)

con <- file(LOG, open = "wt"); sink(con, split = TRUE)
on.exit({ sink(); close(con) }, add = TRUE)
cat("=== peru_dollarization2.R ===\n", format(Sys.time()), "\n\n")
options(width = 140, dplyr.width = Inf, pillar.width = 140)

PAL  <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a", yellow = "#eda100")
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
      strip.text        = element_text(colour = INK[["primary"]], face = "bold", hjust = 0),
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

# Base pdf device with ISOLatin1: cairo_pdf can fail silently on this machine
# (see 04_figures_unused_credit.R). Every write is verified.
save_fig <- function(p, stem, w = 7.2, h = 4.3) {
  f <- file.path(FIGDIR, paste0(stem, ".pdf"))
  ggsave(f, p, width = w, height = h,
         device = function(filename, ...)
           grDevices::pdf(file = filename, encoding = "ISOLatin1", ...))
  if (!file.exists(f) || file.size(f) < 1000) stop("figure was not written: ", f)
  cat(sprintf("  wrote %s.pdf (%d bytes)\n", stem, file.size(f)))
}

# ---- 0. Load ------------------------------------------------------------------
cols <- c("fecha_reporte", "identificador", "entidad", "tipo_empresa", "moneda_codigo",
          "producto", "credito_utilizado", "credito_vigente", "linea_no_utilizada")
raw <- read_parquet(PARQUET, col_select = all_of(cols)) %>%
  mutate(moneda_codigo = as.character(moneda_codigo),
         producto      = as.character(producto))

cat(sprintf("rows: %s | firms: %s | %s .. %s\n",
            format(nrow(raw), big.mark = ","), format(n_distinct(raw$identificador), big.mark = ","),
            min(raw$fecha_reporte), max(raw$fecha_reporte)))
cat(sprintf("dropping moneda_codigo == '3' (undefined): %d rows\n\n",
            sum(raw$moneda_codigo == "3")))

CUR_LAB  <- c("1" = "Domestic currency (soles)", "2" = "Foreign currency (dollars)")
FIRM_LAB <- c("private" = "Private / other", "X" = "Foreign firm",
              "E" = "State enterprise", "M" = "Ministry")

# A single 2010-09 row (entidad 365, soles) reports S/294.88bn of drawn credit:
# ~60x the next-largest balance in the whole panel and a suspiciously round
# number. Treated as a data error and excluded from every output below; the
# exclusion is logged. It does not touch the December 2015 snapshot.
OUTLIER_CAP <- 50e9
outlier <- raw %>% filter(credito_utilizado > OUTLIER_CAP)
cat(sprintf("excluding %d row(s) with credito_utilizado > %.0e:\n", nrow(outlier), OUTLIER_CAP))
print(as.data.frame(outlier %>% select(fecha_reporte, identificador, entidad, moneda_codigo, credito_utilizado)))
cat("\n")
raw <- raw %>% filter(credito_utilizado <= OUTLIER_CAP)

d <- raw %>%
  filter(moneda_codigo %in% c("1", "2")) %>%
  mutate(currency = factor(CUR_LAB[moneda_codigo], levels = CUR_LAB),
         firm_type = factor(FIRM_LAB[if_else(tipo_empresa == "", "private", tipo_empresa)],
                            levels = FIRM_LAB))
stopifnot(!anyNA(d$firm_type), !anyNA(d$currency))

snap <- d %>% filter(fecha_reporte == SNAPSHOT)
stopifnot(nrow(snap) > 0)

pct <- function(x) 100 * x / sum(x)
bn  <- function(x) x / 1e9

# ---- 1. Area plot: used and unused credit by currency --------------------------
ts <- d %>% 
  group_by(fecha_reporte, currency) %>%
  summarise(Used = sum(credito_utilizado), Unused = sum(linea_no_utilizada), .groups = "drop") %>%
  pivot_longer(c(Used, Unused), names_to = "kind", values_to = "amount") %>%
  mutate(kind = factor(kind, levels = c("Unused", "Used")),   # Used at the base
         amount = bn(amount))

p1 <- ggplot(ts, aes(fecha_reporte, amount, fill = kind)) +
  geom_area(alpha = 0.95, colour = SURF, linewidth = 0.15) +
  facet_wrap(~ currency, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(Used = PAL[["blue"]], Unused = PAL[["orange"]]),
                    breaks = c("Used", "Unused")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Used and unused credit, by currency",
       subtitle = "Stacked: drawn credit plus undrawn lines; private and foreign firms only (excl. State enterprises and Ministries)",
       y = "S/ Billions",
       caption = "Source: SBS RCC") +
  theme_fig()
save_fig(p1, "fig_used_unused_by_currency", h = 6)

# ---- 2. Unused credit by type of firm, December 2015 ----------------------------
cat("\n=== (2) Unused credit by type of firm, ", format(SNAPSHOT), " ===\n", sep = "")
t2 <- snap %>%
  group_by(firm_type, currency) %>%
  summarise(unused = sum(linea_no_utilizada), .groups = "drop") %>%
  pivot_wider(names_from = currency, values_from = unused, values_fill = 0) %>%
  left_join(snap %>% group_by(firm_type) %>%
              summarise(n_firms = n_distinct(identificador),
                        n_firms_unused = n_distinct(identificador[linea_no_utilizada > 0]),
                        .groups = "drop"),
            by = "firm_type") %>%
  mutate(across(all_of(unname(CUR_LAB)), bn),
         `Share of soles unused (%)`   = pct(`Domestic currency (soles)`),
         `Share of foreign unused (%)` = pct(`Foreign currency (dollars)`)) %>%
  select(firm_type, n_firms, n_firms_unused, everything())
print(as.data.frame(t2), digits = 3, row.names = FALSE)

# ---- 3. Firm-type composition of credit within each currency, Dec 2015 ----------
cat("\n=== (3) Share of used credit in each currency held by each firm type, ",
    format(SNAPSHOT), " ===\n", sep = "")
cat("Columns sum to 100 within currency.\n\n")
t3 <- snap %>%
  group_by(currency, firm_type) %>%
  summarise(used = sum(credito_utilizado), .groups = "drop") %>%
  group_by(currency) %>% mutate(share = pct(used)) %>% ungroup() %>%
  select(-used) %>%
  pivot_wider(names_from = currency, values_from = share, values_fill = 0)
print(as.data.frame(t3), digits = 3, row.names = FALSE)
cat("\nSame, on unused credit:\n\n")
t3b <- snap %>%
  group_by(currency, firm_type) %>%
  summarise(unused = sum(linea_no_utilizada), .groups = "drop") %>%
  group_by(currency) %>% mutate(share = pct(unused)) %>% ungroup() %>%
  select(-unused) %>%
  pivot_wider(names_from = currency, values_from = share, values_fill = 0)
print(as.data.frame(t3b), digits = 3, row.names = FALSE)

# ---- 4. Summary stat: dollar loans to foreign firms -----------------------------
cat("\n=== (4) Share of dollar (foreign-currency) credit going to foreign firms ===\n")
share_x <- function(dat, var) {
  x <- dat %>% filter(currency == CUR_LAB[["2"]])
  sum(x[[var]][x$firm_type == FIRM_LAB[["X"]]]) / sum(x[[var]])
}
cat(sprintf("December 2015, used credit:   %5.2f%%\n", 100 * share_x(snap, "credito_utilizado")))
cat(sprintf("December 2015, unused credit: %5.2f%%\n", 100 * share_x(snap, "linea_no_utilizada")))
cat(sprintf("All months pooled, used:      %5.2f%%\n", 100 * share_x(d, "credito_utilizado")))
cat("\nBy year-end, used foreign-currency credit:\n")
d %>% filter(format(fecha_reporte, "%m") == "12", currency == CUR_LAB[["2"]]) %>%
  group_by(year = format(fecha_reporte, "%Y")) %>%
  summarise(`share to foreign firms (%)` =
              100 * sum(credito_utilizado[firm_type == FIRM_LAB[["X"]]]) / sum(credito_utilizado),
            .groups = "drop") %>%
  as.data.frame() %>% print(digits = 3, row.names = FALSE)

# ---- 5. Bar plot: composition of unused credit by producto ----------------------
cat("\n=== (5) Composition of unused credit by producto, ", format(SNAPSHOT), " ===\n", sep = "")
prod_tab <- snap %>%
  group_by(producto, currency) %>%
  summarise(unused = sum(linea_no_utilizada), .groups = "drop") %>%
  mutate(share = pct(unused)) %>%
  filter(unused > 0)
tot <- prod_tab %>% group_by(producto) %>% summarise(share = sum(share), .groups = "drop") %>%
  arrange(desc(share))
print(as.data.frame(tot), digits = 3, row.names = FALSE)

prod_tab <- prod_tab %>% mutate(producto = factor(producto, levels = rev(tot$producto)))
p5 <- ggplot(prod_tab, aes(share, producto, fill = currency)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(PAL[["blue"]], PAL[["orange"]]), breaks = unname(CUR_LAB)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Composition of unused credit, by product",
       subtitle = paste0("Share of total unused credit, ", format(SNAPSHOT, "%B %Y"),
                         "; bars split by currency"),
       x = "Share of total unused credit (%)", y = NULL,
       caption = "Source: SBS RCC") +
  theme_fig() +
  theme(panel.grid.major.x = element_line(colour = "#e6e5e0", linewidth = 0.3),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_text(colour = INK[["secondary"]], size = 9))
save_fig(p5, "fig_unused_by_producto", h = 4.8)

cat("\nDone.\n")
