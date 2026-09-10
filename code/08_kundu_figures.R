# ==============================================================================
# 08_kundu_figures.R -- Phase 1 outputs for the Kundu, Park & Vats (2021)
# deposit-shock feasibility exercise.
#
#   Output 1  fig_kundu_disaster_maps.pdf      damages and deaths per capita
#   Output 2  fig_kundu_deposit_share_rank.pdf average deposit share by rank
#   Output 3  tab_kundu_deposit_concentration.tex  + fig_kundu_dep_vs_branch.pdf
#   Output 4  fig_kundu_largest_district_map.pdf   where banks raise deposits
#
# Inputs are the .rds files written by code/07_kundu_clean.R.
# Summary statistics requested alongside each output are printed to stdout and
# also written to output/logs/08_kundu_figures_stats.txt.
#
# Usage:  Rscript code/08_kundu_figures.R
# ==============================================================================


suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(scales)
  library(patchwork); library(kableExtra); library(knitr)
})


# ── Configuration ────────────────────────────────────────────────────────────
IN_DIR    <- "data/processed"
FIG_DIR   <- "output/figures"
TAB_DIR   <- "output/tables"
LOG_DIR   <- "output/logs"
SNAP_LAB  <- "December 2023"      # matches DEPOSIT_SNAPSHOT
MAX_RANK  <- 20                   # x-axis limit for Output 2
BIG_BANK  <- 10                   # "multi-district" = at least this many districts

# Output 1 sample. disaster.rds is event-level and unfiltered, so the window and
# the hazard restriction are set here, next to the figure they produce.
DISASTER_YEARS <- 2010:2023
HAZARD_TYPE    <- "ORIGEN NATURAL"   # NA keeps human-induced events too
YEARS_LAB      <- paste0(min(DISASTER_YEARS), "-", max(DISASTER_YEARS))

for (d in c(FIG_DIR, TAB_DIR, LOG_DIR)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

stats_con <- file(file.path(LOG_DIR, "08_kundu_figures_stats.txt"), open = "wt")
say <- function(...) { cat(..., "\n"); cat(..., "\n", file = stats_con) }
hdr <- function(t) say("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74))

disaster <- readRDS(file.path(IN_DIR, "disaster.rds"))
bd   <- readRDS(file.path(IN_DIR, "kundu_bank_district.rds"))
adm3 <- readRDS(file.path(IN_DIR, "kundu_admin3_sf.rds"))
adm1 <- readRDS(file.path(IN_DIR, "kundu_admin1_sf.rds"))
pop  <- readRDS(file.path(IN_DIR, "kundu_district_population.rds"))

# ── Shared theme, matching 05_figures_deposits.R ──────────────────────────────
base_theme <- theme_bw(base_size = 10, base_family = "serif") +
  theme(panel.grid.minor = element_blank(),
        panel.border     = element_rect(colour = "grey30", linewidth = 0.4),
        plot.title       = element_text(size = 11, face = "bold"),
        plot.subtitle    = element_text(size = 8.5, colour = "grey25"),
        plot.caption     = element_text(size = 7, colour = "grey30", hjust = 0),
        legend.title     = element_text(size = 7.5),
        legend.text      = element_text(size = 7))

map_theme <- base_theme +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(3, "mm"), legend.key.width = unit(12, "mm"))

# Human-readable bank labels. The SBS "raw" strings carry rebrands and stray
# digits, so the display name is fixed here rather than derived.
BANK_LABEL <- c(
  bcp = "Banco de Credito del Peru", bbva_peru = "BBVA Peru",
  interbank = "Interbank", scotiabank = "Scotiabank Peru",
  bif = "BanBif", mibanco = "Mibanco", banco_pichincha = "Banco Pichincha",
  santander_peru = "Banco Santander Peru", citibank = "Citibank del Peru",
  banco_gnb = "Banco GNB Peru", banco_falabella = "Banco Falabella",
  bancom = "Bancom (Banco de Comercio)", bank_of_china = "Bank of China (Peru)",
  banco_icbc = "ICBC Peru Bank", banco_ripley = "Banco Ripley",
  alfin_banco = "Alfin Banco", banco_bci = "Banco BCI Peru",
  compartamos_banco = "Compartamos Banco", santander_consumer = "Santander Consumer",
  banco_efectiva = "Banco Efectiva", banco_cencosud = "Banco Cencosud",
  deutsche_bank = "Deutsche Bank Peru", hsbc_peru = "HSBC Bank Peru")
lab_bank <- function(x) ifelse(is.na(BANK_LABEL[x]), x, BANK_LABEL[x])

titlecase <- function(x) {
  small <- c("de", "del", "la", "las", "los", "y", "el")
  vapply(strsplit(tolower(x), " "), function(w) {
    w2 <- ifelse(w %in% small, w, paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w))))
    w2[1] <- paste0(toupper(substr(w2[1], 1, 1)), substr(w2[1], 2, nchar(w2[1])))
    paste(w2, collapse = " ")
  }, character(1))
}

# ==============================================================================
# OUTPUT 1 -- disaster damages and deaths per capita
# ==============================================================================
hdr("OUTPUT 1  Natural disaster impacts per capita, districts of Peru")

# ── Sample: restrict the event file, then aggregate ──────────────────────────
ev <- disaster[year %in% DISASTER_YEARS]
if (!is.na(HAZARD_TYPE)) ev <- ev[hazard_type == HAZARD_TYPE]
say(sprintf("Event records: %s of %s kept (%s, hazard type %s).",
            format(nrow(ev), big.mark = ","),
            format(nrow(disaster), big.mark = ","), YEARS_LAB,
            if (is.na(HAZARD_TYPE)) "any" else HAZARD_TYPE))

# Property damage is houses destroyed plus houses affected; the two are
# stored separately upstream and combined here.
by_dist <- ev[, .(n_events = .N,
                  deaths = sum(deaths),
                  houses_damaged = sum(houses_destroyed) + sum(houses_affected)),
              by = ubigeo]

# Districts with no qualifying event must survive as zeros, so the map can
# distinguish "nothing recorded" from "not in the data". all.y onto the register
# does that, and drops event ubigeos created after the 2017 census vintage.
dis <- merge(by_dist, pop, by = "ubigeo", all.y = TRUE)
for (v in c("n_events", "deaths", "houses_damaged")) dis[is.na(get(v)), (v) := 0]

# Aggregate to the polygon key (Mazamari/Pangoa share one polygon), then divide,
# so the ratio uses the matching population denominator.
dg <- dis[!is.na(population),
          .(deaths = sum(deaths), houses_damaged = sum(houses_damaged),
            n_events = sum(n_events), population = sum(population)),
          by = .(ubigeo = ubigeo_geo)]
dg[, `:=`(damage_p1k  = 1e3 * houses_damaged / population,
          deaths_p100k = 1e5 * deaths / population)]

map_dat <- merge(adm3, dg, by = "ubigeo", all.x = TRUE)

orphan <- setdiff(unique(ev$ubigeo), pop$ubigeo)
say(sprintf("Dropped at the population join: %d ubigeo(s) absent from the 2017 register (%d records).",
            length(orphan), ev[ubigeo %in% orphan, .N]))
say(sprintf("Window: %s, natural-origin hazards only (INDECI/SINPAD).", YEARS_LAB))
say(sprintf("Districts drawn: %d of %d polygons carry a population denominator.",
            sum(!is.na(map_dat$population)), nrow(map_dat)))
say(sprintf("Dwellings damaged or destroyed, total: %s",
            format(sum(dg$houses_damaged), big.mark = ",")))
say(sprintf("Deaths, total: %s", format(sum(dg$deaths), big.mark = ",")))
say(sprintf("Damage per 1,000 residents  -- median %.2f, p90 %.2f, p99 %.2f, max %.1f",
            median(dg$damage_p1k), quantile(dg$damage_p1k, .9),
            quantile(dg$damage_p1k, .99), max(dg$damage_p1k)))
say(sprintf("Districts with zero recorded dwelling damage: %d (%.1f%%)",
            sum(dg$damage_p1k == 0), 100 * mean(dg$damage_p1k == 0)))
say(sprintf("Districts with zero recorded deaths: %d (%.1f%%)",
            sum(dg$deaths_p100k == 0), 100 * mean(dg$deaths_p100k == 0)))

# Both measures span several orders of magnitude and pile up at zero, so the
# fill is log10 and exact zeros are drawn in grey rather than forced onto the
# scale. Grey therefore reads "nothing recorded", not "small".
# A handful of tiny districts sit three orders of magnitude below the mass of
# the distribution and would otherwise absorb the whole colour ramp, so the
# scale is clipped at the 2nd and 98th percentiles of the positive values and
# out-of-range districts take the end colours.
panel_map <- function(var, title, legend_lab) {
  d <- map_dat
  v <- ifelse(is.na(d[[var]]) | d[[var]] <= 0, NA_real_, d[[var]])
  d[[".v"]] <- v
  lims <- unname(quantile(v, c(0.02, 0.98), na.rm = TRUE))
  ggplot(d) +
    geom_sf(aes(fill = .data[[".v"]]), colour = NA) +
    geom_sf(data = adm1, fill = NA, colour = "grey35", linewidth = 0.12) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, trans = "log10",
                         limits = lims, oob = scales::squish,
                         labels = label_number(accuracy = 0.1, big.mark = ","),
                         na.value = "grey88", name = legend_lab,
                         guide = guide_colourbar(title.position = "top",
                                                 title.hjust = 0.5)) +
    labs(title = title) +
    coord_sf(expand = FALSE) +
    map_theme
}

p1a <- panel_map("damage_p1k",
                 "Property damage",
                 "Dwellings damaged or destroyed per 1,000 residents (log scale)")
p1b <- panel_map("deaths_p100k",
                 "Fatalities",
                 "Deaths per 100,000 residents (log scale)")

fig1 <- (p1a | p1b) +
  plot_annotation(
    title    = "Geographic Distribution of Natural Disaster Impacts in Peru",
    subtitle = sprintf("Cumulative over %s, by distrito. Grey: no event of that kind recorded. Colour scale clipped at the 2nd and 98th percentiles.", YEARS_LAB),
    caption  = paste0(
      "Source: INDECI/SINPAD Emergencias 2003-2025, natural-origin hazards only; ",
      "INEI district population (2017 census vintage).\n",
      "Property damage is the physical count of dwellings destroyed plus dwellings affected; ",
      "INDECI records no monetary loss."),
    theme = theme(plot.title = element_text(size = 12, face = "bold", family = "serif"),
                  plot.subtitle = element_text(size = 9, colour = "grey25", family = "serif"),
                  plot.caption = element_text(size = 7, colour = "grey30", hjust = 0, family = "serif")))

ggsave(file.path(FIG_DIR, "fig_kundu_disaster_maps.pdf"), fig1,
       width = 9, height = 6.2)
say("\n-> output/figures/fig_kundu_disaster_maps.pdf")

# ==============================================================================
# OUTPUT 2 -- average deposit share by within-bank district rank
# ==============================================================================
hdr("OUTPUT 2  Average deposit share by district rank")

banks   <- unique(bd$bank)
n_banks <- length(banks)
multi   <- bd[, .(nd = uniqueN(ubigeo)), by = bank][nd >= BIG_BANK, bank]

# A bank with fewer districts than the rank contributes a zero, so each bank's
# shares sum to one across the full rank grid and the curve is comparable.
grid <- CJ(bank = banks, rank = 1:MAX_RANK)
rk   <- merge(grid, bd[, .(bank, rank, dep_share)], by = c("bank", "rank"), all.x = TRUE)
rk[is.na(dep_share), dep_share := 0]

curve_all   <- rk[, .(share = 100 * mean(dep_share), grp = "All banks"), by = rank]
curve_multi <- rk[bank %in% multi,
                  .(share = 100 * mean(dep_share),
                    grp = sprintf("Banks in >= %d districts", BIG_BANK)), by = rank]
curve <- rbind(curve_all, curve_multi)
curve[, grp := factor(grp, levels = c("All banks", sprintf("Banks in >= %d districts", BIG_BANK)))]

fig2 <- ggplot(curve, aes(rank, share, colour = grp, shape = grp)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.8, fill = "white") +
  scale_x_continuous(breaks = c(1, seq(2, MAX_RANK, 2)), limits = c(1, MAX_RANK)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, .06))) +
  scale_colour_manual(values = c("grey20", "#C0392B")) +
  scale_shape_manual(values = c(21, 24)) +
  labs(title    = "Average Deposit Share by District Rank",
       subtitle = paste0("District #1 refers to the district that raises the greatest amount of ",
                         "deposits for a given bank"),
       x = "District rank within bank", y = "Average deposit share",
       caption = paste0(
         "SBS Boletin B-2358 branch-level deposits, ", SNAP_LAB, "; ", n_banks,
         " private banks. Deposits are aggregated to the distrito of the branch that books\n",
         "the account. A bank present in fewer districts than the rank contributes a zero, ",
         "so each bank's shares sum to 100% across the full rank grid.")) +
  base_theme +
  theme(legend.position = c(0.75, 0.78), legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
        panel.grid.major.x = element_blank())

ggsave(file.path(FIG_DIR, "fig_kundu_deposit_share_rank.pdf"), fig2,
       width = 6.5, height = 4.4)

top1  <- bd[rank == 1, dep_share]
top3  <- bd[rank <= 3, .(s = sum(dep_share)), by = bank]$s
ndist <- bd[, .(nd = uniqueN(ubigeo)), by = bank]$nd

say(sprintf("Number of banks in the sample: %d", n_banks))
say(sprintf("Average number of distritos per bank: %.1f (median %.0f, min %d, max %d)",
            mean(ndist), median(ndist), min(ndist), max(ndist)))
say(sprintf("District #1 deposit share: mean %.1f%%, median %.1f%%", 100 * mean(top1), 100 * median(top1)))
say(sprintf("Top-3 districts combined:  mean %.1f%%, median %.1f%%", 100 * mean(top3), 100 * median(top3)))
say(sprintf("Excluding the %d single-district banks:", sum(ndist == 1)))
mb <- bd[bank %in% multi]
say(sprintf("  District #1 share: mean %.1f%%, median %.1f%%",
            100 * mean(mb[rank == 1, dep_share]), 100 * median(mb[rank == 1, dep_share])))
say(sprintf("  Top-3 combined:    mean %.1f%%, median %.1f%%",
            100 * mean(mb[rank <= 3, sum(dep_share), by = bank]$V1),
            100 * median(mb[rank <= 3, sum(dep_share), by = bank]$V1)))
say("\n-> output/figures/fig_kundu_deposit_share_rank.pdf")

# ==============================================================================
# OUTPUT 3 -- deposit concentration against branch presence
# ==============================================================================
hdr("OUTPUT 3  Deposit concentration and branch presence")

tab <- bd[rank == 1, .(bank, ubigeo, dist_name, prov_name, dep_name,
                       dep_share, n_branches, bank_branches, branch_share,
                       n_districts)]
tab[, `:=`(Bank = lab_bank(bank), District = titlecase(dist_name))]
setorder(tab, -dep_share)

r_pear  <- cor(tab$dep_share, tab$branch_share)
r_spear <- cor(tab$dep_share, tab$branch_share, method = "spearman")
# Five banks operate a single office, so they sit at (100%, 100%) by
# construction and on their own generate most of the raw correlation. The
# multi-branch correlation is the informative one and is reported alongside.
tab[, single_office := bank_branches == 1L]
mb_t    <- tab[single_office == FALSE]
r_pear_m  <- cor(mb_t$dep_share, mb_t$branch_share)
r_spear_m <- cor(mb_t$dep_share, mb_t$branch_share, method = "spearman")

tt <- tab[, .(Bank = fifelse(single_office, paste0(Bank, "$^{a}$"), Bank), `Largest district` = District,
              `Deposits (\\%)` = 100 * dep_share,
              `Branches` = n_branches,
              `All branches`       = bank_branches,
              `Branches (\\%)`   = 100 * branch_share)]

fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
body <- rbind(
  tt[, .(Bank, `Largest district`,
         `Deposits (\\%)` = fmt(`Deposits (\\%)`),
         `Branches`  = as.character(`Branches`),
         `All branches`        = as.character(`All branches`),
         `Branches (\\%)`  = fmt(`Branches (\\%)`))],
  data.table(
    Bank = c("Mean", "Median",
             "Correlation, deposits vs.\\ branches",
             "\\quad excl.\\ single-office banks"),
    `Largest district` = "",
    `Deposits (\\%)` = c(fmt(100 * mean(tab$dep_share)),
                              fmt(100 * median(tab$dep_share)),
                              sprintf("%.2f", r_pear), sprintf("%.2f", r_pear_m)),
    `Branches` = c(fmt(mean(tab$n_branches)),
                              fmt(median(tab$n_branches), 0), "", ""),
    `All branches` = c(fmt(mean(tab$bank_branches)),
                         fmt(median(tab$bank_branches), 0), "", ""),
    `Branches (\\%)` = c(fmt(100 * mean(tab$branch_share)),
                             fmt(100 * median(tab$branch_share)),
                             sprintf("%.2f", r_spear), sprintf("%.2f", r_spear_m))))

tex <- kbl(body, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = c("l", "l", "r", "r", "r", "r"),
    caption = "Deposit Concentration and Branch Presence in Largest Deposit Districts",
    label = "kundu_deposit_concentration") |>
  kable_styling(latex_options = "hold_position", font_size = 9, full_width = FALSE) |>
  row_spec(nrow(tt), extra_latex_after = "\\midrule") |>
  footnote(general = paste0(
    "Each bank's largest deposit district is the distrito in which it books the greatest ",
    "volume of deposits, from SBS Boletin B-2358 branch-level reporting for ", SNAP_LAB,
    ". Deposit share is that district's share of the bank's total deposits; branch share ",
    "is its share of the bank's branches; both are reported as percentages. The ",
    "last two rows report the Pearson correlation between the two shares under Deposits ",
    "and the Spearman rank correlation under Branches. Deposits are recorded at the branch that books the ",
    "account, so wholesale and corporate balances are attributed to head offices in Lima's ",
    "financial district rather than to the depositor's location."),
    alphabet = "Single-office bank: the largest deposit district holds 100 per cent of both deposits and branches by construction.",
    general_title = "Note:", threeparttable = TRUE, escape = FALSE)

# kableExtra emits "\midrule\\" for extra_latex_after, which LaTeX renders as
# an empty row. Written out through a string so the .tex file is never hand-edited.
tex <- gsub("\\\\midrule\\\\\\\\", "\\\\midrule", as.character(tex))
writeLines(tex, file.path(TAB_DIR, "tab_kundu_deposit_concentration.tex"))

say(sprintf("Mean deposit share in largest district:   %.1f%%", 100 * mean(tab$dep_share)))
say(sprintf("Median deposit share in largest district: %.1f%%", 100 * median(tab$dep_share)))
say(sprintf("Mean branches in largest district:        %.1f (median %.0f)",
            mean(tab$n_branches), median(tab$n_branches)))
say(sprintf("Mean branch share in largest district:    %.1f%%", 100 * mean(tab$branch_share)))
say(sprintf("Correlation deposit share vs branch share: Pearson %.2f, Spearman %.2f",
            r_pear, r_spear))
say(sprintf("Excluding the %d single-office banks (n=%d): Pearson %.2f, Spearman %.2f",
            sum(tab$single_office), nrow(mb_t), r_pear_m, r_spear_m))

fig3 <- ggplot(tab, aes(100 * branch_share, 100 * dep_share)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", colour = "grey55", linewidth = 0.4) +
  geom_point(aes(size = bank_branches, fill = single_office), shape = 21,
             colour = "grey20", alpha = 0.85, stroke = 0.3) +
  scale_fill_manual(values = c(`FALSE` = "#4E79A7", `TRUE` = "#C0392B"),
                    labels = c(`FALSE` = "Branch network", `TRUE` = "Single office"),
                    name = NULL, guide = guide_legend(override.aes = list(size = 3.5))) +
  # seed = 1 makes the repulsion reproducible. ggrepel defaults to seed = NA and
  # seeds locally when given a value, so this is what fixes the layout, not any
  # global set.seed.
  ggrepel::geom_text_repel(aes(label = Bank), size = 2.3, family = "serif",
                           colour = "grey15", max.overlaps = 20, seed = 1,
                           point.padding = 0.35, box.padding = 0.45,
                           min.segment.length = 0.2, segment.size = 0.2) +
  scale_size(range = c(2, 8), trans = "sqrt", name = "Total branches",
             breaks = c(1, 50, 150, 300),
             guide = guide_legend(override.aes = list(fill = "grey70"))) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  labs(title    = "Deposit Share against Branch Share in the Largest Deposit District",
       subtitle = "Points above the 45-degree line: deposits are more concentrated than branches",
       x = "Share of the bank's branches in its largest deposit district",
       y = "Share of the bank's deposits in that district",
       caption = paste0(
         "SBS Boletin B-2358, ", SNAP_LAB, ". Pearson correlation ",
         sprintf("%.2f", r_pear), " (Spearman ", sprintf("%.2f", r_spear),
         ") across all ", nrow(tab), " banks.\n",
         "Dropping the ", sum(tab$single_office), " single-office banks, which sit at (100%, 100%) ",
         "by construction and are drawn on top of one another, leaves ",
         sprintf("%.2f", r_pear_m), " (Spearman ", sprintf("%.2f", r_spear_m), ").\n",
         "Marker area is proportional to the bank's total branch count.")) +
  base_theme + theme(legend.position = "right",
                     plot.caption = element_text(size = 6.5, colour = "grey30", hjust = 0))

ggsave(file.path(FIG_DIR, "fig_kundu_dep_vs_branch.pdf"), fig3,
       width = 7, height = 5)
say("\n-> output/tables/tab_kundu_deposit_concentration.tex")
say("-> output/figures/fig_kundu_dep_vs_branch.pdf")

# ==============================================================================
# OUTPUT 4 -- where banks' largest deposit districts are
# ==============================================================================
hdr("OUTPUT 4  Geographic distribution of banks' largest deposit districts")

cnt <- bd[rank == 1, .(n_banks_top = .N), by = .(ubigeo_geo, ubigeo, dist_name, prov_name, dep_name)]
cnt_geo <- cnt[, .(n_banks_top = sum(n_banks_top)), by = .(ubigeo = ubigeo_geo)]

map4 <- merge(adm3, cnt_geo, by = "ubigeo", all.x = TRUE)

setorder(cnt, -n_banks_top, dist_name)
top5 <- head(cnt, 5)
top5_lab <- paste(sprintf("%s (%s, %d %s)", titlecase(top5$dist_name),
                          titlecase(top5$dep_name), top5$n_banks_top,
                          ifelse(top5$n_banks_top == 1, "bank", "banks")),
                  collapse = ";  ")

is_lima_met <- function(dep, prov) (dep == "LIMA" & prov == "LIMA") | dep == "CALLAO"
lima_share <- 100 * mean(is_lima_met(bd[rank == 1, dep_name], bd[rank == 1, prov_name]))

say(sprintf("Distritos that are the largest deposit district for at least one bank: %d",
            nrow(cnt)))
say(sprintf(paste("Banks whose largest deposit district lies in Lima Metropolitana",
                  "(Lima province or Callao): %.1f%% (%d of %d)"),
            lima_share,
            sum(is_lima_met(bd[rank == 1, dep_name], bd[rank == 1, prov_name])), n_banks))
say("Top districts by number of banks:")
print(cnt[, .(dist_name = titlecase(dist_name), prov = titlecase(prov_name),
              dep = titlecase(dep_name), n_banks_top)])
capture.output(print(cnt[, .(dist_name = titlecase(dist_name), prov = titlecase(prov_name),
                             dep = titlecase(dep_name), n_banks_top)]), file = stats_con)

pal_lim <- range(cnt_geo$n_banks_top)
brk <- unique(round(seq(pal_lim[1], pal_lim[2], length.out = 4)))

base_map <- function(dat, xlim = NULL, ylim = NULL, title = NULL, show_legend = TRUE,
                     border = "grey80", lw = 0.05) {
  ggplot(dat) +
    geom_sf(aes(fill = n_banks_top), colour = border, linewidth = lw) +
    scale_fill_distiller(palette = "Blues", direction = 1, na.value = "grey96",
                         breaks = brk, limits = pal_lim,
                         name = "Number of banks for which the district is the largest",
                         guide = guide_colourbar(title.position = "top", title.hjust = .5)) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = title) +
    map_theme + theme(legend.position = if (show_legend) "bottom" else "none")
}

# Every largest-deposit district sits inside a few square kilometres of central
# Lima, invisible at national scale, so the inset is the bounding box of those
# districts with a small margin rather than the whole metropolitan area.
top_sf <- map4[!is.na(map4$n_banks_top), ]
bb     <- st_bbox(top_sf)
pad    <- 0.30 * max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])
zoom   <- c(xmin = bb[["xmin"]] - pad, xmax = bb[["xmax"]] + pad,
            ymin = bb[["ymin"]] - pad, ymax = bb[["ymax"]] + pad)
zoom_box <- st_as_sfc(st_bbox(c(xmin = zoom[["xmin"]], xmax = zoom[["xmax"]],
                                ymin = zoom[["ymin"]], ymax = zoom[["ymax"]]),
                              crs = st_crs(map4)))

p4a <- base_map(map4, title = "Peru", show_legend = TRUE, border = NA) +
  geom_sf(data = adm1, fill = NA, colour = "grey55", linewidth = 0.12) +
  geom_sf(data = zoom_box, fill = NA, colour = "#C0392B", linewidth = 0.7)

inset_sf <- suppressWarnings(st_crop(map4, st_bbox(zoom_box)))
p4b <- base_map(inset_sf, title = "Central Lima", show_legend = FALSE,
                border = "grey60", lw = 0.2)

lab_sf <- top_sf
ctr <- suppressWarnings(st_coordinates(st_point_on_surface(st_geometry(lab_sf))))
lab_dt <- data.table(x = ctr[, 1], y = ctr[, 2],
                     lab = sprintf("%s (%d)",
                                   titlecase(cnt[match(lab_sf$ubigeo, cnt$ubigeo_geo), dist_name]),
                                   lab_sf$n_banks_top))
p4b <- p4b + ggrepel::geom_text_repel(
  data = lab_dt, aes(x, y, label = lab), inherit.aes = FALSE,
  size = 2.6, family = "serif", colour = "grey10", box.padding = 0.5,
  min.segment.length = 0, segment.size = 0.25, max.overlaps = 30, seed = 1)

fig4 <- (p4a | p4b) +
  plot_layout(widths = c(1, 1.1)) +
  plot_annotation(
    title    = "Geographic Distribution of Banks' Largest Deposit Districts",
    subtitle = "Color intensity indicates the number of banks for which a district is the largest deposit-raising location",
    caption  = paste0(
      "Top 5 distritos: ", top5_lab, ".\n",
      sprintf("%.0f%% of the %d banks have their largest deposit district in Lima Metropolitana (Lima province or Callao). ",
              lima_share, n_banks),
      "Districts in light grey are the largest for no bank.\n",
      "SBS Boletin B-2358 branch-level deposits, ", SNAP_LAB,
      ". Deposits are booked at the branch holding the account, which concentrates corporate balances in head-office districts."),
    theme = theme(plot.title = element_text(size = 12, face = "bold", family = "serif"),
                  plot.subtitle = element_text(size = 9, colour = "grey25", family = "serif"),
                  plot.caption = element_text(size = 7, colour = "grey30", hjust = 0, family = "serif")))

ggsave(file.path(FIG_DIR, "fig_kundu_largest_district_map.pdf"), fig4,
       width = 9.5, height = 6.4)
say("\n-> output/figures/fig_kundu_largest_district_map.pdf")

hdr("Session")
say(R.version.string)
for (p in c("data.table", "sf", "ggplot2", "kableExtra", "patchwork"))
  say(sprintf("  %-12s %s", p, as.character(packageVersion(p))))
close(stats_con)
cat("\nall Phase 1 outputs written.\n")
