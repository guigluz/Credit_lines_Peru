# ==============================================================================
# 07_kundu_clean.R -- build the district-level datasets behind the Kundu, Park
# & Vats (2021) feasibility exercise for Peru.
#
# Produces four analysis-ready objects in data/processed/:
#
#   kundu_district_population.rds  ubigeo x population, 1,874 districts.
#   disaster.rds                   ONE ROW PER INDECI EVENT RECORD, unfiltered:
#                                  every hazard type, every year 2003-2025.
#                                  Identifiers (event_id, date, year, month,
#                                  ubigeo, department, province, district),
#                                  the hazard labels, and three damage counts
#                                  (deaths, houses_destroyed, houses_affected).
#                                  Aggregation, the hazard filter and the time
#                                  window all happen downstream, so each analysis
#                                  states its own and no choice is hidden here.
#   kundu_bank_district.rds        bank x ubigeo deposits and branch counts at the date
#                                  DEPOSIT_SNAPSHOT, with within-bank shares
#                                  and the within-bank district rank.
#   kundu_admin3_sf.rds            district polygons (sf), keyed on ubigeo.
#
# KEY IDENTIFIER --------------------------------------------------------------
# Everything is keyed on the 6-digit INEI UBIGEO. The INDECI emergency file and
# the boundary file both carry it natively (the HDX ADM3 pcode is "PE" + ubigeo).
# The SBS branch panel does NOT: it carries only departamento / provincia /
# distrito names, so those are matched to ubigeo by normalised name, with a
# within-province prefix fallback for truncated names and a short manual map for
# the residual. Every stage of that match is reported.
#
# MEASUREMENT NOTES -----------------------------------------------------------
# * Damages. INDECI records physical counts, not monetary loss. Kundu et al. use
#   property damage in dollars from SHELDUS. The closest Peruvian analogue is
#   dwellings destroyed + dwellings affected. The two are kept as separate
#   columns because they are different severities; sum them where the analysis
#   wants the combined measure.
# * FECHA mixes date conventions -- dd/mm/yyyy for 4-digit years, mm/dd/yy for
#   the 2-digit ones. Section 2 splits on that and verifies the result against
#   the MES and ANO columns, stopping if any row disagrees.
# * Population is a single cross-section (INEI, 2017 census vintage), not a
#   yearly series, so per-capita damages use a fixed denominator. It is also the
#   district register, and the 2017 vintage misses the districts created since.
#   disaster.rds KEEPS those events; they drop out only when an analysis joins
#   to population. Their territory still appears on the maps inside the parent
#   districts they were split from.
# * Deposits are in THOUSANDS of soles and are booked where the ACCOUNT sits.
#   For corporates that is typically a Lima head-office branch rather than where
#   the depositor is. This inflates Lima concentration relative to the true
#   geography of deposit-raising and is the main threat to the design.
# * Only the 23 private banks (banca multiple) are in the SBS branch table.
#   Banco de la Nacion, cajas municipales and cajas rurales are absent, which
#   removes most of the non-Lima deposit base.
#
# Usage:  Rscript code/07_kundu_clean.R
# ==============================================================================

for (l in c("en_US.UTF-8", "C.UTF-8", "es_PE.UTF-8"))
  if (suppressWarnings(Sys.setlocale("LC_CTYPE", l)) != "") break

suppressPackageStartupMessages({
  library(data.table); library(stringi); library(sf)
})

set.seed(20240101)

# ── Configuration ────────────────────────────────────────────────────────────
# The disaster window and the hazard-type filter are NOT set here. disaster.rds
# is written unfiltered and the analysis scripts choose both, so those choices
# are visible next to the results they produce.
DEPOSIT_SNAPSHOT <- as.Date("2023-12-31")  # bank x district deposits are read here
MIN_BANK_DEP     <- 0              # drop banks below this total deposit level

BRANCH_GZ  <- "data/processed/b2358_branch_panel.csv.gz"
EMERG_CSV  <- "data/raw/EMERGENCIAS_2003-2025.csv"
POP_GEODIR <- "data/raw/geo/pop_geodir_ubigeo_inei.csv"
ADM3_SHP   <- "data/raw/geo/per_admin_shp/per_admin3.shp"
ADM1_SHP   <- "data/raw/geo/per_admin_shp/per_admin1.shp"

OUT_DIR <- "data/processed"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")
need <- function(p) if (!file.exists(p))
  stop("missing input: ", p, "\n  run code/06_kundu_collect.R first", call. = FALSE)
invisible(lapply(c(BRANCH_GZ, EMERG_CSV, POP_GEODIR, ADM3_SHP, ADM1_SHP), need))

# Uppercase, de-accent, strip punctuation. Used for every name-based join.
norm <- function(x) {
  x <- stri_trans_general(stri_enc_toutf8(as.character(x)), "Latin-ASCII")
  x <- toupper(trimws(x))
  trimws(gsub(" +", " ", gsub("[^A-Z0-9]+", " ", x)))
}
num <- function(x) as.numeric(gsub(",", "", trimws(as.character(x))))

# ==============================================================================
# 1. District population
# ==============================================================================
msg("[1/4] district population")

pop <- fread(POP_GEODIR, colClasses = "character", encoding = "UTF-8")
setnames(pop, 1, "ubigeo")
pop[, ubigeo := formatC(gsub("[^0-9]", "", ubigeo), width = 6, flag = "0")]
pop <- pop[, .(ubigeo,
               dep_name  = norm(Departamento),
               prov_name = norm(Provincia),
               dist_name = norm(Distrito),
               population = num(Poblacion))]

stopifnot(all(grepl("^[0-9]{6}$", pop$ubigeo)),
          !anyDuplicated(pop$ubigeo),
          !anyNA(pop$population))
# The name triple is the join key for the SBS branch panel, which carries no
# ubigeo, so it has to identify a district on its own.
if (anyDuplicated(pop[, paste(dep_name, prov_name, dist_name)]))
  stop("district name triples are not unique; the branch crosswalk would be ambiguous")

# Boundary file <-> district list. The HDX shapefile carries a single polygon,
# coded 120699, for the pair Mazamari (120604) / Pangoa (120606), whose common
# border is disputed. Both districts are mapped onto that polygon so their
# values survive to the maps; every other district maps to itself.
GEO_XWALK <- data.table(ubigeo = c("120604", "120606"), ubigeo_geo = "120699")
pop <- merge(pop, GEO_XWALK, by = "ubigeo", all.x = TRUE)
pop[is.na(ubigeo_geo), ubigeo_geo := ubigeo]

msg("  districts:", nrow(pop))
msg("  total population:", format(round(sum(pop$population)), big.mark = ","))

saveRDS(pop, file.path(OUT_DIR, "kundu_district_population.rds"))

# ==============================================================================
# 2. Disaster damages by district
# ==============================================================================
msg("[2/4] INDECI emergency records")

em <- fread(EMERG_CSV, sep = ";", encoding = "Latin-1", colClasses = "character")
# Column names carry accents that survive the read inconsistently, so rename by
# position against the published layout.
setnames(em, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 17, 18),
         c("event_id", "fecha", "anio", "mes", "ubigeo", "dpto", "prov", "dist",
           "peligro", "tipo_peligro", "fallecidos", "viv_destruidas",
           "viv_afectadas"))
em[, ubigeo := formatC(gsub("[^0-9]", "", ubigeo), width = 6, flag = "0")]
for (v in c("fallecidos", "viv_destruidas", "viv_afectadas"))
  em[, (v) := num(get(v))]

msg("  raw event records:", nrow(em))

# ── Dates ────────────────────────────────────────────────────────────────────
# FECHA mixes two conventions: records with a 4-digit year are dd/mm/yyyy, while
# the 16,401 records carrying a 2-digit year (the 2025 vintage) are mm/dd/yy.
# Parsing everything one way silently mangles one group, so the width of the
# year field selects the format and the result is then checked against the two
# independent columns the file also carries, MES and ANO. Both must agree on
# every row or the run stops -- a future vintage may add a third convention.
MES <- c(ENERO = 1, FEBRERO = 2, MARZO = 3, ABRIL = 4, MAYO = 5, JUNIO = 6,
         JULIO = 7, AGOSTO = 8, SETIEMBRE = 9, SEPTIEMBRE = 9, OCTUBRE = 10,
         NOVIEMBRE = 11, DICIEMBRE = 12)
em[, mes_num := MES[norm(mes)]]
if (anyNA(em$mes_num))
  stop("unrecognised month name(s) in MES: ",
       paste(unique(em[is.na(mes_num), mes]), collapse = ", "))

em[, yr_width := nchar(sub(".*/", "", fecha))]
em[, date := as.Date(NA)]
em[yr_width == 4, date := as.Date(fecha, format = "%d/%m/%Y")]
em[yr_width == 2, date := as.Date(fecha, format = "%m/%d/%y")]

bad <- em[is.na(date) |
            as.integer(format(date, "%m")) != mes_num |
            as.integer(format(date, "%Y")) != as.integer(anio)]
if (nrow(bad))
  stop(nrow(bad), " record(s) whose parsed FECHA contradicts MES or ANO. ",
       "The date convention in the source file has changed; inspect e.g. ",
       paste(utils::head(bad$fecha, 3), collapse = ", "))
msg(sprintf("  dates parsed and cross-checked against MES/ANO: %s dd/mm/yyyy, %s mm/dd/yy",
            format(sum(em$yr_width == 4), big.mark = ","),
            format(sum(em$yr_width == 2), big.mark = ",")))

# ── Event-level output ───────────────────────────────────────────────────────
# Deliberately UNFILTERED: every hazard type and every year is kept, so the
# hazard and window choices live in the analysis scripts where they are visible,
# rather than being baked in here. Dwellings destroyed and affected stay
# separate because they are different severities; sum them downstream if the
# analysis wants the combined "damaged" measure.
disaster <- em[, .(event_id,
                   date,
                   year             = as.integer(anio),
                   month            = mes_num,
                   ubigeo,
                   department       = norm(dpto),
                   province         = norm(prov),
                   district         = norm(dist),
                   hazard           = norm(peligro),
                   hazard_type      = norm(tipo_peligro),
                   deaths           = fallecidos,
                   houses_destroyed = viv_destruidas,
                   houses_affected  = viv_afectadas)]
setkey(disaster, ubigeo, date)

msg("  years:", min(disaster$year), "-", max(disaster$year),
    "| districts appearing:", uniqueN(disaster$ubigeo))
msg("  hazard types:", paste(sort(unique(disaster$hazard_type)), collapse = " / "))
orphan <- setdiff(unique(disaster$ubigeo), pop$ubigeo)
msg("  event ubigeos absent from the district register:", length(orphan),
    sprintf("(%d records, %.2f%% -- districts created after the 2017 vintage)",
            disaster[ubigeo %in% orphan, .N],
            100 * disaster[ubigeo %in% orphan, .N] / nrow(disaster)))
msg("  totals over the WHOLE file: deaths",
    format(sum(disaster$deaths), big.mark = ","),
    "| dwellings destroyed", format(sum(disaster$houses_destroyed), big.mark = ","),
    "| affected", format(sum(disaster$houses_affected), big.mark = ","))

saveRDS(disaster, file.path(OUT_DIR, "disaster.rds"))

# ==============================================================================
# 3. Bank x district deposits
# ==============================================================================
msg("[3/4] bank x district deposits at", format(DEPOSIT_SNAPSHOT))

br <- fread(BRANCH_GZ, encoding = "UTF-8")
br[, report_date := as.Date(report_date)]
if (!DEPOSIT_SNAPSHOT %in% br$report_date)
  stop("DEPOSIT_SNAPSHOT ", DEPOSIT_SNAPSHOT, " is not a reporting date in ",
       BRANCH_GZ, call. = FALSE)

snap <- br[report_date == DEPOSIT_SNAPSHOT & is_foreign == FALSE]
msg("  branches at snapshot:", nrow(snap), "| banks:",
    uniqueN(snap$bank_standardized))

# ── Name -> ubigeo crosswalk for branch locations ────────────────────────────
loc <- unique(snap[, .(departamento, provincia, distrito)])
loc[, `:=`(dep_name  = norm(departamento),
           prov_name = norm(provincia),
           dist_name = norm(distrito))]
# The SBS writes Callao's province as "Prov. Const. del Callao"; INEI uses
# department = province = CALLAO.
loc[dep_name %like% "CALLAO", `:=`(dep_name = "CALLAO", prov_name = "CALLAO")]

# Districts renamed or truncated in the SBS file. Resolved by hand against INEI.
# The first two are SBS spellings; the last three are geodir spellings that the
# SBS writes out in full ("Carmen de La Legua" -> "... Reynoso"), as a word
# rather than a numeral ("26 de Octubre"), or with the province spelled
# correctly ("Contralmirante Villa" is truncated in geodir).
MANUAL_UBIGEO <- data.table(
  dep_name  = c("UCAYALI",          "LIMA",
                "CALLAO",           "PIURA", "TUMBES"),
  prov_name = c("CORONEL PORTILLO", "LIMA",
                "CALLAO",           "PIURA", "CONTRALMIRANTE VILLAR"),
  dist_name = c("CALLARIA",         "PUEBLO LIBRE MAGDALENA VIEJA",
                "CARMEN DE LA LEGUA REYNOSO", "VEINTISEIS DE OCTUBRE", "ZORRITOS"),
  ubigeo_manual = c("250101",       "150121",
                    "070103",       "200115", "240201")
)

# A typo in MANUAL_UBIGEO would silently attach deposits to a district that does
# not exist, so the targets are checked against the register. Entries that do not
# fire at this snapshot are fine: they cover spellings the SBS used in other
# months and keep the crosswalk correct when DEPOSIT_SNAPSHOT is moved.
if (!all(MANUAL_UBIGEO$ubigeo_manual %in% pop$ubigeo))
  stop("MANUAL_UBIGEO points at ubigeos absent from the district register: ",
       paste(setdiff(MANUAL_UBIGEO$ubigeo_manual, pop$ubigeo), collapse = ", "))

key <- function(d) paste(d$dep_name, d$prov_name, d$dist_name, sep = "|")
pop_key <- copy(pop)[, k := key(.SD)]

loc[, k := key(.SD)]
loc <- merge(loc, pop_key[, .(k, ubigeo_exact = ubigeo)], by = "k", all.x = TRUE)
n_exact <- sum(!is.na(loc$ubigeo_exact))

loc <- merge(loc, MANUAL_UBIGEO, by = c("dep_name", "prov_name", "dist_name"),
             all.x = TRUE)
loc[, ubigeo := fcoalesce(ubigeo_exact, ubigeo_manual)]
n_manual <- sum(is.na(loc$ubigeo_exact) & !is.na(loc$ubigeo_manual))

# Prefix fallback: the SBS truncates long district names at a fixed width, so a
# unique district in the same province starting with the SBS string is accepted.
still <- which(is.na(loc$ubigeo))
n_prefix <- 0L
for (i in still) {
  cand <- pop[dep_name == loc$dep_name[i] & prov_name == loc$prov_name[i] &
                startsWith(dist_name, loc$dist_name[i])]
  if (nrow(cand) == 1L) { loc$ubigeo[i] <- cand$ubigeo; n_prefix <- n_prefix + 1L }
}

msg("  location crosswalk: ", nrow(loc), " triples | exact ", n_exact,
    " | manual ", n_manual, " | prefix ", n_prefix,
    " | UNMATCHED ", sum(is.na(loc$ubigeo)), sep = "")
if (any(is.na(loc$ubigeo))) {
  msg("  UNMATCHED locations (their deposits are dropped):")
  print(loc[is.na(ubigeo), .(departamento, provincia, distrito)])
}

snap <- merge(snap, loc[, .(departamento, provincia, distrito, ubigeo)],
              by = c("departamento", "provincia", "distrito"), all.x = TRUE)
dropped <- snap[is.na(ubigeo), sum(dep_total)]
msg(sprintf("  deposits dropped for want of a ubigeo: %.4f%% of the total",
            100 * dropped / sum(snap$dep_total)))
snap <- snap[!is.na(ubigeo)]

# ── Collapse to bank x district ──────────────────────────────────────────────
bd <- snap[, .(deposits  = sum(dep_total, na.rm = TRUE),
               credit    = sum(cred_total, na.rm = TRUE),
               n_branches = uniqueN(branch_id)),
           by = .(bank = bank_standardized, ubigeo)]

bank_tot <- bd[, .(bank_deposits = sum(deposits),
                   bank_credit   = sum(credit),
                   bank_branches = sum(n_branches),
                   n_districts   = uniqueN(ubigeo)), by = bank]
bank_tot <- bank_tot[bank_deposits > MIN_BANK_DEP]
msg("  banks retained:", nrow(bank_tot))

bd <- merge(bd, bank_tot, by = "bank")
bd[, `:=`(dep_share    = deposits / bank_deposits,
          branch_share = n_branches / bank_branches)]
setorder(bd, bank, -deposits)
bd[, rank := seq_len(.N), by = bank]

bd <- merge(bd, pop[, .(ubigeo, ubigeo_geo, dep_name, prov_name, dist_name, population)],
            by = "ubigeo", all.x = TRUE)
setorder(bd, bank, rank)

msg("  bank x district cells:", nrow(bd),
    "| districts covered:", uniqueN(bd$ubigeo))
saveRDS(bd, file.path(OUT_DIR, "kundu_bank_district.rds"))

# ==============================================================================
# 4. District polygons
# ==============================================================================
msg("[4/4] district boundaries")

adm3 <- st_read(ADM3_SHP, quiet = TRUE)
names(adm3) <- tolower(names(adm3))
adm3$ubigeo <- sub("^PE", "", adm3$adm3_pcode)
adm3 <- adm3[, c("ubigeo", "adm3_name", "adm2_name", "adm1_name", "geometry")]
names(adm3) <- c("ubigeo", "dist_shp", "prov_shp", "dep_shp", "geometry")
adm3 <- sf::st_make_valid(adm3)

msg("  polygons:", nrow(adm3),
    "| ubigeos also in the population file:", sum(adm3$ubigeo %in% pop$ubigeo))
msg("  districts with population but no polygon:",
    sum(!pop$ubigeo %in% adm3$ubigeo))
msg("  bank districts without a polygon (after the crosswalk):",
    length(setdiff(unique(bd$ubigeo_geo), adm3$ubigeo)))

# Geometry is simplified for drawing only. The raw admin3 file carries 1.6m
# vertices, which makes a 10 MB map PDF; rmapshaper's Visvalingam simplification
# with keep_shapes keeps every one of the 1,873 polygons and cuts that ~20-fold
# with no visible difference at page size. sf::st_simplify is the fallback: it
# barely helps on lon/lat data (GEOS forces topology preservation) but it always
# works, so the pipeline never fails for want of an optional package.
# The ubigeo keys, not the polygons, do the work everywhere else.
simplify_for_plot <- function(x, keep = 0.05, tol = 0.005) {
  if (requireNamespace("rmapshaper", quietly = TRUE)) {
    out <- try(rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE),
               silent = TRUE)
    if (!inherits(out, "try-error") && nrow(out) == nrow(x))
      return(sf::st_make_valid(out))
    msg("  rmapshaper failed; falling back to st_simplify")
  }
  sf::st_make_valid(sf::st_simplify(x, dTolerance = tol, preserveTopology = TRUE))
}
n_vertices <- function(x)
  sum(vapply(sf::st_geometry(x), function(g) nrow(sf::st_coordinates(g)), numeric(1)))

v0 <- n_vertices(adm3)
adm3_simp <- simplify_for_plot(adm3)
msg("  polygons after simplification:", nrow(adm3_simp),
    sprintf("| vertices %s -> %s", format(v0, big.mark = ","),
            format(n_vertices(adm3_simp), big.mark = ",")))

adm1 <- st_read(ADM1_SHP, quiet = TRUE)
names(adm1) <- tolower(names(adm1))
adm1 <- adm1[, c("adm1_pcode", "adm1_name", "geometry")]
names(adm1) <- c("dep_pcode", "dep_name", "geometry")
adm1 <- simplify_for_plot(adm1, keep = 0.05, tol = 0.01)
msg("  departamento polygons:", nrow(adm1))

saveRDS(adm3_simp, file.path(OUT_DIR, "kundu_admin3_sf.rds"))
saveRDS(adm1,      file.path(OUT_DIR, "kundu_admin1_sf.rds"))

msg("done. wrote:")
print(list.files(OUT_DIR, pattern = "^(kundu_|disaster\\.rds$)"))
cat("\nsessionInfo:\n"); print(sessionInfo()$otherPkgs |> names())
