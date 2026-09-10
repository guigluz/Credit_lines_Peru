# ==============================================================================
# 06_geo_collect.R -- download the geographic inputs needed to implement the
# Kundu, Park & Vats (2021) deposit-shock design for Peru.
#
# The SBS branch panel and the INDECI emergency file are already in data/raw/.
# What is missing is (i) district population, to scale damages per capita, and
# (ii) district boundaries, to draw the maps. Both are pulled here, once, and
# written to data/raw/geo/ so the rest of the pipeline runs offline.
#
# DOWNLOADS -------------------------------------------------------------------
# 1) pop_geodir_ubigeo_inei.csv
#    geodir/ubigeo-peru, file geodir-ubigeo-inei.csv. INEI UBIGEO (6 digits) with
#    population and surface area for the 1,874 districts of the 2017 census
#    boundary vintage. Population carries thousands separators; the UBIGEO,
#    district, province and department names are unique and complete, so this is
#    the sole district register for the pipeline.
#
# 2) per_admin_boundaries.shp.zip
#    OCHA/HDX Common Operational Dataset -- Peru administrative boundaries.
#    One archive carrying EVERY administrative level, all WGS84, all keyed on a
#    pcode that is "PE" + the INEI UBIGEO prefix for that level. That shared key
#    is what makes the file mergeable with the population data and the INDECI
#    emergency records without any name matching, at whichever level is wanted:
#
#      per_admin0.shp        1 feature     country          pcode "PE"
#      per_admin1.shp       25 features    DEPARTAMENTO     pcode PE + 2 digits
#      per_admin2.shp      196 features    provincia        pcode PE + 4 digits
#      per_admin3.shp    1,873 features    distrito         pcode PE + 6 digits
#      per_adminlines.shp / per_adminpoints.shp  boundary lines, settlements
#
#
# Usage:  Rscript code/06_kundu_collect.R
# ==============================================================================

suppressPackageStartupMessages({
  library(utils)
})

RAW_GEO   <- "data/raw/geo"
LOG_DIR   <- "output/logs"
OVERWRITE <- FALSE   # TRUE re-downloads even if the file is already on disk

dir.create(RAW_GEO, showWarnings = FALSE, recursive = TRUE)
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

SOURCES <- list(
  list(
    name = "district population (geodir / INEI ubigeo)",
    url  = paste0("https://raw.githubusercontent.com/geodir/ubigeo-peru/",
                  "master/geodir-ubigeo-inei.csv"),
    dest = file.path(RAW_GEO, "pop_geodir_ubigeo_inei.csv"),
    min_bytes = 50e3
  ),
  list(
    name = "admin boundaries (OCHA/HDX COD-AB Peru)",
    url  = paste0("https://data.humdata.org/dataset/",
                  "54fc7f4d-f4c0-4892-91f6-2fe7c1ecf363/resource/",
                  "61faa8d6-fbfa-4d44-a94d-8f3b0241277a/download/",
                  "per_admin_boundaries.shp.zip"),
    dest = file.path(RAW_GEO, "per_admin_boundaries.shp.zip"),
    min_bytes = 10e6
  )
)

# ── Download with retries. A truncated file is worse than no file, so the size
#    check runs before the download is accepted. ───────────────────────────────
fetch <- function(src, tries = 3) {
  if (file.exists(src$dest) && !OVERWRITE &&
      file.info(src$dest)$size >= src$min_bytes) {
    msg("skip (already on disk):", basename(src$dest))
    return(TRUE)
  }
  tmp <- paste0(src$dest, ".part")
  for (k in seq_len(tries)) {
    ok <- tryCatch({
      download.file(src$url, tmp, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) { msg("  attempt", k, "failed:", conditionMessage(e)); FALSE })

    if (ok && file.exists(tmp) && file.info(tmp)$size >= src$min_bytes) {
      file.rename(tmp, src$dest)
      msg("ok:", basename(src$dest),
          sprintf("(%.1f MB)", file.info(src$dest)$size / 1e6))
      return(TRUE)
    }
    if (file.exists(tmp)) {
      msg("  attempt", k, "too small:", file.info(tmp)$size, "bytes")
      unlink(tmp)
    }
    Sys.sleep(2 * k)
  }
  msg("FAILED:", src$name, "->", src$url)
  FALSE
}

msg("collecting geographic inputs into", RAW_GEO)
status <- vapply(SOURCES, fetch, logical(1))

# ── Unpack the boundary archive ──────────────────────────────────────────────
# Every level in ADMIN_LEVELS must land. Checking only admin3, as this did
# before, would let a partial extraction pass and leave the departamento and
# provincia layers silently missing.
ADMIN_LEVELS <- c(admin0 = "country", admin1 = "departamento",
                  admin2 = "provincia", admin3 = "distrito")

shp_zip <- file.path(RAW_GEO, "per_admin_boundaries.shp.zip")
shp_dir <- file.path(RAW_GEO, "per_admin_shp")
lvl_shp <- file.path(shp_dir, paste0("per_", names(ADMIN_LEVELS), ".shp"))

if (file.exists(shp_zip)) {
  if (!all(file.exists(lvl_shp)) || OVERWRITE) {
    dir.create(shp_dir, showWarnings = FALSE)
    unzip(shp_zip, exdir = shp_dir)
    msg("unzipped ->", shp_dir)
  } else {
    msg("skip unzip (all admin levels already present)")
  }

  missing <- names(ADMIN_LEVELS)[!file.exists(lvl_shp)]
  if (length(missing))
    stop("the boundary archive did not yield ", paste(missing, collapse = ", "),
         ".\n  Delete ", shp_dir, " and re-run, or set OVERWRITE <- TRUE.")

  # A shapefile is only usable with its .dbf, .shx and .prj siblings.
  for (lv in names(ADMIN_LEVELS)) {
    parts <- file.path(shp_dir, paste0("per_", lv, c(".shp", ".dbf", ".shx", ".prj")))
    if (!all(file.exists(parts)))
      stop("per_", lv, " is missing sidecar files (.dbf/.shx/.prj); the archive ",
           "extracted incompletely. Delete ", shp_dir, " and re-run.")
    msg(sprintf("  per_%s.shp  (%s)  %.1f MB", lv, ADMIN_LEVELS[[lv]],
                file.info(file.path(shp_dir, paste0("per_", lv, ".shp")))$size / 1e6))
  }
}

if (!all(status)) {
  stop("one or more downloads failed; see messages above. ",
       "Re-run, or place the files in ", RAW_GEO, " manually.")
}
msg("done. files in", RAW_GEO, ":")
print(list.files(RAW_GEO, recursive = FALSE))
