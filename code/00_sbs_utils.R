# ==============================================================================
# 00_sbs_utils.R -- Shared, cuadro-agnostic helpers for SBS Boletin Estadistico
# ==============================================================================
# Sourced by the collect/clean scripts for any SBS bulletin cuadro (B-2343
# creditos directos, B-2322 creditos indirectos, ...). Nothing here hardcodes a
# particular cuadro: the code, title regex and expected units label are all
# arguments.
#
# Access notes (verified 2026-08-26):
#   * The listing page www.sbs.gob.pe/app/stats/EstadisticaSistemaFinanciero
#     Resultados.asp?c=<CODE> sits behind Imperva/Incapsula and returns a JS
#     challenge stub to curl/httr. Do NOT scrape it.
#   * The file host intranet2.sbs.gob.pe is NOT behind Incapsula and serves the
#     workbooks to plain httr, but presents an INCOMPLETE TLS CHAIN, so a
#     verified request fails. We retry with verification disabled (same
#     workaround as the earlier SF-2101 downloader, git show
#     a9e6037:code/peru_dollarization.R).
#   * URL pattern was read off the rendered listing page, not invented:
#     https://intranet2.sbs.gob.pe/estadistica/financiera/{YYYY}/{Mes}/{CODE}-{mm}{YYYY}.XLS
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(purrr)
})

# ---- locale hardening (same approach as code/peru_dollarization.R) -----------
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (loc in c("en_US.UTF-8", "C.UTF-8", "es_PE.UTF-8")) {
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
  }
}

# ---- month table -------------------------------------------------------------
# NOTE: SBS spells September "Setiembre" (no 'p'). Getting this wrong 404s
# every September file, so it is asserted rather than trusted.
SBS_MONTHS <- c(
  Enero = "en", Febrero = "fe", Marzo    = "ma", Abril   = "ab",
  Mayo  = "my", Junio   = "jn", Julio    = "jl", Agosto  = "ag",
  Setiembre = "se", Octubre = "oc", Noviembre = "no", Diciembre = "di"
)
stopifnot(length(SBS_MONTHS) == 12L)

# Spanish month names -> month number, for parsing in-file date strings such as
# "Al 30 de Junio de 2001". Accepts both Setiembre and Septiembre on the way in.
SBS_MONTH_LOOKUP <- c(
  enero = 1L, febrero = 2L, marzo = 3L, abril = 4L, mayo = 5L, junio = 6L,
  julio = 7L, agosto = 8L, setiembre = 9L, septiembre = 9L, octubre = 10L,
  noviembre = 11L, diciembre = 12L
)

SBS_BASE_URL <- "https://intranet2.sbs.gob.pe/estadistica/financiera"
SBS_UA <- paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"
)

#' Build the download URL for one cuadro-month.
#'
#' @param code   Cuadro code, e.g. "B-2343".
#' @param year   4-digit year.
#' @param month  Month number 1-12.
#' @param two_digit_year Use a 2-digit year in the filename. SBS used this for
#'   the oldest files (e.g. B-2322-di98.XLS for December 1998).
sbs_url <- function(code, year, month, two_digit_year = FALSE) {
  stopifnot(month >= 1L, month <= 12L)
  yr_tag <- if (two_digit_year) sprintf("%02d", year %% 100L) else as.character(year)
  sprintf(
    "%s/%d/%s/%s-%s%s.XLS",
    SBS_BASE_URL, year, names(SBS_MONTHS)[month], code, SBS_MONTHS[[month]], yr_tag
  )
}

#' Local cache path for one cuadro-month, as named by SBS (always ".XLS").
sbs_dest <- function(root, code, year, month, two_digit_year = FALSE) {
  file.path(root, code, as.character(year), basename(sbs_url(code, year, month, two_digit_year)))
}

#' All plausible cache filenames for one cuadro-month.
#'
#' SBS names every workbook ".XLS" regardless of what is inside it, but readxl
#' resolves the format from the EXTENSION before the signature -- so an OOXML
#' file named ".XLS" is handed to libxls and fails to open. We therefore store
#' each file under the extension matching its true signature (see
#' sbs_normalize_ext) and look for any variant when checking the cache.
sbs_dest_variants <- function(dest) {
  stem <- sub("\\.[Xx][Ll][Ss][Xx]?$", "", dest)
  c(dest, paste0(stem, ".xls"), paste0(stem, ".xlsx"))
}

#' Return the cached path for a cuadro-month, or NA if not cached.
sbs_cached_path <- function(dest) {
  cands <- sbs_dest_variants(dest)
  hit <- cands[file.exists(cands) & file.size(cands) > SBS_MIN_BYTES]
  if (length(hit)) hit[[1]] else NA_character_
}

#' Rename a downloaded file so its extension matches its true container format.
#' Returns the (possibly new) path.
sbs_normalize_ext <- function(path) {
  fmt <- sbs_format(path)
  if (is.na(fmt) || !fmt %in% c("xls", "xlsx")) return(path)
  stem <- sub("\\.[Xx][Ll][Ss][Xx]?$", "", path)
  target <- paste0(stem, ".", fmt)
  if (identical(normalizePath(path, mustWork = FALSE),
                normalizePath(target, mustWork = FALSE))) return(path)
  if (file.exists(target)) unlink(target)
  ok <- file.rename(path, target)
  if (ok) target else path
}

# A real workbook is never this small; anything under the floor is an error page
# or a truncated transfer, not data.
SBS_MIN_BYTES <- 5000L

#' Download one file, caching aggressively.
#'
#' Returns a one-row tibble describing the outcome. Never throws on an HTTP
#' failure -- the manifest records the status so failures are visible instead of
#' silently dropped.
sbs_download <- function(url, dest, sleep = 0.25, overwrite = FALSE) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  hit <- sbs_cached_path(dest)
  if (!overwrite && !is.na(hit)) {
    return(tibble(url = url, dest = hit, http_status = NA_integer_,
                  bytes = file.size(hit), cached = TRUE, error = NA_character_))
  }

  tmp <- paste0(dest, ".part")
  on.exit(unlink(tmp), add = TRUE)

  get_one <- function(insecure) {
    cfg <- if (insecure) httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L) else httr::config()
    httr::GET(url, httr::write_disk(tmp, overwrite = TRUE), httr::timeout(300),
              httr::user_agent(SBS_UA), cfg)
  }

  resp <- try(get_one(FALSE), silent = TRUE)
  # intranet2 serves an incomplete chain; retry without verification.
  if (inherits(resp, "try-error")) resp <- try(get_one(TRUE), silent = TRUE)

  if (inherits(resp, "try-error")) {
    return(tibble(url = url, dest = dest, http_status = NA_integer_, bytes = NA_real_,
                  cached = FALSE, error = trimws(as.character(resp))))
  }

  status <- httr::status_code(resp)
  sz <- if (file.exists(tmp)) file.size(tmp) else 0

  if (status == 200L && sz > SBS_MIN_BYTES) {
    file.rename(tmp, dest)
    dest <- sbs_normalize_ext(dest)
    Sys.sleep(sleep)
    return(tibble(url = url, dest = dest, http_status = status, bytes = sz,
                  cached = FALSE, error = NA_character_))
  }

  Sys.sleep(sleep)
  tibble(url = url, dest = dest, http_status = status, bytes = sz, cached = FALSE,
         error = sprintf("unavailable (status=%s, bytes=%s)", status, sz))
}

#' True container format, from the file signature -- NOT the extension.
#' SBS names every file .XLS, but files are legacy BIFF up to ~2013 and OOXML
#' (zip) afterwards. readxl sniffs this itself when reading; we record it so the
#' format break is auditable in the manifest.
sbs_format <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  out <- try(readxl::format_from_signature(path), silent = TRUE)
  if (inherits(out, "try-error") || is.na(out)) NA_character_ else as.character(out)
}

# Content hash: prefer sha256 when openssl/digest is present; fall back to md5.
if (requireNamespace("openssl", quietly = TRUE)) {
  # Hash the raw bytes rather than a connection: connection-based hashing was
  # observed to return a wrong digest for a small number of files, presumably
  # from connection reuse/exhaustion. Reading the whole file is trivially cheap
  # here (workbooks are 17-90 KB) and is deterministic.
  sbs_sha256 <- function(path) {
    if (!file.exists(path)) return(NA_character_)
    raw <- readBin(path, "raw", n = file.size(path))
    paste0("sha256:", as.character(openssl::sha256(raw)))
  }
} else if (requireNamespace("digest", quietly = TRUE)) {
  sbs_sha256 <- function(path) {
    if (!file.exists(path)) return(NA_character_)
    paste0("sha256:", digest::digest(path, algo = "sha256", file = TRUE))
  }
} else {
  sbs_sha256 <- function(path) {
    if (!file.exists(path)) return(NA_character_)
    paste0("md5:", unname(tools::md5sum(path)))
  }
}

# ==============================================================================
# Text / numeric normalization
# ==============================================================================

#' Strip accents, collapse whitespace, uppercase. Used for anchor matching and
#' as the first step of entity-name standardization.
norm_txt <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, " ", " ")   # non-breaking space
  x <- str_squish(x)
  toupper(x)
}

#' Parse an SBS numeric cell held as text.
#'
#' Handles: thousands separators, ',' vs '.' decimal marks, parentheses for
#' negatives, and the '-' missing marker. Returns NA for '-' and blanks -- the
#' zero interpretation is applied downstream and explicitly, because the missing
#' marker convention flips across eras ('-' in 2001, 0 in 2009-2014, '-' in 2026).
sbs_num <- function(x) {
  x <- str_squish(as.character(x))
  out <- rep(NA_real_, length(x))
  blankish <- is.na(x) | x == "" | str_detect(x, "^[-–—]$") | x == "n.a." | x == "N.A."
  idx <- which(!blankish)
  if (!length(idx)) return(out)

  v <- x[idx]
  neg <- str_detect(v, "^\\(.*\\)$")
  v <- str_replace_all(v, "[()]", "")
  v <- str_replace_all(v, "[[:space:]%]", "")

  # Decide the decimal mark per value: whichever separator appears last wins.
  last_comma <- str_locate_last_fixed(v, ",")
  last_dot   <- str_locate_last_fixed(v, ".")
  comma_dec  <- !is.na(last_comma) & (is.na(last_dot) | last_comma > last_dot)

  v[comma_dec]  <- str_replace_all(v[comma_dec], "\\.", "")
  v[comma_dec]  <- str_replace(v[comma_dec], ",", ".")
  v[!comma_dec] <- str_replace_all(v[!comma_dec], ",", "")

  num <- suppressWarnings(as.numeric(v))
  num[neg & !is.na(num)] <- -abs(num[neg & !is.na(num)])
  out[idx] <- num
  out
}

# helper: position of the last occurrence of a fixed pattern (NA if absent)
str_locate_last_fixed <- function(x, pat) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_integer_)
    p <- gregexpr(pat, s, fixed = TRUE)[[1]]
    if (length(p) == 1L && p[1] == -1L) NA_integer_ else as.integer(p[length(p)])
  }, integer(1), USE.NAMES = FALSE)
}

#' Parse the SBS report-date cell, which appears in three encodings across eras:
#'   * Spanish text  "Al 30 de Junio de 2001"
#'   * Excel serial  "39994" / "41455"   (origin 1899-12-30)
#'   * a real datetime, which read_excel(col_types="text") renders as a serial
sbs_parse_date <- function(x) {
  x <- str_squish(as.character(x))
  if (is.na(x) || x == "") return(as.Date(NA))

  # Excel serial (plain number, plausible range 1990-2050)
  if (str_detect(x, "^[0-9]+(\\.[0-9]+)?$")) {
    n <- as.numeric(x)
    if (n > 30000 && n < 80000) return(as.Date(floor(n), origin = "1899-12-30"))
  }
  # ISO datetime, in case a reader hands one back
  if (str_detect(x, "^\\d{4}-\\d{2}-\\d{2}")) return(as.Date(substr(x, 1, 10)))

  # Spanish long form
  low <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  m <- str_match(low, "(\\d{1,2})\\s+de\\s+([a-z]+)\\s+(?:de[l]?\\s+)?(\\d{4})")
  if (!is.na(m[1, 1])) {
    mon <- SBS_MONTH_LOOKUP[[m[1, 3]]]
    if (!is.null(mon) && !is.na(mon)) {
      return(as.Date(sprintf("%s-%02d-%02d", m[1, 4], mon, as.integer(m[1, 2]))))
    }
  }
  # "Junio 2001" style fallback
  m2 <- str_match(low, "([a-z]+)\\s+(?:de[l]?\\s+)?(\\d{4})")
  if (!is.na(m2[1, 1])) {
    mon <- SBS_MONTH_LOOKUP[[m2[1, 2]]]
    if (!is.null(mon) && !is.na(mon)) return(month_end(as.integer(m2[1, 3]), mon))
  }
  as.Date(NA)
}

#' Last calendar day of a given year-month.
month_end <- function(year, month) {
  first_next <- as.Date(sprintf("%04d-%02d-01", year + (month == 12L), (month %% 12L) + 1L))
  first_next - 1L
}

# ==============================================================================
# Anchor detection -- find structure by CONTENT, never by fixed row/col index
# ==============================================================================

#' Read a worksheet as a raw character grid, preserving every original cell
#' string before any transformation.
sbs_read_grid <- function(path, sheet) {
  g <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                          col_types = "text", .name_repair = "minimal",
                          progress = FALSE)
  as.data.frame(g, stringsAsFactors = FALSE)
}

#' Which cells in the grid match a regex (on accent-stripped, uppercased text)?
#' Returns a tibble of row/col/value.
sbs_find_cells <- function(grid, pattern) {
  hits <- list()
  for (j in seq_along(grid)) {
    v <- norm_txt(grid[[j]])
    i <- which(!is.na(v) & str_detect(v, pattern))
    if (length(i)) hits[[length(hits) + 1L]] <-
        tibble(row = i, col = j, value = as.character(grid[[j]])[i])
  }
  if (!length(hits)) return(tibble(row = integer(), col = integer(), value = character()))
  bind_rows(hits) %>% arrange(row, col)
}

#' Pick the single sheet whose grid contains the table title.
#' Errors if zero or more than one sheet matches -- silence here would mean
#' parsing the wrong sheet.
sbs_pick_sheet <- function(path, title_pattern) {
  sheets <- readxl::excel_sheets(path)
  matched <- character()
  grids <- list()
  for (s in sheets) {
    g <- try(sbs_read_grid(path, s), silent = TRUE)
    if (inherits(g, "try-error") || !ncol(g) || !nrow(g)) next
    if (nrow(sbs_find_cells(g, title_pattern)) > 0L) {
      matched <- c(matched, s)
      grids[[s]] <- g
    }
  }
  if (length(matched) == 0L) stop("no sheet matches title pattern in ", basename(path))
  if (length(matched) > 1L) {
    stop("ambiguous: ", length(matched), " sheets match title in ", basename(path),
         " (", paste(matched, collapse = ", "), ")")
  }
  list(sheet = matched[[1]], grid = grids[[matched[[1]]]])
}

# ==============================================================================
# Entity-name standardization (shared by the clean and validate steps)
# ==============================================================================
# Scope qualifiers such as "(con sucursales en el exterior)" are EXTRACTED into
# their own column, never silently deleted: whether foreign branches are in
# scope is a live candidate explanation for level differences between the
# bulletin workbooks and the Series Estadisticas extract.
split_scope <- function(x) {
  scope <- str_extract(x, "\\((?:con|sin)[^)]*\\)")
  base  <- str_squish(str_remove(x, "\\((?:con|sin)[^)]*\\)"))
  list(base = base, scope = str_squish(scope))
}

#' Trailing footnote markers attached to entity names ("Alfin Banco 4/",
#' "Scotiabank Peru *", "Banco BCI Peru**"). They carry no entity information;
#' extracted here so they are recorded rather than silently dropped.
bank_footnote <- function(x) {
  m <- str_extract(str_squish(as.character(x)), "(?:[0-9]+/|\\*+)\\s*$")
  str_squish(ifelse(is.na(m), "", m))
}

#' Canonical form for matching: accent-free, uppercase, punctuation-normalized,
#' "B." expanded to "BANCO". Deliberately conservative -- it strips only
#' trailing footnote markers and non-substantive legal suffixes (S.A., S.A.A.),
#' never a distinguishing word.
canon_bank <- function(x) {
  s <- str_squish(as.character(x))
  s <- str_remove(s, "(?:[0-9]+/|\\*+)\\s*$")     # trailing footnote marker
  s <- norm_txt(s)
  s <- str_remove(s, "\\bS\\s*[.]?\\s*A\\s*[.]?\\s*A?\\s*[.]?\\s*$")  # S.A. / S.A.A.
  s <- str_replace_all(s, "[.,;]+", " ")
  s <- str_replace(s, "^B\\s+", "BANCO ")
  s <- str_replace(s, "^B$", "BANCO")
  str_squish(s)
}

#' Standardize a vector of raw entity names against the manual map.
#' Ladder: (1) exact canonical, (2) canonical after scope-suffix extraction,
#' (3) unmatched -> review_required. No fuzzy/similarity matching, ever.
standardize_banks <- function(raw, source_tag, manual) {
  sp <- split_scope(raw)
  tibble(bank_raw = raw, scope_note = sp$scope, bank_footnote = bank_footnote(raw),
         canon_full = canon_bank(raw), canon_base = canon_bank(sp$base)) %>%
    left_join(manual %>% select(canon_excel, std1 = bank_standardized, note1 = note,
                                rev1 = review_required),
              by = c("canon_full" = "canon_excel")) %>%
    left_join(manual %>% select(canon_excel, std2 = bank_standardized, note2 = note,
                                rev2 = review_required),
              by = c("canon_base" = "canon_excel")) %>%
    mutate(
      bank_standardized = coalesce(std1, std2),
      mapping_method = case_when(
        !is.na(std1) ~ "exact_canonical",
        !is.na(std2) ~ "canonical_after_scope_strip",
        TRUE         ~ "unmatched"
      ),
      mapping_note   = coalesce(note1, note2),
      review_required = case_when(
        is.na(bank_standardized) ~ TRUE,
        TRUE ~ coalesce(rev1, rev2, FALSE)
      ),
      mapping_confidence = case_when(
        mapping_method == "unmatched"                   ~ "none",
        review_required                                 ~ "low",
        mapping_method == "exact_canonical"             ~ "high",
        TRUE                                            ~ "medium"
      ),
      source = source_tag
    ) %>%
    select(bank_raw, bank_standardized, scope_note, bank_footnote, source,
           mapping_method, mapping_note, mapping_confidence, review_required)
}

# ==============================================================================
# Generic collector -- shared by 01_collect_direct.R and 01_collect_indirect.R
# ==============================================================================

#' Enumerate a cuadro's monthly URLs, download with caching, write a manifest.
#'
#' @param code      cuadro code, e.g. "B-2343" / "B-2322"
#' @param start_ym  c(year, month) inclusive
#' @param end_ym    c(year, month) inclusive
#' @param raw_root  cache root, e.g. "data/raw/sbs_boletin"
#' @return the manifest tibble (also written to <raw_root>/manifest_<code>.csv)
sbs_collect <- function(code, start_ym, end_ym, raw_root = "data/raw/sbs_boletin",
                        sleep = 0.25) {
  a <- start_ym[1] * 12L + (start_ym[2] - 1L)
  b <- end_ym[1]   * 12L + (end_ym[2]   - 1L)
  k <- a:b
  grid <- tibble(
    cuadro       = code,
    report_year  = k %/% 12L,
    report_month = (k %% 12L) + 1L
  )
  grid$report_date <- as.Date(vapply(seq_len(nrow(grid)),
    function(i) as.numeric(month_end(grid$report_year[i], grid$report_month[i])),
    numeric(1)), origin = "1970-01-01")
  grid$url  <- vapply(seq_len(nrow(grid)),
    function(i) sbs_url(code, grid$report_year[i], grid$report_month[i]), character(1))
  grid$dest <- vapply(seq_len(nrow(grid)),
    function(i) sbs_dest(raw_root, code, grid$report_year[i], grid$report_month[i]), character(1))

  cat(sprintf("Enumerated %d monthly URLs for %s (%d-%02d .. %d-%02d)\n\n",
              nrow(grid), code, start_ym[1], start_ym[2], end_ym[1], end_ym[2]))

  n <- nrow(grid); results <- vector("list", n)
  for (i in seq_len(n)) {
    r <- sbs_download(grid$url[i], grid$dest[i], sleep = sleep)
    grid$dest[i] <- r$dest[[1]]     # may have been renamed to its true extension
    results[[i]] <- r
    if (i %% 25L == 0L || i == n) {
      cat(sprintf("  [%3d/%3d] %s  cached=%s\n", i, n, basename(grid$dest[i]), r$cached))
      flush.console()
    }
    if (!is.na(r$error)) cat(sprintf("  !! %s -> %s\n", basename(grid$dest[i]), r$error))
  }
  dl <- bind_rows(results)

  manifest <- grid %>%
    select(cuadro, report_year, report_month, report_date, url, dest) %>%
    bind_cols(dl %>% select(http_status, bytes, cached, error)) %>%
    mutate(
      available         = file.exists(dest) & !is.na(bytes) & bytes > SBS_MIN_BYTES,
      detected_format   = vapply(dest, sbs_format, character(1), USE.NAMES = FALSE),
      sha256            = vapply(dest, sbs_sha256, character(1), USE.NAMES = FALSE),
      download_datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
    ) %>%
    relocate(available, detected_format, .after = bytes)

  path <- file.path(raw_root, sprintf("manifest_%s.csv", code))
  dir.create(raw_root, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(manifest, path)

  cat("\n--- summary ---\n")
  cat(sprintf("files enumerated : %d\n", nrow(manifest)))
  cat(sprintf("available        : %d\n", sum(manifest$available)))
  cat(sprintf("newly downloaded : %d\n", sum(!manifest$cached & manifest$available)))
  cat(sprintf("served from cache: %d\n", sum(manifest$cached)))
  cat(sprintf("unavailable      : %d\n", sum(!manifest$available)))
  if (any(!manifest$available)) {
    cat("\nUnavailable months (recorded, not silently dropped):\n")
    print(as.data.frame(manifest[!manifest$available,
          c("report_year", "report_month", "http_status", "bytes", "error")]), row.names = FALSE)
  }
  cat("\nContainer format by year (true signature, not the .XLS extension):\n")
  print(as.data.frame(manifest %>% filter(available) %>% count(report_year, detected_format) %>%
        tidyr::pivot_wider(names_from = detected_format, values_from = n, values_fill = 0L)),
        row.names = FALSE)
  brk <- manifest %>% filter(available) %>% arrange(report_year, report_month) %>%
    mutate(prev = lag(detected_format)) %>% filter(!is.na(prev), prev != detected_format)
  if (nrow(brk)) {
    cat("\nFormat transitions:\n")
    print(as.data.frame(brk[, c("report_year", "report_month", "prev", "detected_format")]),
          row.names = FALSE)
  }
  cat(sprintf("\nManifest written: %s\n", path))
  invisible(manifest)
}
