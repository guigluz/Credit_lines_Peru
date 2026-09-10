#!/usr/bin/env Rscript
#
# Compile every .tex table fragment in this folder to a standalone PDF.
# Re-run any time a table is added, removed, or edited.
#
# Usage:
#   Rscript build_tables.R

suppressPackageStartupMessages(library(tinytex))

folder <- "output/tables"
out_dir <- file.path(folder, "_gallery")

# varwidth needs an explicit max, or it silently clips content wider than a
# normal page's textwidth (~355pt) instead of growing the page to fit it --
# 50cm comfortably covers even the widest multi-column regression tables.
preamble <- "
\\documentclass[preview,border=6pt,varwidth=50cm]{standalone}
\\usepackage{booktabs}
\\usepackage{threeparttable}
\\usepackage{amssymb}
\\usepackage{multirow}
\\usepackage{siunitx}
\\usepackage{dcolumn}
\\begin{document}
"
postamble <- "\n\\end{document}\n"

unlink(out_dir, recursive = TRUE)
dir.create(out_dir, recursive = TRUE)

tex_files <- list.files(folder, pattern = "\\.tex$", full.names = TRUE)
tex_files <- tex_files[basename(tex_files) != "build_tables.R"]

for (tex_path in tex_files) {
  tex_filename <- basename(tex_path)
  name <- tools::file_path_sans_ext(tex_filename)
  cat("Compiling", tex_filename, "... ")

  workdir <- tempfile("build_tables_")
  dir.create(workdir)
  body <- paste(readLines(tex_path, warn = FALSE), collapse = "\n")
  writeLines(paste0(preamble, body, postamble), file.path(workdir, paste0(name, ".tex")))

  # tinytex::latexmk() writes .aux/.log next to the R session's actual
  # working directory (not the target file's) when given an absolute path --
  # switching into workdir keeps every intermediate file confined there.
  owd <- setwd(workdir)
  ok <- tryCatch({
    withCallingHandlers(
      tinytex::latexmk(paste0(name, ".tex"), engine = "pdflatex", clean = TRUE),
      message = function(m) invokeRestart("muffleMessage")
    )
    TRUE
  }, error = function(e) FALSE)
  setwd(owd)

  if (ok) {
    file.copy(file.path(workdir, paste0(name, ".pdf")), file.path(out_dir, paste0(name, ".pdf")))
    cat("ok\n")
  } else {
    cat("FAILED\n")
  }
  unlink(workdir, recursive = TRUE)
}

cat("\nDone. PDFs in", out_dir, "\n")
