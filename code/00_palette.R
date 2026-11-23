# ==============================================================================
# 00_palette.R -- Project-wide discrete colour palette (ggsci Lancet)
# ==============================================================================
# Every discrete colour scale in the figure scripts uses these colours.
#   - Categories take slots 1..n in order (navy, red, green, ...).
#   - Slots 8-9 (gray, near-black) are not used for categories so they cannot be
#     confused with the greys used for background/context series, which caps a
#     plot at 7 categories. Plots with more keep their own colours.
#   - A lone series uses navy; a lone highlight/accent uses red.
#   - Continuous scales (e.g. map fills) are not covered by this palette.
#
# The alpha suffix ggsci adds ("#00468BFF") is stripped so the colours work with
# the base pdf() device every save_fig() uses.
#
# Usage:  source("code/00_palette.R")
# ==============================================================================

LANCET <- setNames(substr(ggsci::pal_lancet()(9), 1, 7),
                   c("navy", "red", "green", "teal", "purple",
                     "salmon", "darkred", "gray", "black"))

lancet_n <- function(n) {
  stopifnot(n <= 7)
  unname(LANCET[seq_len(n)])
}
