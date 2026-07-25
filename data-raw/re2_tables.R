# Builds R/sysdata.rda: the constants .re2_fold() needs to reproduce ICU's
# Latin-ASCII transliteration on top of the much faster NFD-based fold.
#
# The two transliterations diverge in BOTH directions over Unicode, which is why
# a table of characters to expand is not enough on its own:
#   - Latin-ASCII rewrites the character and NFD does not, or differently
#     (oe-ligature -> "oe", TEL sign -> "TEL", guillemet -> "<<"): expand it
#     before folding.
#   - Latin-ASCII leaves the character alone and NFD would rewrite it
#     (OHM SIGN, which NFD canonically decomposes to GREEK CAPITAL OMEGA):
#     shield it from the fold.
#
# Run with: Rscript data-raw/re2_tables.R

library(stringi)

fold_icu <- \(x) stri_trans_general(x, "Latin-ASCII")
fold_nfd <- \(x) stri_trans_general(x, "NFD; [:Mn:] Remove; NFC")

code_point <- setdiff(seq_len(0x10FFFF), 0xD800:0xDFFF)
char <- intToUtf8(code_point, multiple = TRUE)

out_icu <- fold_icu(char)
out_nfd <- fold_nfd(char)
diverge <- which(out_icu != out_nfd)

# A code point may be pre-expanded only if the fast fold is a no-op on what
# Latin-ASCII produced for it. Expansion output is not always ASCII: ICU maps
# COMBINING GRAVE TONE MARK to COMBINING GRAVE ACCENT, still a mark, which the
# fold would then delete. Everything else that diverges falls back to ICU.
stable <- fold_nfd(out_icu[diverge]) == out_icu[diverge]
rewritten <- diverge[out_icu[diverge] != char[diverge] & stable]

.re2_expand_from <- char[rewritten]
.re2_protect <- setdiff(char[diverge], .re2_expand_from)

# Expansion runs on escaped code points rather than on literal strings: ICU's
# fixed-string search skips default-ignorable code points, so a fixed replacement
# silently deletes a BOM anywhere in the input, matched or not.
escape_replacement <- \(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  gsub("$", "\\$", x, fixed = TRUE)
}

.re2_expand_pattern <- ifelse(
  code_point[rewritten] <= 0xFFFF,
  sprintf("\\u%04X", code_point[rewritten]),
  sprintf("\\U%08X", code_point[rewritten])
)

.re2_expand_replacement <- escape_replacement(out_icu[rewritten])

# output width of the fold per code point, for the BMP; astral code points are
# rare enough to be folded on demand
bmp <- seq_len(0xFFFF)
usable <- !(bmp >= 0xD800 & bmp <= 0xDFFF)

.re2_width <- rep(1L, length(bmp))
.re2_width[usable] <- nchar(
  fold_icu(intToUtf8(bmp[usable], multiple = TRUE)),
  "chars"
)

expanded <- char
expanded[rewritten] <- out_icu[rewritten]
reproduced <- fold_nfd(expanded)
reproduced[char %in% .re2_protect] <- out_icu[char %in% .re2_protect]

stopifnot(
  length(.re2_expand_from) == length(.re2_expand_pattern),
  length(.re2_expand_from) == length(.re2_expand_replacement),
  !any(.re2_expand_from %in% .re2_protect),
  all(.re2_width >= 1L),
  # the fold reproduces Latin-ASCII on every code point in Unicode
  identical(reproduced, out_icu)
)

message(
  "expand: ",
  length(.re2_expand_from),
  " | protect: ",
  length(.re2_protect),
  " | width table: ",
  length(.re2_width)
)

save(
  .re2_expand_from,
  .re2_expand_pattern,
  .re2_expand_replacement,
  .re2_protect,
  .re2_width,
  file = "R/sysdata.rda",
  compress = "bzip2",
  version = 3
)
