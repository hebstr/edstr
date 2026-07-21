# fold+re2 matcher: a drop-in for str_extract_all() on the giant-alternation
# clinical pattern. re2's automaton runs the ~189-branch alternation in one
# linear pass where ICU backtracking re-evaluates each branch per document.
#
# re2's \b is ASCII-only, so it would miss accented French clinical terms at
# word edges. The fix moves the boundary out of the engine: fold the text 1:1
# per char (NFD; strip marks; NFC) so ASCII \b lands on the same edges, locate
# on the folded text (byte offsets), convert byte->char, then slice the ORIGINAL
# to recover the accented surface forms. Guarded fallback to ICU on the rare
# doc whose fold is not 1:1 in characters.

.re2_fold <- \(x) stri_trans_general(x, "NFD; [:Nonspacing Mark:] Remove; NFC")

.re2_prepare <- \(text) {
  folded <- .re2_fold(text)

  list(
    text = text,
    folded = folded,
    ascii = nchar(folded, "bytes") == nchar(folded, "chars"),
    len_ok = nchar(folded, "chars") == nchar(text, "chars")
  )
}

.re2_extract_prepared <- \(prep, pattern) {
  pattern <- as.character(pattern)
  loc <- re2_locate_all(prep$folded, re2_regexp(pattern))
  icu_pattern <- regex(pattern, multiline = TRUE)

  map2(loc, seq_along(prep$folded), \(m, i) {
    if (nrow(m) == 0) {
      return(character(0))
    }
    if (!prep$len_ok[i]) {
      return(str_extract_all(prep$text[i], icu_pattern)[[1]])
    }
    if (prep$ascii[i]) {
      return(stri_sub(prep$text[i], m[, "begin"], m[, "end"]))
    }
    cp <- utf8ToInt(prep$folded[i])
    bl <- 1L + (cp >= 0x80) + (cp >= 0x800) + (cp >= 0x10000)
    cb <- findInterval(m[, "begin"], cumsum(c(1L, bl[-length(bl)])))
    ce <- findInterval(m[, "end"], cumsum(bl))
    stri_sub(prep$text[i], cb, ce)
  })
}

.re2_extract_all <- \(text, pattern) {
  .re2_extract_prepared(.re2_prepare(text), pattern)
}
