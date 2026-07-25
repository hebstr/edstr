# fold+re2 matcher: a drop-in for str_extract_all() on the giant-alternation
# clinical pattern. re2's automaton runs the ~189-branch alternation in one
# linear pass where ICU backtracking re-evaluates each branch per document.
#
# The fold uses the same Latin-ASCII transliteration as tokenisation, so a
# matched token and the folded source are the same string by construction, and
# re2's ASCII-only \b lands on real word edges. Latin-ASCII is not length
# preserving (oe-ligature -> "oe"), so positions are carried back through two
# maps: re2 byte offsets -> folded character indices -> original character
# indices, the original then being sliced to recover the true surface form.
#
# The second map relies on Latin-ASCII being context free, i.e. transliterating
# a string character by character equals transliterating it whole. Per document,
# the per-character widths must sum to the whole-string folded length; a
# mismatch falls back to ICU.

# ICU's Latin-ASCII is ~3x slower than an NFD-based fold over the same text, and
# the fold runs over the whole corpus twice per extraction. `NFD; Mn Remove; NFC`
# reproduces it everywhere except on a set of code points derived offline in
# `data-raw/re2_tables.R`: those Latin-ASCII rewrites and NFD does not are
# expanded first, those Latin-ASCII leaves alone and NFD would rewrite are held
# inert through the fold. Output is identical to Latin-ASCII, per code point over
# all of Unicode.
#
# Only the code points actually present in `x` are repaired, which is what keeps
# the repair free: a clinical corpus carries a couple of dozen of the ~4300.
# a character class of escaped code points. The set holds combining marks and
# zero-width characters, which attach to their neighbour when written literally
# and silently mangle the class.
.re2_char_class <- \(char) {
  code_point <- vapply(char, utf8ToInt, integer(1), USE.NAMES = FALSE)

  escaped <- ifelse(
    code_point <= 0xFFFF,
    sprintf("\\u%04X", code_point),
    sprintf("\\U%08X", code_point)
  )

  paste0("[", paste(escaped, collapse = ""), "]")
}

.re2_fold <- \(x) {
  residue <- stri_replace_all_regex(x, "[\\u0000-\\u007F]+", "")
  present <- intToUtf8(
    unique(utf8ToInt(paste(residue, collapse = ""))),
    multiple = TRUE
  )

  if (length(present) == 0) {
    return(x)
  }

  protect <- .re2_protect[.re2_protect %in% present]

  # a string carrying one of these cannot take the fast fold, so it goes to ICU;
  # they are rare enough that this stays a handful of documents, and in the worst
  # case the whole vector simply falls back to Latin-ASCII
  icu <- rep(FALSE, length(x))

  if (length(protect) > 0) {
    hit <- stri_detect_regex(x, .re2_char_class(protect))
    icu <- hit & !is.na(hit)
  }

  expand <- .re2_expand_from %in% present
  out <- x

  if (any(expand)) {
    out[!icu] <- stri_replace_all_regex(
      str = out[!icu],
      pattern = .re2_expand_pattern[expand],
      replacement = .re2_expand_replacement[expand],
      vectorize_all = FALSE
    )
  }

  out[!icu] <- stri_trans_general(out[!icu], "NFD; [:Mn:] Remove; NFC")

  if (any(icu)) {
    out[icu] <- stri_trans_general(x[icu], "Latin-ASCII")
  }

  out
}

# position in the output -> index of the input unit that produced it,
# given the cumulative output width of the input units
.re2_map <- \(pos, cumwidth) findInterval(pos - 1L, cumwidth) + 1L

.re2_widths <- \(text, expand) {
  width <- vector("list", length(text))

  if (!any(expand)) {
    return(width)
  }

  table <- .re2_width

  width[expand] <- lapply(text[expand], \(x) {
    cp <- utf8ToInt(x)
    w <- table[cp]

    if (anyNA(w)) {
      astral <- is.na(w)
      w[astral] <- nchar(
        .re2_fold(intToUtf8(cp[astral], multiple = TRUE)),
        "chars"
      )
    }

    cumsum(w)
  })

  width
}

.re2_prepare <- \(text) {
  folded <- .re2_fold(text)
  folded_len <- nchar(folded, "chars")

  # Latin-ASCII never deletes a character (verified over all of Unicode), so an
  # unchanged length means every character mapped to exactly one: no offset
  # map is needed for that document.
  known <- !is.na(folded_len)
  expand <- known & folded_len != nchar(text, "chars")

  list(
    text = text,
    folded = folded,
    ascii = known & nchar(folded, "bytes") == folded_len,
    folded_len = folded_len,
    expand = expand,
    width = .re2_widths(text, expand)
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

    if (prep$ascii[i]) {
      begin <- m[, "begin"]
      end <- m[, "end"]
    } else {
      cp <- utf8ToInt(prep$folded[i])
      bytes <- cumsum(1L + (cp >= 0x80) + (cp >= 0x800) + (cp >= 0x10000))
      begin <- .re2_map(m[, "begin"], bytes)
      end <- .re2_map(m[, "end"], bytes)
    }

    if (prep$expand[i]) {
      width <- prep$width[[i]]

      if (width[length(width)] != prep$folded_len[i]) {
        return(str_extract_all(prep$text[i], icu_pattern)[[1]])
      }

      begin <- .re2_map(begin, width)
      end <- .re2_map(end, width)
    }

    stri_sub(prep$text[i], begin, end)
  })
}
