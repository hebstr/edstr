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

.re2_fold <- \(x) stri_trans_general(x, "Latin-ASCII")

# position in the output -> index of the input unit that produced it,
# given the cumulative output width of the input units
.re2_map <- \(pos, cumwidth) findInterval(pos - 1L, cumwidth) + 1L

.re2_width_table <- \() {
  cp <- seq_len(0xFFFF)
  surrogate <- cp >= 0xD800 & cp <= 0xDFFF

  width <- rep(1L, length(cp))
  width[!surrogate] <- nchar(
    .re2_fold(intToUtf8(cp[!surrogate], multiple = TRUE)),
    "chars"
  )

  width
}

.re2_widths <- \(text, expand) {
  width <- vector("list", length(text))

  if (!any(expand)) {
    return(width)
  }

  table <- .re2_width_table()

  width[expand] <- lapply(text[expand], \(x) {
    cp <- utf8ToInt(x)
    w <- table[cp]

    if (anyNA(w)) {
      astral <- is.na(w)
      w[astral] <- nchar(.re2_fold(intToUtf8(cp[astral], multiple = TRUE)), "chars")
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
  expand <- folded_len != nchar(text, "chars")

  list(
    text = text,
    folded = folded,
    ascii = nchar(folded, "bytes") == folded_len,
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
