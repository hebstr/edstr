fold_icu <- \(x) stringi::stri_trans_general(x, "Latin-ASCII")

test_that("re2_fold: reproduces Latin-ASCII on every divergent code point", {
  char <- c(edstr:::.re2_expand_from, edstr:::.re2_protect)

  expect_equal(edstr:::.re2_fold(char), fold_icu(char))
})

test_that("re2_fold: reproduces Latin-ASCII on mixed multi-character strings", {
  withr::local_seed(1)

  char <- c(edstr:::.re2_expand_from, edstr:::.re2_protect, letters)
  mixed <- vapply(
    1:500,
    \(i) paste(sample(char, 40), collapse = ""),
    character(1)
  )

  expect_equal(edstr:::.re2_fold(mixed), fold_icu(mixed))
})

test_that("re2_fold: leaves ASCII, empty strings and NA alone", {
  x <- c("abc", "", NA_character_, "cafe au lait")

  expect_equal(edstr:::.re2_fold(x), fold_icu(x))
})

test_that("re2_fold: ligatures expand and accents fold", {
  expect_equal(edstr:::.re2_fold("cœur élargi"), "coeur elargi")
})

test_that("re2_fold: a zero-width no-break space survives an expansion", {
  bom <- intToUtf8(0xFEFF)
  x <- c(bom, paste0(bom, "cœur"))

  expect_equal(edstr:::.re2_fold(x), fold_icu(x))
  expect_equal(edstr:::.re2_fold(x)[[1]], bom)
})

test_that("re2_fold: a protected code point falls back without losing its neighbours", {
  ohm <- intToUtf8(0x2126)
  x <- c(paste0("580", ohm, " cœur"), "cœur seul")

  expect_equal(edstr:::.re2_fold(x), fold_icu(x))
})

test_that("re2_width: matches the fold's own output width", {
  code_point <- c(utf8ToInt("œ"), utf8ToInt("é"), utf8ToInt("a"))

  expect_equal(
    edstr:::.re2_width[code_point],
    nchar(fold_icu(intToUtf8(code_point, multiple = TRUE)), "chars")
  )
})

test_that("re2_prepare: expansion is reflected in the offset map", {
  prep <- edstr:::.re2_prepare("cœur")

  expect_equal(prep$folded, "coeur")
  expect_true(prep$expand)
  expect_equal(prep$width[[1]], c(1L, 3L, 4L, 5L))
})

test_that("re2_extract_prepared: slices the original, ligature intact", {
  prep <- edstr:::.re2_prepare(c("le cœur bat", "rien"))

  expect_equal(
    edstr:::.re2_extract_prepared(prep, "coeur"),
    list("cœur", character(0))
  )
})

test_that("re2_width: an astral code point falls outside the table", {
  expect_length(edstr:::.re2_width, 65535L)
  expect_true(is.na(edstr:::.re2_width[utf8ToInt(intToUtf8(0x1F600))]))
})

test_that("re2_prepare: an astral code point keeps the width map aligned", {
  emoji <- intToUtf8(0x1F600)
  prep <- edstr:::.re2_prepare(paste0("cœur ", emoji, " battant fort"))

  expect_true(prep$expand)

  width <- prep$width[[1]]
  expect_equal(width[length(width)], prep$folded_len)

  expect_equal(
    edstr:::.re2_extract_prepared(prep, "(?i)\\b(coeur)\\b"),
    list("cœur")
  )
  expect_equal(
    edstr:::.re2_extract_prepared(prep, "(?i)\\b(fort)\\b"),
    list("fort")
  )
})

test_that("re2_prepare: a missing document does not stop the width map", {
  prep <- edstr:::.re2_prepare(c("cœur battant", NA_character_, "coeur"))

  expect_false(prep$expand[[2]])
  expect_equal(
    edstr:::.re2_extract_prepared(prep, "(?i)\\b(coeur)\\b"),
    list("cœur", character(0), "coeur")
  )
})

test_that("re2_extract_prepared: both offset maps run on a non-ASCII fold", {
  pua <- intToUtf8(0xF0A7)
  prep <- edstr:::.re2_prepare(paste0("le cœur ", pua, " bat fort"))

  expect_false(prep$ascii)
  expect_true(prep$expand)

  expect_equal(
    edstr:::.re2_extract_prepared(prep, "(?i)\\b(coeur)\\b"),
    list("cœur")
  )
  expect_equal(
    edstr:::.re2_extract_prepared(prep, "(?i)\\b(bat)\\b"),
    list("bat")
  )
})

test_that("re2_extract_prepared: a broken width map falls back to ICU", {
  prep <- edstr:::.re2_prepare("cœur battant")

  expect_equal(
    edstr:::.re2_extract_prepared(prep, "(?i)\\b(coeur)\\b"),
    list("cœur")
  )

  # the guard only fires when the per-character widths stop summing to the
  # folded length, which no known code point produces: corrupt the map instead
  broken <- prep
  width <- broken$width[[1]]
  width[length(width)] <- width[length(width)] - 1L
  broken$width[[1]] <- width

  expect_equal(
    edstr:::.re2_extract_prepared(broken, "(?i)\\b(coeur)\\b"),
    list(character(0))
  )
})
