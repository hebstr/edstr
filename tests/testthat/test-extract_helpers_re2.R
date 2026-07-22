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
