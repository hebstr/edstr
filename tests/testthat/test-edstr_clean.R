test_that("edstr_clean() errors when edstr_config is not set", {
  withr::local_options(edstr_dirname = NULL, edstr_filename = NULL, edstr_text = "x")

  expect_error(
    edstr_clean(data = data.frame(x = 1), replace = c("a" = "b")),
    "edstr_dirname"
  )
})

test_that("edstr_clean() errors when data is not a data.frame", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = "note",
    edstr_overwrite = TRUE
  )

  expect_error(
    edstr_clean(data = "not_a_df", replace = list()),
    "data.frame"
  )
})

test_that("edstr_clean() errors when text is not a character", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = 123,
    edstr_overwrite = TRUE
  )

  expect_error(
    edstr_clean(data = data.frame(note = "abc"), replace = list()),
    "character"
  )
})

test_that("edstr_clean() errors when text column does not exist in data", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = "colonne_inexistante",
    edstr_overwrite = TRUE
  )

  expect_error(
    suppressMessages(
      edstr_clean(data = data.frame(id = 1, note = "abc"), replace = c("a" = "b"))
    ),
    "not found"
  )
})

test_that("edstr_clean() errors when replace is not a named character vector", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = "note",
    edstr_overwrite = TRUE
  )

  df <- data.frame(id = 1, note = "abc")

  expect_error(edstr_clean(data = df, replace = 123), "named character vector")
  expect_error(edstr_clean(data = df, replace = c("a", "b")), "named character vector")
  expect_error(edstr_clean(data = df, replace = list(c("a", "b"))), "named character vector")
})

test_that("edstr_clean() applies regex replacements and saves RDS", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = "note",
    edstr_overwrite = TRUE
  )

  input <- data.frame(
    id = 1:3,
    note = c("foo bar", "baz qux", "foo baz")
  )

  result <- suppressMessages(
    edstr_clean(
      data = input,
      replace = c("foo" = "FOO", "baz" = "BAZ")
    )
  )

  expect_equal(result$note, c("FOO bar", "BAZ qux", "FOO BAZ"))
  expect_true(file.exists(file.path(tmp, "test_clean.rds")))
})

test_that("edstr_clean() applies multiple replacement lists sequentially", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = "note",
    edstr_overwrite = TRUE
  )

  input <- data.frame(id = 1, note = "abc 123 def")

  result <- suppressMessages(
    edstr_clean(
      data = input,
      replace = list(
        c("abc" = "AAA"),
        c("123" = "999")
      )
    )
  )

  expect_equal(result$note, "AAA 999 def")
})

test_that("edstr_clean() overwrites existing RDS when edstr_overwrite is TRUE", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = "note",
    edstr_overwrite = TRUE
  )

  old_data <- data.frame(id = 1, note = "stale")
  rds_path <- file.path(tmp, "test_clean.rds")
  saveRDS(old_data, rds_path)

  input <- data.frame(id = 1, note = "foo bar")

  result <- suppressMessages(
    edstr_clean(data = input, replace = c("foo" = "FOO"))
  )

  expect_equal(result$note, "FOO bar")
  expect_equal(readRDS(rds_path)$note, "FOO bar")
})

test_that("edstr_clean() loads existing RDS when edstr_overwrite is FALSE", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = "note",
    edstr_overwrite = FALSE
  )

  expected <- data.frame(id = 1, note = "already cleaned")
  saveRDS(expected, file.path(tmp, "test_clean.rds"))

  result <- suppressMessages(
    edstr_clean(
      data = data.frame(id = 1, note = "raw"),
      replace = list()
    )
  )

  expect_equal(result, expected)
})

test_that("edstr_clean() uses edstr_text option as default text column", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_text = "contenu",
    edstr_overwrite = TRUE
  )

  input <- data.frame(id = 1, contenu = "hello world")

  result <- suppressMessages(
    edstr_clean(
      data = input,
      replace = c("hello" = "HELLO")
    )
  )

  expect_equal(result$contenu, "HELLO world")
})
