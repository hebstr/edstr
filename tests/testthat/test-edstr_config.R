test_that("edstr_config() sets R options correctly", {
  withr::local_options(
    edstr_dirname = NULL,
    edstr_filename = NULL,
    edstr_text = NULL,
    edstr_overwrite = NULL
  )

  tmp <- withr::local_tempdir()

  suppressMessages(
    edstr_config(
      edstr_dirname = tmp,
      edstr_filename = "test_prefix",
      edstr_text = "note_text",
      edstr_overwrite = TRUE
    )
  )

  expect_equal(as.character(getOption("edstr_dirname")), as.character(tmp))
  expect_equal(getOption("edstr_filename"), "test_prefix")
  expect_equal(getOption("edstr_text"), "note_text")
  expect_true(getOption("edstr_overwrite"))
})

test_that("edstr_config() creates directory if it does not exist", {
  withr::local_options(
    edstr_dirname = NULL,
    edstr_filename = NULL,
    edstr_text = NULL,
    edstr_overwrite = NULL
  )

  tmp <- withr::local_tempdir()
  new_dir <- file.path(tmp, "subdir")

  suppressMessages(
    edstr_config(
      edstr_dirname = new_dir,
      edstr_filename = "test_prefix"
    )
  )

  expect_true(dir.exists(new_dir))
})

test_that("edstr_config() passes extra options via ...", {
  withr::local_options(
    edstr_dirname = NULL,
    edstr_filename = NULL,
    edstr_custom = NULL
  )

  tmp <- withr::local_tempdir()

  suppressMessages(
    edstr_config(
      edstr_dirname = tmp,
      edstr_filename = "test_prefix",
      edstr_custom = "custom_value"
    )
  )

  expect_equal(getOption("edstr_custom"), "custom_value")
})

test_that("edstr_config() produces CLI output", {
  withr::local_options(
    edstr_dirname = NULL,
    edstr_filename = NULL
  )

  tmp <- withr::local_tempdir()

  expect_message(
    edstr_config(
      edstr_dirname = tmp,
      edstr_filename = "test_prefix"
    )
  )
})

test_that("edstr_config() rejects invalid edstr_dirname", {
  expect_error(edstr_config(edstr_dirname = 123, edstr_filename = "x"))
  expect_error(edstr_config(edstr_dirname = c("a", "b"), edstr_filename = "x"))
})

test_that("edstr_config() rejects invalid edstr_filename", {
  tmp <- withr::local_tempdir()
  expect_error(edstr_config(edstr_dirname = tmp, edstr_filename = 123))
  expect_error(edstr_config(edstr_dirname = tmp, edstr_filename = c("a", "b")))
})

test_that("edstr_config() rejects invalid edstr_text", {
  tmp <- withr::local_tempdir()
  expect_error(edstr_config(edstr_dirname = tmp, edstr_filename = "x", edstr_text = 42))
  expect_error(edstr_config(edstr_dirname = tmp, edstr_filename = "x", edstr_text = c("a", "b")))
})

test_that("edstr_config() rejects invalid edstr_overwrite", {
  tmp <- withr::local_tempdir()
  expect_error(edstr_config(edstr_dirname = tmp, edstr_filename = "x", edstr_overwrite = "yes"))
  expect_error(edstr_config(edstr_dirname = tmp, edstr_filename = "x", edstr_overwrite = 42))
})

test_that("edstr_config() defaults text and overwrite to NULL", {
  withr::local_options(
    edstr_dirname = NULL,
    edstr_filename = NULL,
    edstr_text = "previous",
    edstr_overwrite = TRUE
  )

  tmp <- withr::local_tempdir()

  suppressMessages(
    edstr_config(
      edstr_dirname = tmp,
      edstr_filename = "test_prefix"
    )
  )

  expect_null(getOption("edstr_text"))
  expect_null(getOption("edstr_overwrite"))
})
