test_that("edstr_import() errors when edstr_config is not set", {
  withr::local_options(edstr_dirname = NULL, edstr_filename = NULL)

  expect_error(
    edstr_import(query = "SELECT 1"),
    "edstr_dirname"
  )
})

test_that("edstr_import() errors when query is NULL", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_overwrite = NULL
  )

  expect_error(
    edstr_import(query = NULL),
    "query"
  )
})

test_that("edstr_import() loads existing RDS when edstr_overwrite is FALSE", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_overwrite = FALSE
  )

  expected <- data.frame(x = 1:3)
  saveRDS(expected, file.path(tmp, "test_import.rds"))

  result <- suppressMessages(edstr_import(query = "SELECT 1"))

  expect_equal(result, expected)
})
