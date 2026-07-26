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

test_that("edstr_import() refuses a missing password outside an interactive session", {
  tmp <- withr::local_tempdir()
  connect <- file.path(tmp, "connect.yml")
  writeLines("default:\n  db:\n    driver: oracle\n", connect)

  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_overwrite = TRUE,
    rlang_interactive = FALSE
  )

  # a prompt would answer itself with "", which the server counts as a
  # failed attempt rather than a missing credential
  expect_error(
    edstr_import(query = "SELECT 1", connect_dir = connect),
    "required outside an interactive session"
  )

  # `Sys.getenv()` on an unset variable yields "", which is not NULL and so
  # reaches the server unchanged
  expect_error(
    edstr_import(query = "SELECT 1", connect_dir = connect, password = ""),
    "is empty"
  )
})

test_that("edstr_import() loads the parquet cache when edstr_overwrite is FALSE", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test",
    edstr_overwrite = FALSE
  )

  expected <- data.frame(x = 1:3)
  nanoparquet::write_parquet(expected, file.path(tmp, "test_import.parquet"))

  result <- suppressMessages(edstr_import(query = "SELECT 1"))

  expect_equal(as.data.frame(result), expected)
})
