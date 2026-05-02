test_that("check_config() returns correct paths", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "myfile"
  )

  result <- edstr:::check_config("import")

  expect_equal(as.character(result$dir), as.character(tmp))
  expect_equal(result$file, "myfile_import")
  expect_equal(result$save, fs::path(tmp, "myfile_import", ext = "rds"))
})

test_that("check_config() errors when edstr_dirname is not set", {
  withr::local_options(edstr_dirname = NULL)

  expect_error(
    edstr:::check_config("import"),
    "edstr_dirname"
  )
})

test_that("cli_check() calls fun_load when menu choice is 1", {
  withr::local_options(edstr_overwrite = NULL)

  local_mocked_bindings(is_interactive = \() TRUE, .package = "rlang")
  local_mocked_bindings(menu = \(...) 1, .package = "edstr")

  result <- suppressMessages(
    edstr:::cli_check(
      config_file = "test_file",
      fun_save = \() "saved",
      fun_load = \() "loaded"
    )
  )

  expect_equal(result, "loaded")
})

test_that("cli_check() calls fun_save when menu choice is 2", {
  withr::local_options(edstr_overwrite = NULL)

  local_mocked_bindings(is_interactive = \() TRUE, .package = "rlang")
  local_mocked_bindings(menu = \(...) 2, .package = "edstr")

  result <- suppressMessages(
    edstr:::cli_check(
      config_file = "test_file",
      fun_save = \() "saved",
      fun_load = \() "loaded"
    )
  )

  expect_equal(result, "saved")
})

test_that("cli_check() aborts when menu choice is 3 (cancel)", {
  withr::local_options(edstr_overwrite = NULL)

  local_mocked_bindings(is_interactive = \() TRUE, .package = "rlang")
  local_mocked_bindings(menu = \(...) 3, .package = "edstr")

  expect_error(
    suppressMessages(
      edstr:::cli_check(
        config_file = "test_file",
        fun_save = \() "saved",
        fun_load = \() "loaded"
      )
    ),
    "cancelled"
  )
})
