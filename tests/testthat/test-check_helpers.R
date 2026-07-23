test_that("check_config() returns correct paths", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "myfile"
  )

  result <- edstr:::check_config("import")

  expect_equal(as.character(result$dir), as.character(tmp))
  expect_equal(result$file, "myfile_import")
  expect_equal(result$save, fs::path(tmp, "myfile_import", ext = "parquet"))
})

test_that("check_config() errors when edstr_dirname is not set", {
  withr::local_options(edstr_dirname = NULL)

  expect_error(
    edstr:::check_config("import"),
    "edstr_dirname"
  )
})

test_that("check_id_key() errors when no column can serve as a key", {
  data <- data.frame(doc_id = c("a", "a"), texte = c("x", "y"))

  expect_error(
    edstr:::check_id_key(data = data, exclude = "texte"),
    "no primary key found"
  )
})

test_that("check_id_key() errors when several columns are candidates", {
  data <- data.frame(doc_id = 1:2, patient_id = 3:4, texte = c("x", "y"))

  expect_error(
    edstr:::check_id_key(data = data, exclude = "texte"),
    "multiple candidate primary keys"
  )
})

test_that("check_id_key() returns every candidate when error is FALSE", {
  data <- data.frame(doc_id = 1:2, patient_id = 3:4, texte = c("x", "y"))

  expect_setequal(
    edstr:::check_id_key(data = data, exclude = "texte", error = FALSE),
    c("doc_id", "patient_id")
  )
})

test_that("check_id_group() errors when the group column is absent", {
  data <- data.frame(doc_id = 1:2, texte = c("x", "y"))

  expect_error(
    edstr:::check_id_group(data = data, id = "patient_id"),
    "not found"
  )
})

test_that("check_id_group() errors when the group column has missing values", {
  data <- data.frame(
    doc_id = 1:2,
    patient_id = c("P1", NA),
    texte = c("x", "y")
  )

  expect_error(
    edstr:::check_id_group(data = data, id = "patient_id"),
    "contains missing values"
  )
})

test_that("check_id_group() returns NULL when no group is requested", {
  data <- data.frame(doc_id = 1:2, texte = c("x", "y"))

  expect_null(edstr:::check_id_group(data = data, id = NULL))
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

test_that("cli_check() aborts in a non-interactive session", {
  withr::local_options(edstr_overwrite = NULL)

  local_mocked_bindings(is_interactive = \() FALSE, .package = "rlang")
  local_mocked_bindings(
    menu = \(...) stop("menu() must not be reached"),
    .package = "edstr"
  )

  expect_error(
    suppressMessages(
      edstr:::cli_check(
        config_file = "test_file",
        fun_save = \() "saved",
        fun_load = \() "loaded"
      )
    ),
    "non-interactive"
  )
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
