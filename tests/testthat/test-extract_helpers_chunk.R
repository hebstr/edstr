chunk_fixture <- \() {
  tibble::tibble(
    id_doc = sprintf("d%02d", 1:12),
    id_pat = rep(c("p1", "p2", "p3", "p4"), each = 3),
    sej_date = rep(c("2024-03-01", "2024-01-01", "2024-02-01"), times = 4),
    doc_texte = rep(
      c(
        '<p class="t">diabete de type 2</p>',
        '<p class="t">cancer du sein</p>',
        '<p class="t">examen sans particularite</p>'
      ),
      times = 4
    )
  )
}

chunk_concepts <- c(diabete = "diabet", cancer = "cancer")

local_chunk_config <- \(data = chunk_fixture(), env = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = env)

  withr::local_options(
    .local_envir = env,
    edstr_dirname = tmp,
    edstr_filename = "test_chunk",
    edstr_text = "doc_texte",
    edstr_overwrite = TRUE
  )

  nanoparquet::write_parquet(data, check_config("clean")$save)

  tmp
}

test_that("chunk_n: TRUE takes the documented default and a count is validated", {
  expect_null(.extract_chunk_n(NULL))
  expect_null(.extract_chunk_n(FALSE))
  expect_equal(.extract_chunk_n(TRUE), 15000L)
  expect_equal(.extract_chunk_n(500), 500L)
  expect_error(.extract_chunk_n(0), "whole number")
  expect_error(.extract_chunk_n(1.5), "whole number")
  expect_error(.extract_chunk_n(c(10, 20)), "whole number")
  expect_error(.extract_chunk_n("500"), "whole number")
})

test_that("chunk_plan: the bounds partition the corpus", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  local_chunk_config()

  plan <- .extract_chunk_plan(
    path = check_config("clean")$save,
    id = "id_doc",
    group = "id_pat",
    text_input = "doc_texte",
    chunk = 6L,
    chunk_sort = NULL,
    concept_keys = c("diabete", "cancer")
  )

  expect_equal(plan$doc_total, 12L)
  expect_equal(plan$bound$chunk, 1:2)
  expect_equal(plan$bound$doc, c(6L, 6L))
  expect_equal(plan$bound$lo, c("p1", "p3"))
  expect_equal(plan$bound$hi, c("p2", "p4"))
  expect_equal(plan$cols$schema, names(chunk_fixture()))
  expect_equal(plan$cols$meta, c("id_doc", "sej_date"))
})

test_that("chunk_plan: a single chunk covers a corpus smaller than the budget", {
  skip_if_not_installed("duckdb")

  local_chunk_config()

  plan <- .extract_chunk_plan(
    path = check_config("clean")$save,
    id = "id_doc",
    group = "id_pat",
    text_input = "doc_texte",
    chunk = 15000L,
    chunk_sort = NULL,
    concept_keys = character()
  )

  expect_equal(nrow(plan$bound), 1L)
  expect_equal(plan$bound$doc, 12L)
})

test_that("chunk_plan: a duplicated or missing id aborts", {
  skip_if_not_installed("duckdb")

  data <- chunk_fixture()
  data$id_doc[[2]] <- data$id_doc[[1]]

  local_chunk_config(data)

  expect_error(
    .extract_chunk_plan(
      path = check_config("clean")$save,
      id = "id_doc",
      group = "id_pat",
      text_input = "doc_texte",
      chunk = 6L,
      chunk_sort = NULL,
      concept_keys = character()
    ),
    "unique"
  )
})

test_that("chunk_plan: a missing group value aborts", {
  skip_if_not_installed("duckdb")

  data <- chunk_fixture()
  data$id_pat[[1]] <- NA_character_

  local_chunk_config(data)

  expect_error(
    .extract_chunk_plan(
      path = check_config("clean")$save,
      id = "id_doc",
      group = "id_pat",
      text_input = "doc_texte",
      chunk = 6L,
      chunk_sort = NULL,
      concept_keys = character()
    ),
    "missing values"
  )
})

test_that("chunk_plan: a column the output builds for itself aborts", {
  skip_if_not_installed("duckdb")

  data <- chunk_fixture()
  data$concept <- "x"

  local_chunk_config(data)

  expect_error(
    .extract_chunk_plan(
      path = check_config("clean")$save,
      id = "id_doc",
      group = "id_pat",
      text_input = "doc_texte",
      chunk = 6L,
      chunk_sort = NULL,
      concept_keys = c("diabete", "cancer")
    ),
    "Reserved"
  )
})

test_that("chunk_plan: an unknown column name aborts", {
  skip_if_not_installed("duckdb")

  local_chunk_config()

  args <- list(
    path = check_config("clean")$save,
    id = "id_doc",
    group = "id_pat",
    text_input = "doc_texte",
    chunk = 6L,
    concept_keys = character()
  )

  expect_error(
    do.call(.extract_chunk_plan, c(args, list(chunk_sort = "sej_absent"))),
    "not found"
  )
  expect_error(
    do.call(
      .extract_chunk_plan,
      c(utils::modifyList(args, list(group = "pat_absent")), list(chunk_sort = NULL))
    ),
    "not found"
  )
})

test_that("chunk_plan: a group column out of file order warns", {
  skip_if_not_installed("duckdb")

  local_chunk_config(chunk_fixture()[c(1:3, 7:9, 4:6, 10:12), ])

  expect_warning(
    .extract_chunk_plan(
      path = check_config("clean")$save,
      id = "id_doc",
      group = "id_pat",
      text_input = "doc_texte",
      chunk = 6L,
      chunk_sort = NULL,
      concept_keys = character()
    ),
    "file order"
  )
})

test_that("chunk_plan: a group larger than the budget warns and names the cause", {
  skip_if_not_installed("duckdb")

  local_chunk_config()
  withr::local_options(cli.width = 200)

  warned <- tryCatch(
    .extract_chunk_plan(
      path = check_config("clean")$save,
      id = "id_doc",
      group = "id_pat",
      text_input = "doc_texte",
      chunk = 2L,
      chunk_sort = NULL,
      concept_keys = character()
    ),
    warning = \(cnd) cnd
  )

  expect_s3_class(warned, "warning")

  msg <- conditionMessage(warned)

  expect_match(msg, "4 groups are larger than the `chunk` budget of 2 documents")
  expect_match(msg, 'The widest is "p1", carrying 3 documents')
  expect_match(msg, 'A chunk cannot split a "id_pat"')
  expect_match(msg, "straddling a boundary goes whole into the later chunk")
})

test_that("chunk_plan: a chunk overshooting on a straddling group stays silent", {
  skip_if_not_installed("duckdb")

  local_chunk_config()

  plan <- expect_no_warning(
    .extract_chunk_plan(
      path = check_config("clean")$save,
      id = "id_doc",
      group = "id_pat",
      text_input = "doc_texte",
      chunk = 4L,
      chunk_sort = NULL,
      concept_keys = character()
    )
  )

  expect_equal(plan$bound$doc, c(3L, 3L, 6L))
})

test_that("edstr_extract: the chunked run reproduces the single-pass output", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  data <- chunk_fixture()

  ref_dir <- withr::local_tempdir()
  ref <- withr::with_options(
    list(
      edstr_dirname = ref_dir,
      edstr_filename = "test_ref",
      edstr_text = "doc_texte",
      edstr_overwrite = TRUE
    ),
    suppressMessages(suppressWarnings(edstr_extract(
      data = data,
      concepts = chunk_concepts,
      id = "id_doc",
      group = "id_pat",
      ngram_max = 2
    )))
  )

  local_chunk_config(data)

  local_mocked_bindings(
    .extract_chunk_spawn = \(job, config, path, cols, args) {
      .extract_chunk_local(job, config, path, cols, args)
    }
  )

  got <- suppressMessages(suppressWarnings(edstr_extract(
    concepts = chunk_concepts,
    id = "id_doc",
    group = "id_pat",
    ngram_max = 2,
    chunk = 6
  )))

  expect_equal(got$summary$token, ref$summary$token)
  expect_equal(got$summary$concept, ref$summary$concept)
  expect_equal(got$summary$params, ref$summary$params)
  expect_equal(got$data$extract, ref$data$extract)
  expect_equal(got$data$note, ref$data$note)
  expect_equal(got$data$base, ref$data$base)
  expect_equal(got$count$final, ref$count$final)
  expect_equal(got$count$init, ref$count$init)
  expect_equal(got$regex$final, ref$regex$final)
  expect_equal(got$regex$replace, ref$regex$replace)
  expect_equal(got$unmatched$n_no_concept, ref$unmatched$n_no_concept)
  expect_equal(nrow(got$mismatched), nrow(ref$mismatched))
})

test_that("edstr_extract: the chunked run saves the three files and clears the chunks", {
  skip_if_not_installed("duckdb")

  tmp <- local_chunk_config()

  local_mocked_bindings(
    .extract_chunk_spawn = \(job, config, path, cols, args) {
      .extract_chunk_local(job, config, path, cols, args)
    }
  )

  suppressMessages(suppressWarnings(edstr_extract(
    concepts = chunk_concepts,
    id = "id_doc",
    group = "id_pat",
    chunk = 6
  )))

  expect_true(all(fs::file_exists(fs::path(
    tmp,
    "extract",
    paste0("test_chunk_extract.", c("rds", "json", "xlsx"))
  ))))
  expect_length(fs::dir_ls(tmp, type = "directory", regexp = "extract_chunk_"), 0)
})

test_that("edstr_extract: the chunked run orders the output by the sort argument", {
  skip_if_not_installed("duckdb")

  local_chunk_config()

  local_mocked_bindings(
    .extract_chunk_spawn = \(job, config, path, cols, args) {
      .extract_chunk_local(job, config, path, cols, args)
    }
  )

  got <- suppressMessages(suppressWarnings(edstr_extract(
    concepts = chunk_concepts,
    id = "id_doc",
    group = "id_pat",
    chunk = 6,
    chunk_sort = "sej_date"
  )))

  expect_equal(
    got$data$extract$sej_date[got$data$extract$id_pat == "p1"],
    c("2024-01-01", "2024-03-01")
  )
  expect_equal(got$data$extract$n, seq_len(nrow(got$data$extract)))
  expect_equal(got$data$extract$id_pat, rep(c("p1", "p2", "p3", "p4"), each = 2))
})

chunk_stub <- \(ids, params) {
  list(
    summary = list(params = params),
    data = list(base = tibble::tibble(id_doc = ids))
  )
}

test_that("chunk_check: a chunk run with other arguments aborts the combine", {
  chunks <- list(
    chunk_stub(1:2, list(ngram_max = 1)),
    chunk_stub(3:4, list(ngram_max = 2))
  )

  expect_error(
    .extract_chunk_check(
      chunks,
      c("a.rds", "b.rds"),
      list(ngram_max = 1),
      4L,
      "id_doc"
    ),
    "other arguments"
  )

  expect_no_error(
    .extract_chunk_check(
      chunks[1],
      "a.rds",
      list(ngram_max = 1),
      2L,
      "id_doc"
    )
  )
})

test_that("chunk_check: chunks that do not partition the corpus abort", {
  params <- list(ngram_max = 1)

  expect_error(
    .extract_chunk_check(
      list(chunk_stub(1:2, params), chunk_stub(2:3, params)),
      c("a.rds", "b.rds"),
      params,
      4L,
      "id_doc"
    ),
    "do not partition"
  )

  expect_error(
    .extract_chunk_check(
      list(chunk_stub(1:2, params), chunk_stub(3:4, params)),
      c("a.rds", "b.rds"),
      params,
      5L,
      "id_doc"
    ),
    "do not partition"
  )
})

test_that("edstr_extract: chunk rejects the arguments it cannot honour", {
  withr::local_options(
    edstr_dirname = withr::local_tempdir(),
    edstr_filename = "test_guard",
    edstr_text = "doc_texte",
    edstr_overwrite = TRUE
  )

  guard <- \(...) {
    edstr_extract(
      concepts = chunk_concepts,
      id = "id_doc",
      group = "id_pat",
      chunk = 6,
      ...
    )
  }

  expect_error(guard(data = chunk_fixture()), "reads the Parquet cache")
  expect_error(guard(sample = 5), "cannot be combined")
  expect_error(guard(seed = 1), "cannot be combined")
  expect_error(guard(ano_hash = "pat"), "cannot be combined")
  expect_error(guard(ano_hide = "pat"), "cannot be combined")
})

test_that("edstr_extract: chunk requires the id and group columns", {
  withr::local_options(
    edstr_dirname = withr::local_tempdir(),
    edstr_filename = "test_keys",
    edstr_text = "doc_texte",
    edstr_overwrite = TRUE
  )

  expect_error(
    edstr_extract(concepts = chunk_concepts, group = "id_pat", chunk = 6),
    "\\{\\.arg id\\}|`id`"
  )
  expect_error(
    edstr_extract(concepts = chunk_concepts, id = "id_doc", chunk = 6),
    "\\{\\.arg group\\}|`group`"
  )
})

test_that("edstr_extract: a missing data without chunk still aborts", {
  withr::local_options(
    edstr_dirname = withr::local_tempdir(),
    edstr_filename = "test_nodata",
    edstr_text = "doc_texte",
    edstr_overwrite = TRUE
  )

  expect_error(
    edstr_extract(concepts = chunk_concepts),
    "is not set"
  )
})

test_that("edstr_extract: chunk aborts when the clean Parquet cache is absent", {
  skip_if_not_installed("duckdb")

  withr::local_options(
    edstr_dirname = withr::local_tempdir(),
    edstr_filename = "test_nocache",
    edstr_text = "doc_texte",
    edstr_overwrite = TRUE
  )

  expect_error(
    edstr_extract(
      concepts = chunk_concepts,
      id = "id_doc",
      group = "id_pat",
      chunk = 6
    ),
    "not found"
  )
})
