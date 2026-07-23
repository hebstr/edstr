test_that("edstr_extract: a true negative is separated from an empty source", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_no_source",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = c("match", "vrai_negatif", "na", "vide"),
    texte = c(
      '<p class="t">oedeme aigu</p>',
      '<p class="t">examen sans particularite</p>',
      NA_character_,
      ""
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diag = "oedeme"),
      token = 1,
      unmatched_data = TRUE
    )
  )

  expect_setequal(result$unmatched$no_source$doc_id, c("na", "vide"))
  expect_equal(result$unmatched$no_concept$doc_id, "vrai_negatif")
})

test_that("edstr_extract: full pipeline runs and produces expected output", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_extract",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:5),
    texte = c(
      '<p class="t">Patient diabetique equilibre</p>',
      '<p class="t">Pas de diabete retrouve</p>',
      '<p class="t">Cancer du sein gauche</p>',
      '<p class="t">Bilan normal</p>',
      '<p class="t">Diabete de type 2 ancien</p>'
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet"),
      token = 1
    )
  )

  expect_type(result, "list")
  expect_named(
    result,
    c(
      "data",
      "regex",
      "match",
      "count",
      "exclus",
      "unmatched",
      "mismatched",
      "summary",
      "sheets"
    )
  )

  expect_true(nrow(result$data$extract) > 0)
  expect_true("doc_id" %in% names(result$data$extract))
  expect_true("extract" %in% names(result$data$extract))

  save_dir <- file.path(tmp, "extract")
  expect_true(file.exists(file.path(save_dir, "test_extract_extract.xlsx")))
  expect_true(file.exists(file.path(save_dir, "test_extract_extract.rds")))
  expect_true(file.exists(file.path(save_dir, "test_extract_extract.json")))
})

test_that("edstr_extract: extract and note output are stable (refactor oracle)", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_oracle",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:6),
    texte = c(
      '<p class="t">Patient diabetique, cancer du sein gauche</p>',
      '<p class="t">Pas de diabete retrouve au bilan</p>',
      '<p class="t">Cancer du sein droit, metastases</p>',
      '<p class="t">Bilan normal, pas de pathologie</p>',
      '<p class="t">Diabete de type 2 ancien et cancer</p>',
      '<p class="t">Suivi cancer, diabete insulino-requerant</p>'
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet", cancer = "cancer"),
      token = 1
    )
  )

  expect_snapshot_value(result$data$extract, style = "json2")
  expect_snapshot_value(result$data$note, style = "json2")
})

test_that("edstr_extract: loads cached RDS when file exists", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_cache",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:3),
    texte = c(
      '<p class="t">Patient diabetique</p>',
      '<p class="t">Diabete type 1</p>',
      '<p class="t">Bilan normal</p>'
    ),
    stringsAsFactors = FALSE
  )

  result1 <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet"),
      token = 1
    )
  )

  expect_s3_class(result1$data$extract, "data.frame")

  # a sentinel on disk separates loading the cache from recomputing it: the
  # pipeline is deterministic, so an identical result proves neither
  cached <- file.path(tmp, "extract", "test_cache_extract.rds")
  expect_true(file.exists(cached))
  saveRDS(list(data = list(extract = "sentinel")), cached)

  withr::local_options(edstr_overwrite = NULL)
  local_mocked_bindings(is_interactive = \() TRUE, .package = "rlang")
  local_mocked_bindings(menu = \(...) 1, .package = "edstr")

  result2 <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet"),
      token = 1
    )
  )

  expect_equal(result2$data$extract, "sentinel")
})

test_that("edstr_extract: the summary renders every optional block", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_summary",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:6),
    patient_id = c("P1", "P1", "P2", "P2", "P3", "P3"),
    texte = c(
      '<p class="t">Patient diabetique</p>',
      '<p class="t">Diabete type 2</p>',
      '<p class="t">Diabete gestationnel</p>',
      '<p class="t">Diabete insipide</p>',
      '<p class="t">Bilan normal</p>',
      '<p class="t">Examen sans particularite</p>'
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet"),
      token = 1,
      group = "patient_id",
      sample = 5,
      seed = 42,
      unmatched_data = TRUE
    )
  )

  expect_equal(nrow(result$data$base), 5)
  expect_equal(result$summary$params$sample, 5)
  expect_equal(result$summary$params$seed, 42)
  expect_equal(result$summary$params$group, "patient_id")

  expect_true("patient_id" %in% names(result$data$extract))

  # one row of six is dropped, so the counts hold whatever the draw
  expect_gte(nrow(result$data$extract), 3)
  expect_gte(nrow(result$unmatched$no_concept), 1)
})

test_that("edstr_extract: save_as_gt builds one gt table per sheet", {
  skip_if_not_installed("gt")

  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_gt",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:4),
    texte = c(
      '<p class="t">Patient diabetique</p>',
      '<p class="t">Diabete type 2</p>',
      '<p class="t">Suivi diabetologique</p>',
      '<p class="t">Bilan normal</p>'
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet"),
      token = 1,
      exclus_manual = "logique$",
      save_as_gt = TRUE
    )
  )

  expect_named(
    result$sheets$gt,
    c(
      "data",
      "token_regex",
      "token_match",
      "token_count",
      "token_exclusion",
      "concept_count",
      "text_replace",
      "text_regex",
      "text_match",
      "text_count",
      "unmatched",
      "mismatched",
      "params"
    )
  )

  expect_s3_class(result$sheets$gt$data$id, "gt_tbl")
  expect_s3_class(result$sheets$gt$data$text, "gt_tbl")
  expect_s3_class(result$sheets$gt$token_regex, "gt_tbl")
  expect_s3_class(result$sheets$gt$params, "gt_tbl")

  expect_equal(result$exclus$match$manual$texte, "diabetologique")
  expect_gt(nrow(result$exclus$count), 0)
})

test_that("edstr_extract: multiple concepts with intersect", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_intersect",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:4),
    texte = c(
      '<p class="t">Patient diabetique avec cancer</p>',
      '<p class="t">Diabete seul</p>',
      '<p class="t">Cancer seul</p>',
      '<p class="t">Bilan normal</p>'
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet", cancer = "cancer"),
      token = 1,
      intersect = TRUE
    )
  )

  expect_equal(nrow(result$data$extract), 1)
  expect_equal(result$data$extract$doc_id, "1")
})

test_that("edstr_extract: dirname_suffix and filename_suffix affect paths", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_suffix",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:2),
    texte = c(
      '<p class="t">Patient diabetique</p>',
      '<p class="t">Bilan normal</p>'
    ),
    stringsAsFactors = FALSE
  )

  suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet"),
      token = 1,
      dirname_suffix = "custom",
      filename_suffix = "custom"
    )
  )

  save_dir <- file.path(tmp, "extract_custom")

  expect_true(dir.exists(save_dir))
  expect_true(file.exists(file.path(save_dir, "test_suffix_extract_custom.rds")))
  expect_true(
    file.exists(file.path(save_dir, "test_suffix_extract_custom.xlsx"))
  )
  expect_false(dir.exists(file.path(tmp, "extract")))
})

test_that("edstr_extract: accented text in source is matched correctly", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_accents",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:2),
    texte = c(
      '<p class="t">Diab\u00e8te de type 2</p>',
      '<p class="t">Insuffisance r\u00e9nale</p>'
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diabete = "diabet"),
      token = 1
    )
  )

  expect_equal(nrow(result$data$extract), 1)
  expect_equal(result$data$extract$doc_id, "1")
  expect_true(nrow(result$regex$match) > 0)
})

test_that("edstr_extract: ligatures are matched and returned unchanged", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_ligatures",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:3),
    texte = c(
      '<p class="t">Œdème aigu du poumon</p>',
      '<p class="t">Arrêt cardiaque, cœur non réanimé</p>',
      '<p class="t">Fracture du col fémoral</p>'
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(diag = "oedeme|coeur"),
      token = 1
    )
  )

  expect_setequal(result$data$match$doc_id, c("1", "2"))

  expect_equal(nrow(result$mismatched), 0)
  expect_setequal(result$data$extract$doc_id, c("1", "2"))
  expect_setequal(result$regex$match$match, c("Œdème", "cœur"))
})

test_that("edstr_extract: bigram token matching works", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_bigram",
    edstr_text = "texte",
    edstr_overwrite = TRUE
  )

  data <- data.frame(
    doc_id = as.character(1:2),
    texte = c(
      '<p class="t">cancer du sein</p>',
      '<p class="t">bilan normal</p>'
    ),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    edstr_extract(
      data = data,
      concepts = c(sein = "cancer du"),
      token = c(1, 2)
    )
  )

  expect_equal(nrow(result$data$extract), 1)
  expect_equal(result$data$extract$doc_id, "1")
})
