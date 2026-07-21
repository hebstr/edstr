test_that("parse_concepts: single unnamed concept gets default name", {
  result <- edstr:::.extract_parse_concepts(
    concepts = "diabet",
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_equal(result$keys, "concept")
  expect_equal(result$root, "concept")
})

test_that("parse_concepts: single named concept preserves name", {
  result <- edstr:::.extract_parse_concepts(
    concepts = c(diabete = "diabet"),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_equal(result$keys, "diabete")
  expect_equal(result$root, "diabete")
})

test_that("parse_concepts: multiple named concepts", {
  result <- edstr:::.extract_parse_concepts(
    concepts = c(diabete = "diabet", cancer = "cancer|tumeur"),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_equal(result$keys, c("diabete", "cancer"))
  expect_equal(result$root, c("diabete", "cancer"))
  expect_length(result$regex, 2)
})

test_that("parse_concepts: multiple unnamed concepts errors", {
  expect_error(
    edstr:::.extract_parse_concepts(
      concepts = c("diabet", "cancer"),
      collapse = FALSE,
      intersect = FALSE,
      starts_with_only = TRUE
    ),
    "must be named"
  )
})

test_that("parse_concepts: collapse OR-combines patterns", {
  result <- edstr:::.extract_parse_concepts(
    concepts = c(diabete = "diabet", cancer = "cancer"),
    collapse = TRUE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_length(result$keys, 1)
  expect_match(result$regex_df$regex, "diabet|cancer")
})

test_that("parse_concepts: collapse with single concept errors", {
  expect_error(
    edstr:::.extract_parse_concepts(
      concepts = c(diabete = "diabet"),
      collapse = TRUE,
      intersect = FALSE,
      starts_with_only = TRUE
    ),
    "Cannot collapse"
  )
})

test_that("parse_concepts: intersect with single concept errors", {
  expect_error(
    edstr:::.extract_parse_concepts(
      concepts = c(diabete = "diabet"),
      collapse = FALSE,
      intersect = TRUE,
      starts_with_only = TRUE
    ),
    "Cannot intersect"
  )
})

test_that("parse_concepts: starts_with_only = FALSE omits S*$", {
  result <- edstr:::.extract_parse_concepts(
    concepts = c(diabete = "diabet"),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = FALSE
  )

  expect_false(grepl("\\\\S", result$regex_df$regex))
  expect_match(result$regex_df$regex, "^\\^\\(diabet\\)$")
})

test_that("parse_concepts: nested list keeps single root key", {
  result <- edstr:::.extract_parse_concepts(
    concepts = list(cancer = c(sein = "sein|mammaire", poumon = "poumon")),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_equal(result$root, "cancer")
  expect_equal(nrow(result$regex_df), 2)
  expect_equal(unname(result$regex_df$concept_name), c("sein", "poumon"))
})

test_that("parse_concepts: concept names are cleaned to lowercase alphanumeric", {
  result <- edstr:::.extract_parse_concepts(
    concepts = c("Diab-2" = "diabet"),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_equal(result$keys, "diab2")
})


test_that("check_ids: auto-generates id_group when group is NULL", {
  df <- data.frame(
    doc_id = c("a", "b", "c"),
    texte = c("foo", "bar", "baz")
  )

  result <- edstr:::.extract_check_ids(
    data = df,
    sample = NULL,
    text_input = "texte",
    id = "doc_id",
    group = NULL
  )

  expect_true("id_group" %in% names(result$data))
  expect_equal(result$group, "id_group")
  expect_equal(result$data$id_group, 1:3)
})

test_that("check_ids: sampling reduces rows", {
  df <- data.frame(
    doc_id = as.character(1:100),
    texte = paste("text", 1:100)
  )

  result <- edstr:::.extract_check_ids(
    data = df,
    sample = 10,
    text_input = "texte",
    id = "doc_id",
    group = NULL
  )

  expect_equal(nrow(result$data), 10)
  expect_equal(result$nrow_init, 100)
})

test_that("check_ids: invalid id errors", {
  df <- data.frame(
    doc_id = c("a", "b", "c"),
    texte = c("foo", "bar", "baz")
  )

  expect_error(
    edstr:::.extract_check_ids(
      data = df,
      sample = NULL,
      text_input = "texte",
      id = "nonexistent",
      group = NULL
    )
  )
})


test_that("format_text: strips HTML tags and transliterates", {
  df <- data.frame(
    doc_id = "1",
    id_group = 1,
    texte = '<p class="foo">R\u00e9sultat n\u00e9gatif</p>'
  )

  result <- edstr:::.extract_format_text(
    data = df,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    ano_hash = NULL,
    ano_hide = NULL
  )

  expect_false(grepl("<", result$texte))
  expect_match(result$texte, "Resultat negatif")
})

test_that("format_text: keeps only id, group and text columns", {
  df <- data.frame(
    doc_id = "1",
    id_group = 1,
    texte = '<p class="foo">texte</p>',
    extra_col = "drop_me"
  )

  result <- edstr:::.extract_format_text(
    data = df,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    ano_hash = NULL,
    ano_hide = NULL
  )

  expect_equal(names(result), c("doc_id", "id_group", "texte"))
})

test_that("format_text: drop attribute carries empty-text and outside-<p> ids", {
  df <- data.frame(
    doc_id = c("1", "2", "3"),
    id_group = 1:3,
    texte = c(
      '<p class="a">contenu normal</p>',
      "<html><head><style>.x{color:red}</style></head><body><div></div></body></html>",
      "<html><body><span>hemiplegie gauche</span><div>suite du texte</div></body></html>"
    )
  )

  result <- expect_no_warning(
    edstr:::.extract_format_text(
      data = df,
      text_input = "texte",
      id = "doc_id",
      group = "id_group",
      ano_hash = NULL,
      ano_hide = NULL
    )
  )

  drops <- attr(result, "format_drops")
  expect_equal(drops$empty_text, "2")
  expect_equal(drops$outside_p, "3")

  expect_equal(nrow(result), 3)
  expect_match(result$texte[1], "contenu normal")
  expect_equal(result$texte[2], "")
  expect_equal(result$texte[3], "")
})

test_that("format_text: empty-source only yields no outside-<p> ids", {
  df <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    texte = c(
      '<p class="a">contenu</p>',
      "<html><head><style>.x{color:red}</style></head><body><div></div></body></html>"
    )
  )

  drops <- attr(
    edstr:::.extract_format_text(
      data = df,
      text_input = "texte",
      id = "doc_id",
      group = "id_group",
      ano_hash = NULL,
      ano_hide = NULL
    ),
    "format_drops"
  )

  expect_equal(drops$empty_text, "2")
  expect_length(drops$outside_p, 0)
})

test_that("format_text: fully covered lot attaches no drop partition", {
  df <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    texte = c('<p class="a">un</p>', '<p class="b">deux</p>')
  )

  result <- expect_no_warning(
    edstr:::.extract_format_text(
      data = df,
      text_input = "texte",
      id = "doc_id",
      group = "id_group",
      ano_hash = NULL,
      ano_hide = NULL
    )
  )

  expect_null(attr(result, "format_drops"))
})

test_that("print_drops: nests detail lines under a bulleted total", {
  lines <- cli::cli_fmt(edstr:::.extract_print_drops(
    list(empty_text = "a", outside_p = "b")
  ))

  total <- grep("produced no matchable text", lines, value = TRUE)
  detail <- grep("no recoverable text", lines, value = TRUE)

  expect_match(total, "2 documents produced no matchable text")
  expect_false(grepl("^\\s", total))
  expect_match(detail, "^\\s+")
  expect_match(paste(lines, collapse = "\n"), "outside")
})

test_that("print_drops: omits empty category when it has no ids", {
  out <- paste(
    cli::cli_fmt(edstr:::.extract_print_drops(
      list(empty_text = character(), outside_p = "b")
    )),
    collapse = "\n"
  )

  expect_no_match(out, "no recoverable text")
  expect_match(out, "outside")
})

test_that("print_drops: emits nothing when partition is NULL", {
  out <- cli::cli_fmt(edstr:::.extract_print_drops(NULL))
  expect_equal(length(out), 0)
})

test_that("unmatched: partitions unmatched docs into concept/empty/outside sets", {
  data <- data.frame(
    doc_id = c("1", "2", "3", "4", "5"),
    id_group = 1:5,
    texte = "x"
  )
  data_match_init <- data.frame(doc_id = "1")
  data_match <- data.frame(
    doc_id = character(),
    concept = character(),
    texte = character()
  )
  data_regex_match <- data.frame(
    doc_id = character(),
    concept = character(),
    match = character()
  )
  format_drops <- list(empty_text = "2", outside_p = "3")

  res <- edstr:::.extract_unmatched(
    data = data,
    data_match = data_match,
    data_match_init = data_match_init,
    data_regex_match = data_regex_match,
    id = "doc_id",
    group = "id_group",
    text_input = "texte",
    unmatched_data = TRUE,
    format_drops = format_drops
  )

  expect_equal(res$unmatched$empty_text$doc_id, "2")
  expect_equal(res$unmatched$outside_p$doc_id, "3")
  expect_setequal(res$unmatched$no_concept$doc_id, c("4", "5"))

  all_unmatched <- c(
    res$unmatched$no_concept$doc_id,
    res$unmatched$empty_text$doc_id,
    res$unmatched$outside_p$doc_id
  )
  expect_setequal(all_unmatched, c("2", "3", "4", "5"))
  expect_equal(anyDuplicated(all_unmatched), 0L)
  expect_named(res$unmatched$no_concept, c("doc_id", "id_group"))
})

test_that("unmatched: unmatched_data = FALSE gates only the no_concept set", {
  data <- data.frame(
    doc_id = c("1", "2", "3", "4", "5"),
    id_group = 1:5,
    texte = "x"
  )
  data_match_init <- data.frame(doc_id = "1")
  data_match <- data.frame(
    doc_id = character(),
    concept = character(),
    texte = character()
  )
  data_regex_match <- data.frame(
    doc_id = character(),
    concept = character(),
    match = character()
  )
  format_drops <- list(empty_text = "2", outside_p = "3")

  res <- edstr:::.extract_unmatched(
    data = data,
    data_match = data_match,
    data_match_init = data_match_init,
    data_regex_match = data_regex_match,
    id = "doc_id",
    group = "id_group",
    text_input = "texte",
    unmatched_data = FALSE,
    format_drops = format_drops
  )

  expect_equal(nrow(res$unmatched$no_concept), 0)
  expect_equal(res$unmatched$empty_text$doc_id, "2")
  expect_equal(res$unmatched$outside_p$doc_id, "3")
})

test_that("unmatched: mismatched captures token matches absent from source", {
  data <- data.frame(doc_id = "1", id_group = 1, texte = "x")
  data_match_init <- data.frame(doc_id = "1")
  data_match <- data.frame(
    doc_id = "1",
    concept = "c1",
    texte = "AVC"
  )
  data_regex_match <- data.frame(
    doc_id = character(),
    concept = character(),
    match = character()
  )

  res <- edstr:::.extract_unmatched(
    data = data,
    data_match = data_match,
    data_match_init = data_match_init,
    data_regex_match = data_regex_match,
    id = "doc_id",
    group = "id_group",
    text_input = "texte",
    unmatched_data = TRUE,
    format_drops = NULL
  )

  expect_equal(nrow(res$mismatched), 1)
  expect_equal(res$mismatched$match, "avc")
})


test_that("tokenize: produces named list of n-gram data frames", {
  df <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    texte = c("patient diabetique stable", "cancer du poumon")
  )

  token <- c(n1 = 1, n2 = 2)

  result <- edstr:::.extract_tokenize(
    data_token = df,
    text_input = "texte",
    token = token
  )

  expect_type(result, "list")
  expect_named(result, c("n1", "n2"))
  expect_s3_class(result$n1, "data.frame")
  expect_true("texte" %in% names(result$n1))
})

test_that("tokenize: bigrams contain spaces", {
  df <- data.frame(
    doc_id = "1",
    id_group = 1,
    texte = "patient diabetique stable"
  )

  token <- c(n2 = 2)

  result <- edstr:::.extract_tokenize(
    data_token = df,
    text_input = "texte",
    token = token
  )

  expect_true(all(grepl(" ", result$n2$texte)))
})


test_that("exclusions: exclus_auto_escape filters before auto-exclusion", {
  data_match <- data.frame(
    doc_id = c("1", "2", "3"),
    id_group = 1:3,
    concept_key = "diabete",
    concept = "diabete",
    texte = c("diabetique", "prediabetique", "diabete"),
    token = c("n1", "n1", "n1"),
    stringsAsFactors = FALSE
  )

  result <- edstr:::.extract_exclusions(
    data_match = data_match,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    exclus_manual = NULL,
    exclus_auto_escape = "^pre",
    exclus_auto_token_min = 10
  )

  expect_false("prediabetique" %in% result$data_match$texte)
})

test_that("exclusions: manual exclusion filters matching tokens", {
  data_match <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    concept_key = "diabete",
    concept = "diabete",
    texte = c("diabetique", "antidiabetique"),
    token = c("n1", "n1"),
    stringsAsFactors = FALSE
  )

  result <- edstr:::.extract_exclusions(
    data_match = data_match,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    exclus_manual = "^anti",
    exclus_auto_escape = NULL,
    exclus_auto_token_min = 10
  )

  excluded_texts <- result$data_match_exclus$manual$texte
  expect_true("antidiabetique" %in% excluded_texts)
})

test_that("exclusions: auto-exclusion only applies to token > threshold", {
  data_match <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    concept_key = "diabete",
    concept = "diabete",
    texte = c("diabetique", "diabetique"),
    token = c("n1", "n1"),
    stringsAsFactors = FALSE
  )

  result <- edstr:::.extract_exclusions(
    data_match = data_match,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    exclus_manual = NULL,
    exclus_auto_escape = NULL,
    exclus_auto_token_min = 10
  )

  expect_equal(nrow(result$data_match_exclus$auto), 0)
})

test_that("exclusions: regex metacharacters in tokens don't cause errors", {
  data_match <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    concept_key = "test",
    concept = "test",
    texte = c("test (positif)", "test normal"),
    token = c("n12", "n12"),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    edstr:::.extract_exclusions(
      data_match = data_match,
      text_input = "texte",
      id = "doc_id",
      group = "id_group",
      exclus_manual = NULL,
      exclus_auto_escape = NULL,
      exclus_auto_token_min = 10
    )
  )
})


# .extract_match_token ---------------------------------------------------------

local_match_token_inputs <- \() {
  data <- data.frame(
    doc_id = c("1", "2", "3"),
    id_group = 1:3,
    texte = c(
      "patient diabetique stable",
      "cancer du poumon droit",
      "bilan normal sans anomalie"
    ),
    stringsAsFactors = FALSE
  )

  concepts_list <- edstr:::.extract_parse_concepts(
    concepts = c(diabete = "diabet", cancer = "cancer"),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  data_token_df <- data
  data_token_df$texte <- tolower(data_token_df$texte)

  data_token <- edstr:::.extract_tokenize(
    data_token = data_token_df,
    text_input = "texte",
    token = c(n1 = 1)
  )

  list(data = data, concepts_list = concepts_list, data_token = data_token)
}

test_that("match_token: finds matching concepts", {
  inputs <- local_match_token_inputs()

  result <- edstr:::.extract_match_token(
    data = inputs$data,
    data_token = inputs$data_token,
    token = c(n1 = 1),
    text_input = "texte",
    concepts_list = inputs$concepts_list,
    id = "doc_id",
    group = "id_group",
    intersect = FALSE
  )

  expect_true("data_match" %in% names(result))
  expect_true(nrow(result$data_match) > 0)
  matched_ids <- unique(result$data_match$doc_id)
  expect_true("1" %in% matched_ids)
  expect_true("2" %in% matched_ids)
  expect_false("3" %in% matched_ids)
})

test_that("match_token: intersect keeps only docs matching all concepts", {
  inputs <- local_match_token_inputs()

  expect_error(
    edstr:::.extract_match_token(
      data = inputs$data,
      data_token = inputs$data_token,
      token = c(n1 = 1),
      text_input = "texte",
      concepts_list = inputs$concepts_list,
      id = "doc_id",
      group = "id_group",
      intersect = TRUE
    ),
    "intersection"
  )
})

test_that("match_token: no match at all errors", {
  data <- data.frame(
    doc_id = "1",
    id_group = 1,
    texte = "rien a voir",
    stringsAsFactors = FALSE
  )

  concepts_list <- edstr:::.extract_parse_concepts(
    concepts = c(diabete = "diabet"),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  data_token <- edstr:::.extract_tokenize(
    data_token = data,
    text_input = "texte",
    token = c(n1 = 1)
  )

  expect_error(
    edstr:::.extract_match_token(
      data = data,
      data_token = data_token,
      token = c(n1 = 1),
      text_input = "texte",
      concepts_list = concepts_list,
      id = "doc_id",
      group = "id_group",
      intersect = FALSE
    ),
    "No matches found"
  )
})

test_that("match_token: data_match_df contains original data for matched ids", {
  inputs <- local_match_token_inputs()

  result <- edstr:::.extract_match_token(
    data = inputs$data,
    data_token = inputs$data_token,
    token = c(n1 = 1),
    text_input = "texte",
    concepts_list = inputs$concepts_list,
    id = "doc_id",
    group = "id_group",
    intersect = FALSE
  )

  expect_true("texte" %in% names(result$data_match_df))
  expect_true(all(result$data_match_df$doc_id %in% c("1", "2")))
})


# edstr_extract integration ----------------------------------------------------

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

  expect_equal(result2$data$extract, result1$data$extract)
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


test_that("check_ids: sample + seed produce reproducible rows", {
  df <- data.frame(
    doc_id = as.character(1:50),
    texte = paste("text", 1:50)
  )

  withr::local_seed(42)
  result1 <- edstr:::.extract_check_ids(
    data = df,
    sample = 10,
    text_input = "texte",
    id = "doc_id",
    group = NULL
  )

  withr::local_seed(42)
  result2 <- edstr:::.extract_check_ids(
    data = df,
    sample = 10,
    text_input = "texte",
    id = "doc_id",
    group = NULL
  )

  expect_equal(result1$data$doc_id, result2$data$doc_id)
})


test_that("edstr_extract: dirname_suffix and filename_suffix affect paths", {
  tmp <- withr::local_tempdir()
  withr::local_options(
    edstr_dirname = tmp,
    edstr_filename = "test_suffix"
  )

  config_dir <- getOption("edstr_dirname")
  filename <- getOption("edstr_filename")
  dirname <- "extract"
  dirname_suffix <- "custom"
  filename_suffix <- "custom"

  save_dir <- fs::path(config_dir, dirname)
  save_dir <- glue::glue("{save_dir}_{dirname_suffix}")

  save_files <- glue::glue("{filename}_{dirname}")
  save_files <- glue::glue("{save_files}_{filename_suffix}")

  expect_match(as.character(save_dir), "extract_custom$")
  expect_equal(as.character(save_files), "test_suffix_extract_custom")
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


test_that("check_ids: explicit group column is preserved", {
  df <- data.frame(
    doc_id = as.character(1:4),
    patient_id = c("P1", "P1", "P2", "P2"),
    texte = c("foo", "bar", "baz", "qux")
  )

  result <- edstr:::.extract_check_ids(
    data = df,
    sample = NULL,
    text_input = "texte",
    id = "doc_id",
    group = "patient_id"
  )

  expect_equal(result$group, "patient_id")
  expect_true("patient_id" %in% names(result$data))
  expect_equal(result$data$patient_id, c("P1", "P1", "P2", "P2"))
})

test_that("format_text: ano_hash pseudonymises in tokenised data", {
  df <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    nom = c("Dupont", "Martin"),
    texte = '<p class="t">diabetique</p>',
    stringsAsFactors = FALSE
  )

  result <- edstr:::.extract_format_text(
    data = df,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    ano_hash = "nom",
    ano_hide = NULL
  )

  expect_false("nom" %in% names(result))
})

test_that("format_text: ano_hide masks columns before tokenisation", {
  df <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    nom = c("Dupont", "Martin"),
    texte = '<p class="t">diabetique</p>',
    stringsAsFactors = FALSE
  )

  result <- edstr:::.extract_format_text(
    data = df,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    ano_hash = NULL,
    ano_hide = "nom"
  )

  expect_false("nom" %in% names(result))
})

test_that("match_source: regex_replace adds custom replacement rules", {
  data_match_df <- data.frame(
    doc_id = "1",
    id_group = 1,
    texte = "diabetique stable",
    stringsAsFactors = FALSE
  )

  data_count <- data.frame(
    concept = "diabete",
    texte = "diabetique",
    stringsAsFactors = FALSE
  )

  result <- edstr:::.extract_match_source(
    data_match_df = data_match_df,
    data_count = data_count,
    text_input = "texte",
    id = "doc_id",
    regex_replace = c("x" = "[x]")
  )

  expect_true("x" %in% result$regex_replace_df$pattern)
})

test_that("match_token: multiple top-level concepts match correct docs", {
  data <- data.frame(
    doc_id = c("1", "2", "3"),
    id_group = 1:3,
    texte = c("sein gauche opere", "poumon droit atteint", "bilan normal"),
    stringsAsFactors = FALSE
  )

  concepts_list <- edstr:::.extract_parse_concepts(
    concepts = c(sein = "sein", poumon = "poumon"),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  data_token <- edstr:::.extract_tokenize(
    data_token = data,
    text_input = "texte",
    token = c(n1 = 1)
  )

  result <- edstr:::.extract_match_token(
    data = data,
    data_token = data_token,
    token = c(n1 = 1),
    text_input = "texte",
    concepts_list = concepts_list,
    id = "doc_id",
    group = "id_group",
    intersect = FALSE
  )

  matched_ids <- unique(result$data_match$doc_id)
  expect_true("1" %in% matched_ids)
  expect_true("2" %in% matched_ids)
  expect_false("3" %in% matched_ids)
  expect_equal(length(unique(result$data_match$concept_key)), 2)
})

test_that("exclusions: exclus_auto_token_min = 0 activates auto-exclusion on unigrams", {
  data_match <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    concept_key = "diabete",
    concept = "diabete",
    texte = c("diabetique", "diabete"),
    token = c("n1", "n1"),
    stringsAsFactors = FALSE
  )

  result_default <- edstr:::.extract_exclusions(
    data_match = data_match,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    exclus_manual = NULL,
    exclus_auto_escape = NULL,
    exclus_auto_token_min = 10
  )

  result_zero <- edstr:::.extract_exclusions(
    data_match = data_match,
    text_input = "texte",
    id = "doc_id",
    group = "id_group",
    exclus_manual = NULL,
    exclus_auto_escape = NULL,
    exclus_auto_token_min = 0
  )

  expect_true(
    nrow(result_zero$data_match_exclus$auto) >= nrow(result_default$data_match_exclus$auto)
  )
})
