test_that("parse_concepts: single unnamed concept gets default name", {
  result <- edstr:::.extract_parse_concepts(
    concepts = "diabet",
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_equal(result$keys, "concepts")
  expect_equal(result$root, "concepts")
})

test_that("parse_concepts: sub-concept names in a character vector are normalised", {
  result <- edstr:::.extract_parse_concepts(
    concepts = list(cancer = c("Sein Droit" = "sein"), diab = "diabet"),
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_equal(result$regex_df$concept_name, c("seindroit", "diab"))
  expect_false(any(str_detect(result$regex_df$concept_name, "[^a-z0-9_]")))
})

test_that("parse_concepts: the default name is the same on both sentinel paths", {
  unnamed <- edstr:::.extract_parse_concepts(
    concepts = "diabet",
    collapse = FALSE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  collapsed <- edstr:::.extract_parse_concepts(
    concepts = c(diabete = "diabet", cancer = "cancer"),
    collapse = TRUE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_equal(unnamed$keys, collapsed$keys)
  expect_false(any(str_detect(collapsed$keys, "[^a-z0-9]")))
  expect_false(any(collapsed$keys %in% c("n", "concept", "extract", "id_group")))
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

test_that("parse_concepts: collapse OR-combines a nested character list", {
  result <- edstr:::.extract_parse_concepts(
    concepts = list(
      cancer = c(sein = "sein", poumon = "poumon"),
      diab = c(t1 = "diabet", t2 = "insulin")
    ),
    collapse = TRUE,
    intersect = FALSE,
    starts_with_only = TRUE
  )

  expect_setequal(result$keys, c("cancer", "diab"))
  expect_setequal(
    result$regex_df$regex,
    c("^(sein|poumon)\\S*$", "^(diabet|insulin)\\S*$")
  )
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

test_that("parse_concepts: intersect with a single nested root errors", {
  expect_error(
    edstr:::.extract_parse_concepts(
      concepts = list(cancer = list(sein = "sein", poumon = "poumon")),
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
  expect_equal(result$regex_df$concept_name, c("sein", "poumon"))
  expect_null(names(result$regex_df$regex))
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

test_that("parse_concepts: names colliding after normalisation error", {
  expect_error(
    edstr:::.extract_parse_concepts(
      concepts = c("Diab-2" = "diabet", "diab_2" = "insulin"),
      collapse = FALSE,
      intersect = FALSE,
      starts_with_only = TRUE
    ),
    "collide after normalisation"
  )
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
    group = NULL,
    concept_keys = character()
  )

  expect_true("id_group" %in% names(result$data))
  expect_equal(result$group, "id_group")
  expect_equal(result$data$id_group, 1:3)
})

test_that("check_ids: a column the output builds for itself is reserved", {
  df <- data.frame(
    doc_id = c("a", "b", "c"),
    texte = c("foo", "bar", "baz")
  )

  check_with <- function(extra, ...) {
    data <- df
    if (!is.null(extra)) {
      data[[extra]] <- 1:3
    }

    edstr:::.extract_check_ids(
      data = data,
      sample = NULL,
      text_input = "texte",
      id = "doc_id",
      ...
    )
  }

  for (col in c("n", "concept", "extract", "id_group", "diag")) {
    expect_error(
      check_with(col, group = NULL, concept_keys = "diag"),
      "builds for itself",
      info = col
    )
  }

  expect_no_error(check_with("token", group = NULL, concept_keys = "diag"))
  expect_no_error(check_with(NULL, group = NULL, concept_keys = "diag"))

  # an explicit `group` leaves the user's own `id_group` column alone
  expect_no_error(
    check_with("id_group", group = "id_group", concept_keys = "diag")
  )
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
    group = NULL,
    concept_keys = character()
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
      group = NULL,
      concept_keys = character()
    ),
    "must be one of"
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

test_that("format_text: NA or empty source lands in no_source", {
  df <- data.frame(
    doc_id = c("1", "2", "3", "4"),
    id_group = 1:4,
    texte = c(
      '<p class="a">contenu normal</p>',
      NA_character_,
      "",
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

  expect_setequal(drops$no_source, c("2", "3"))
  expect_equal(drops$empty_text, "4")
  expect_length(drops$outside_p, 0)
})

test_that("format_text: an empty source alone still attaches a drop partition", {
  df <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    texte = c('<p class="a">un</p>', NA_character_)
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

  expect_equal(drops$no_source, "2")
  expect_length(drops$empty_text, 0)
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

test_that("print_drops: counts no_source in the total and on its own line", {
  out <- paste(
    cli::cli_fmt(edstr:::.extract_print_drops(
      list(no_source = "a", empty_text = "b", outside_p = "c")
    )),
    collapse = "\n"
  )

  expect_match(out, "3 documents produced no matchable text")
  expect_match(out, "empty or missing source")
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
  expect_equal(res$unmatched$n_no_concept, nrow(res$unmatched$no_concept))
  expect_equal(res$unmatched$n_no_concept, 2)

  all_unmatched <- c(
    res$unmatched$no_concept$doc_id,
    res$unmatched$empty_text$doc_id,
    res$unmatched$outside_p$doc_id
  )
  expect_setequal(all_unmatched, c("2", "3", "4", "5"))
  expect_equal(anyDuplicated(all_unmatched), 0L)
  expect_named(res$unmatched$no_concept, c("doc_id", "id_group"))
})

test_that("unmatched: no_source is kept out of the no_concept set", {
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
  format_drops <- list(no_source = "2", empty_text = "3", outside_p = "4")

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

  expect_equal(res$unmatched$no_source$doc_id, "2")
  expect_equal(res$unmatched$empty_text$doc_id, "3")
  expect_equal(res$unmatched$outside_p$doc_id, "4")
  expect_equal(res$unmatched$no_concept$doc_id, "5")

  all_unmatched <- c(
    res$unmatched$no_concept$doc_id,
    res$unmatched$no_source$doc_id,
    res$unmatched$empty_text$doc_id,
    res$unmatched$outside_p$doc_id
  )
  expect_setequal(all_unmatched, c("2", "3", "4", "5"))
  expect_equal(anyDuplicated(all_unmatched), 0L)
})

test_that("unmatched: no_source is populated regardless of unmatched_data", {
  data <- data.frame(
    doc_id = c("1", "2", "3"),
    id_group = 1:3,
    texte = "x"
  )

  res <- edstr:::.extract_unmatched(
    data = data,
    data_match = data.frame(
      doc_id = character(),
      concept = character(),
      texte = character()
    ),
    data_match_init = data.frame(doc_id = "1"),
    data_regex_match = data.frame(
      doc_id = character(),
      concept = character(),
      match = character()
    ),
    id = "doc_id",
    group = "id_group",
    text_input = "texte",
    unmatched_data = FALSE,
    format_drops = list(no_source = "2")
  )

  expect_equal(res$unmatched$no_source$doc_id, "2")
  expect_equal(nrow(res$unmatched$no_concept), 0)
  expect_equal(res$unmatched$n_no_concept, 1)
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
  expect_equal(res$unmatched$n_no_concept, 2)
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
    group = NULL,
    concept_keys = character()
  )

  withr::local_seed(42)
  result2 <- edstr:::.extract_check_ids(
    data = df,
    sample = 10,
    text_input = "texte",
    id = "doc_id",
    group = NULL,
    concept_keys = character()
  )

  expect_equal(result1$data$doc_id, result2$data$doc_id)
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
    group = "patient_id",
    concept_keys = character()
  )

  expect_equal(result$group, "patient_id")
  expect_true("patient_id" %in% names(result$data))
  expect_equal(result$data$patient_id, c("P1", "P1", "P2", "P2"))
})

test_that("format_text: the tokenised frame keeps only the key and text columns", {
  df <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    nom = c("Dupont", "Martin"),
    texte = '<p class="t">diabetique</p>',
    stringsAsFactors = FALSE
  )

  format_one <- function(...) {
    edstr:::.extract_format_text(
      data = df,
      text_input = "texte",
      id = "doc_id",
      group = "id_group",
      ...
    )
  }

  expect_equal(
    names(format_one(ano_hash = NULL, ano_hide = NULL)),
    c("doc_id", "id_group", "texte")
  )
  expect_false("nom" %in% names(format_one(ano_hash = "nom", ano_hide = NULL)))
  expect_false("nom" %in% names(format_one(ano_hash = NULL, ano_hide = "nom")))
})

test_that("format_text: anonymising a key column errors", {
  df <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    nom = c("Dupont", "Martin"),
    texte = '<p class="t">diabetique</p>',
    stringsAsFactors = FALSE
  )

  format_one <- function(...) {
    edstr:::.extract_format_text(
      data = df,
      text_input = "texte",
      id = "doc_id",
      group = "id_group",
      ...
    )
  }

  expect_error(
    format_one(ano_hash = "doc_id", ano_hide = NULL),
    "cannot target a key column"
  )
  expect_error(
    format_one(ano_hash = NULL, ano_hide = "doc_id"),
    "cannot target a key column"
  )
  expect_error(
    format_one(ano_hash = NULL, ano_hide = "id"),
    "cannot target a key column"
  )

  # `ano_hide` is matched case-insensitively, `ano_hash` is not
  expect_error(
    format_one(ano_hash = NULL, ano_hide = "DOC_ID"),
    "cannot target a key column"
  )
  expect_no_error(format_one(ano_hash = "DOC_ID", ano_hide = NULL))
})
