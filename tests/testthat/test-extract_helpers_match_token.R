new_match_token_inputs <- \() {
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
  inputs <- new_match_token_inputs()

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

test_that("match_token: intersect errors when no doc matches every concept", {
  inputs <- new_match_token_inputs()

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
  inputs <- new_match_token_inputs()

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

test_that("match_token: rows are concept-major in declaration order", {
  data <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    texte = c(
      "tumeur confirmee par biopsie",
      "tumeur au poumon biopsie prevue"
    ),
    stringsAsFactors = FALSE
  )

  concepts_list <- edstr:::.extract_parse_concepts(
    concepts = c(tumeur = "tumeur", biopsie = "biopsie"),
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

  expect_equal(
    result$data_match_init$concept,
    c("tumeur", "tumeur", "biopsie", "biopsie")
  )
  expect_equal(result$data_match_init$doc_id, c("1", "2", "1", "2"))
})

new_ngram_inputs <- \(token, concept) {
  data <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    texte = c("cancer du sein", "bilan normal"),
    stringsAsFactors = FALSE
  )

  list(
    data = data,
    concepts_list = edstr:::.extract_parse_concepts(
      concepts = concept,
      collapse = FALSE,
      intersect = FALSE,
      starts_with_only = TRUE
    ),
    data_token = edstr:::.extract_tokenize(
      data_token = data,
      text_input = "texte",
      token = token
    )
  )
}

test_that("match_token: n-gram tables are paired by size, not by position", {
  token <- c(n2 = 2, n1 = 1)
  inputs <- new_ngram_inputs(token, c(bi = "cancer du"))

  result <- edstr:::.extract_match_token(
    data = inputs$data,
    data_token = inputs$data_token,
    token = token,
    text_input = "texte",
    concepts_list = inputs$concepts_list,
    id = "doc_id",
    group = "id_group",
    intersect = FALSE
  )

  expect_equal(result$data_match_init$texte, "cancer du")
  expect_equal(result$data_match_init$token, "n2")
})

test_that("match_token: a non-contiguous token vector is indexed by size", {
  token <- c(n1 = 1, n3 = 3)
  inputs <- new_ngram_inputs(token, c(tri = "cancer du sein"))

  result <- edstr:::.extract_match_token(
    data = inputs$data,
    data_token = inputs$data_token,
    token = token,
    text_input = "texte",
    concepts_list = inputs$concepts_list,
    id = "doc_id",
    group = "id_group",
    intersect = FALSE
  )

  expect_equal(result$data_match_init$texte, "cancer du sein")
  expect_equal(result$data_match_init$token, "n3")
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
