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

test_that("exclusions: exclus_auto_token_min gates auto-exclusion by n-gram size", {
  data_match <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    concept_key = "diabete",
    concept = "diabete",
    texte = c("diabete", "diabete type 2"),
    token = c("n1", "n3"),
    stringsAsFactors = FALSE
  )

  exclusions <- \(min) {
    edstr:::.extract_exclusions(
      data_match = data_match,
      text_input = "texte",
      id = "doc_id",
      group = "id_group",
      exclus_manual = NULL,
      exclus_auto_escape = NULL,
      exclus_auto_token_min = min
    )
  }

  above <- exclusions(10)
  below <- exclusions(0)

  expect_equal(nrow(above$data_match_exclus$auto), 0)
  expect_setequal(
    above$data_match_final$keep$texte,
    c("diabete", "diabete type 2")
  )

  expect_equal(below$data_match_exclus$auto$texte, "diabete type 2")
  expect_equal(below$data_match_exclus$auto$start, "diabete")
  expect_equal(below$data_match_final$keep$texte, "diabete")
  expect_equal(below$data_match_final$drop$texte, "diabete type 2")
})

test_that("exclusions: an empty auto result keeps its schema on either path", {
  data_match <- data.frame(
    doc_id = c("1", "2"),
    id_group = 1:2,
    concept_key = "diabete",
    concept = "diabete",
    texte = c("alpha", "beta"),
    token = c("n1", "n1"),
    stringsAsFactors = FALSE
  )

  exclusions <- \(min) {
    edstr:::.extract_exclusions(
      data_match = data_match,
      text_input = "texte",
      id = "doc_id",
      group = "id_group",
      exclus_manual = NULL,
      exclus_auto_escape = NULL,
      exclus_auto_token_min = min
    )$data_match_exclus$auto
  }

  # no n-gram clears the threshold, so the scan is skipped altogether
  skipped <- exclusions(10)
  # every n-gram clears it, but neither token anchors the other, so it finds nothing
  scanned <- exclusions(0)

  expect_equal(nrow(skipped), 0)
  expect_equal(nrow(scanned), 0)
  expect_identical(skipped, scanned)
})
